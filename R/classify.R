# =============================================================================
# KLASSINDELNING FÖR TEMATISKA KARTOR
# =============================================================================
#
# Funktioner för att klassindela kontinuerliga värden till diskreta klasser
# för användning i tematiska kartor enligt Göteborgs Stads designsystem.
#
# Använder classInt-paketet för robust och beprövad klassindelning.
#
# =============================================================================


# 1. SKAPA KLASSGRÄNSER =======================================================

#' Skapa klassgränser för kartdata
#'
#' Beräknar klassgränser (breaks) med olika metoder. Använder classInt-paketet
#' för robust och beprövad klassindelning.
#'
#' Metoder:
#' - quantile: Lika många observationer per klass (bra för skeva fördelningar)
#' - equal: Lika stora intervall (enkelt, intuitivt)
#' - jenks: Natural breaks som minimerar variation inom klasser
#' - fisher: Fisher-Jenks algoritm (bättre än jenks, snabbare)
#' - pretty: Snygga avrundade tal (bra för presentation)
#' - sd: Baserat på standardavvikelse från medelvärde
#' - kmeans: K-means clustering
#' - hclust: Hierarkisk clustering
#' - dpih: Histogram bin width (automatiskt antal klasser)
#' - headtails: För heavy-tailed distributioner
#' - maximum: Maximum breaks method
#' - box: Baserat på boxplot (6 klasser)
#' - manual: Egna gränsvärden
#'
#' @param values Numerisk vektor att klassindela
#' @param method Klassindelningsmetod (se ovan)
#' @param n_classes Antal klasser (2-10 rekommenderas, ignoreras för dpih/headtails/box)
#' @param breaks Manuella gränsvärden (endast för method = "manual")
#' @param ... Extra argument som skickas vidare till classInt::classIntervals()
#'
#' @return Numerisk vektor med klassgränser (n_classes + 1 värden)
#'
#' @examples
#' # Skapa 5 klasser med quantile-metoden
#' breaks <- create_breaks(kommuner$befolkning, "quantile", 5)
#'
#' # Fisher-Jenks (rekommenderas över jenks)
#' breaks <- create_breaks(kommuner$befolkning, "fisher", 5)
#'
#' # Manuella gränsvärden
#' breaks <- create_breaks(
#'   kommuner$inkomst,
#'   method = "manual",
#'   breaks = c(0, 200000, 300000, 400000, Inf)
#' )
#'
#' # Använd i create_map()
#' create_map(
#'   map_data, stat_data, "befolkning",
#'   breaks = create_breaks(stat_data$befolkning, "fisher", 6)
#' )
create_breaks <- function(values,
                          method = "quantile",
                          n_classes = 5,
                          breaks = NULL,
                          ...) {
  
  # VARFÖR classInt?: Beprövat paket som används brett i GIS-världen,
  # väl testat och hanterar edge cases bättre än hemmasnickrad kod
  
  # 1. VALIDERING ----
  
  if (!is.numeric(values)) {
    stop(
      "values måste vara numerisk, inte ", class(values)[1], "\n",
      "Kontrollera att du valt rätt kolumn från din statistikfil."
    )
  }
  
  # Ta bort NA
  values_clean <- values[!is.na(values)]
  n_na <- sum(is.na(values))
  
  if (length(values_clean) == 0) {
    stop("Inga giltiga värden att klassindela (alla är NA)")
  }
  
  if (length(unique(values_clean)) == 1) {
    stop(
      "Alla värden är identiska (", unique(values_clean), ")\n",
      "Kan inte skapa klassindelning."
    )
  }
  
  if (n_na > 0) {
    message("OBS: ", n_na, " NA-värden kommer inte klassindelas")
  }
  
  # Validera metod
  valid_methods <- c("quantile", "equal", "jenks", "fisher", "pretty", "sd", 
                     "kmeans", "hclust", "dpih", "headtails", "maximum", 
                     "box", "manual")
  if (!method %in% valid_methods) {
    stop(
      "Okänd metod: '", method, "'\n",
      "Välj mellan: ", paste(valid_methods, collapse = ", ")
    )
  }
  
  # Validera n_classes (ej för manual, dpih, headtails, box)
  auto_n_methods <- c("dpih", "headtails", "box")
  if (!method %in% c("manual", auto_n_methods)) {
    if (!is.numeric(n_classes) || n_classes < 2 || n_classes > 15) {
      stop("n_classes måste vara mellan 2 och 15")
    }
    
    n_unique <- length(unique(values_clean))
    if (n_classes > n_unique) {
      warning(
        "n_classes (", n_classes, ") > antal unika värden (", n_unique, ")\n",
        "Reducerar till ", n_unique, " klasser."
      )
      n_classes <- n_unique
    }
  }
  
  # 2. BERÄKNA BREAKS ----
  
  if (method == "manual") {
    # MANUAL: Användardefinierade breaks (hanteras utan classInt)
    if (is.null(breaks)) {
      stop(
        "För method = 'manual' måste du ange breaks.\n",
        "Exempel: breaks = c(0, 100, 500, 1000, 5000)"
      )
    }
    
    if (!is.numeric(breaks)) {
      stop("breaks måste vara en numerisk vektor")
    }
    
    if (length(breaks) < 3) {
      stop("breaks måste ha minst 3 värden (för 2 klasser)")
    }
    
    if (any(diff(breaks) <= 0)) {
      stop("breaks måste vara i stigande ordning utan dubletter")
    }
    
    breaks_result <- breaks
    n_classes_actual <- length(breaks_result) - 1
    
  } else {
    # ALLA ANDRA METODER: Använd classInt
    
    # Kontrollera att classInt finns
    if (!requireNamespace("classInt", quietly = TRUE)) {
      stop(
        "Paketet 'classInt' krävs för klassindelning.\n",
        "Installera med: install.packages('classInt')"
      )
    }
    
    # Översätt våra metodnamn till classInt style-namn
    # VARFÖR översättning?: Vi vill ha intuitivare namn för svenska användare
    style_map <- c(
      "quantile" = "quantile",
      "equal" = "equal",
      "jenks" = "jenks",
      "fisher" = "fisher",
      "pretty" = "pretty",
      "sd" = "sd",
      "kmeans" = "kmeans",
      "hclust" = "hclust",
      "dpih" = "dpih",
      "headtails" = "headtails",
      "maximum" = "maximum",
      "box" = "box"
    )
    
    classint_style <- style_map[method]
    
    # Anropa classInt
    # VARFÖR try-catch?: Vissa metoder kan misslyckas med vissa dataset
    result <- tryCatch({
      if (method %in% auto_n_methods) {
        # Dessa metoder bestämmer n själva
        classInt::classIntervals(values_clean, style = classint_style, ...)
      } else {
        # Dessa tar n som parameter
        classInt::classIntervals(values_clean, n = n_classes, style = classint_style, ...)
      }
    }, error = function(e) {
      stop(
        "Kunde inte beräkna klassgränser med metod '", method, "':\n",
        e$message, "\n",
        "Prova en annan metod eller färre klasser."
      )
    })
    
    # Extrahera breaks från classInt-objektet
    breaks_result <- result$brks
    n_classes_actual <- length(breaks_result) - 1
  }
  
  # 3. JUSTERA OCH VALIDERA ----
  
  # Säkerställ att alla värden täcks
  # VARFÖR?: classInt kan ibland ge breaks som inte täcker min/max helt
  if (min(values_clean) < min(breaks_result)) {
    breaks_result[1] <- min(values_clean)
  }
  if (max(values_clean) > max(breaks_result)) {
    breaks_result[length(breaks_result)] <- max(values_clean)
  }
  
  # Ta bort eventuella dubletter
  breaks_result <- unique(breaks_result)
  
  if (length(breaks_result) < 2) {
    stop(
      "Kunde inte skapa klassindelning. ",
      "Troligen för många lika värden i datan."
    )
  }
  
  # Uppdatera actual classes efter eventuell unique()
  n_classes_actual <- length(breaks_result) - 1
  
  # Ge feedback
  message(
    "✓ Skapade ", n_classes_actual, " klasser med metod: ", method
  )
  
  return(breaks_result)
}


# 2. SKAPA LABELS ============================================================

#' Skapa labels för klassindelning
#'
#' Formaterar klassgränser till läsbara labels för legend.
#' Kan skapa range-labels ("0-100"), ruler-labels ("100", "200")
#' eller anpassade labels.
#'
#' VARFÖR ruler bara övre gränser?: 
#' - Matchar antal klasser (4 klasser → 4 labels)
#' - Funkar direkt med ggplot2
#' - Tydligt visar "upp till X" för varje klass
#'
#' @param breaks Vektor med klassgränser
#' @param style Format på labels:
#'   - "range": Intervall, t.ex. "0-100", "100-200"
#'   - "ruler": Övre gränser, t.ex. "100", "200", "300"
#'   - "custom": Egna labels
#' @param unit Enhet att lägga till (t.ex. "%", "kr", "inv/km²")
#' @param decimals Antal decimaler (default: 1)
#' @param custom_labels Vektor med egna labels (endast för style = "custom")
#'
#' @return Character vector med labels
#'
#' @examples
#' breaks <- c(0, 100, 500, 1000, 5000)
#'
#' # Range-labels
#' create_labels(breaks, "range")
#' # "0-100", "100-500", "500-1 000", "1 000-5 000"
#'
#' # Ruler-labels (4 klasser → 4 labels)
#' create_labels(breaks, "ruler")
#' # "100", "500", "1 000", "5 000"
#'
#' # Med enhet och decimaler
#' create_labels(breaks, "ruler", unit = "kr", decimals = 0)
#' # "100 kr", "500 kr", "1 000 kr", "5 000 kr"
#'
#' # Custom labels
#' create_labels(breaks, "custom", 
#'               custom_labels = c("Låg", "Medel", "Hög", "Mycket hög"))
create_labels <- function(breaks,
                          style = "range",
                          unit = NULL,
                          decimals = 1,
                          custom_labels = NULL) {
  
  # VARFÖR egen funktion för labels?: ggplot2 har inte bra stöd för
  # tusentalsavskiljare och svenska decimalkomma
  
  # 1. VALIDERING ----
  
  if (!is.numeric(breaks)) {
    stop("breaks måste vara en numerisk vektor")
  }
  
  if (length(breaks) < 2) {
    stop("breaks måste ha minst 2 värden")
  }
  
  valid_styles <- c("range", "ruler", "custom")
  if (!style %in% valid_styles) {
    stop(
      "Okänd style: '", style, "'\n",
      "Välj mellan: ", paste(valid_styles, collapse = ", ")
    )
  }
  
  if (style == "custom" && is.null(custom_labels)) {
    stop("För style = 'custom' måste du ange custom_labels")
  }
  
  n_intervals <- length(breaks) - 1
  
  if (style == "custom") {
    if (length(custom_labels) != n_intervals) {
      stop(
        "Antal custom_labels (", length(custom_labels), ") måste matcha ",
        "antal intervall (", n_intervals, ")"
      )
    }
    return(as.character(custom_labels))
  }
  
  # 2. FORMATERA VÄRDEN MED TUSENTALSAVSKILJARE ----
  
  # VARFÖR egen formatering?: R's format() ger inte svensk standard
  # Vi vill ha: 1 234,5 (inte 1234.5 eller 1,234.5)
  
  format_value <- function(x, dec = decimals) {
    # Avrunda
    x_rounded <- round(x, dec)
    
    # Dela upp i heltal och decimaler
    is_negative <- x_rounded < 0
    x_abs <- abs(x_rounded)
    
    if (dec == 0) {
      # Inga decimaler
      int_part <- as.character(floor(x_abs))
      dec_part <- ""
    } else {
      # Med decimaler
      int_part <- as.character(floor(x_abs))
      dec_part <- sprintf(paste0("%0", dec, "d"), 
                          round((x_abs - floor(x_abs)) * 10^dec))
    }
    
    # Lägg till tusentalsavskiljare (mellanslag)
    # VARFÖR mellanslag?: Internationell standard, fungerar både med punkt och komma
    if (nchar(int_part) > 3) {
      # Dela upp i grupper om 3 från höger
      chars <- strsplit(int_part, "")[[1]]
      n_chars <- length(chars)
      
      # Bygg upp från VÄNSTER med mellanslag från höger
      formatted <- ""
      for (i in 1:n_chars) {
        # Lägg till siffran
        formatted <- paste0(formatted, chars[i])
        # Lägg till mellanslag om vi är på en tredje-position (räknat från höger)
        # OCH det inte är sista siffran
        remaining <- n_chars - i
        if (remaining > 0 && remaining %% 3 == 0) {
          formatted <- paste0(formatted, " ")
        }
      }
      int_part <- formatted
    }
    
    # Sätt ihop
    if (dec == 0) {
      result <- int_part
    } else {
      # Svensk standard: komma som decimaltecken
      result <- paste0(int_part, ",", dec_part)
    }
    
    # Lägg till minustecken om negativt
    if (is_negative) {
      result <- paste0("-", result)
    }
    
    return(result)
  }
  
  # 3. SKAPA LABELS BASERAT PÅ STYLE ----
  
  if (style == "range") {
    # RANGE: Visa hela intervallet för varje klass
    # Exempel: "0-100", "100-500", "500-1000"
    
    labels <- character(n_intervals)
    
    for (i in 1:n_intervals) {
      lower <- format_value(breaks[i])
      upper <- format_value(breaks[i + 1])
      labels[i] <- paste0(lower, "-", upper)
    }
    
  } else if (style == "ruler") {
    # RULER: Visa endast övre gräns för varje klass
    # Exempel: "100", "500", "1000"
    # VARFÖR bara övre?: Ger n labels för n klasser (perfekt för ggplot2)
    
    labels <- character(n_intervals)
    
    for (i in 1:n_intervals) {
      labels[i] <- format_value(breaks[i + 1])
    }
  }
  
  # STEG 4: Lägg till enhet om angiven (men inte för custom labels) ----
  
  if (!is.null(unit) && style != "custom") {
    labels <- paste(labels, unit)
  }
  
  return(labels)
}


# 3. APPLICERA KLASSINDELNING =================================================

#' Applicera klassindelning på data
#'
#' Klassindelar numeriska värden baserat på breaks och returnerar factor.
#' Användbart för att förbereda data innan visualisering.
#'
#' @param values Numeriska värden att klassindela
#' @param breaks Klassgränser (från create_breaks())
#' @param labels Labels för klasserna (från create_labels())
#'
#' @return Factor med klassindelade värden
#'
#' @examples
#' # Skapa klassindelning
#' breaks <- create_breaks(befolkning$täthet, "quantile", 5)
#' labels <- create_labels(breaks, "range", unit = "pers/km²")
#'
#' # Applicera på data
#' befolkning$klass <- apply_classification(
#'   befolkning$täthet,
#'   breaks,
#'   labels
#' )
#'
#' # Använd i visualisering
#' ggplot(befolkning_sf, aes(fill = klass)) +
#'   geom_sf() +
#'   scale_fill_gbg_categorical("palette_5")
apply_classification <- function(values, breaks, labels) {
  
  if (!is.numeric(values)) {
    stop("values måste vara numerisk")
  }
  
  if (!is.numeric(breaks)) {
    stop("breaks måste vara numerisk vektor")
  }
  
  n_intervals <- length(breaks) - 1
  
  if (length(labels) != n_intervals) {
    stop(
      "Antal labels (", length(labels), ") måste matcha ",
      "antal intervall (", n_intervals, ")"
    )
  }
  
  # Klassindela
  # VARFÖR include.lowest och right = FALSE?: Säkerställer att alla värden
  # klassificeras korrekt inklusive min och max
  classes <- cut(
    values,
    breaks = breaks,
    labels = labels,
    include.lowest = TRUE,
    right = FALSE
  )
  
  return(classes)
}


# 4. HJÄLPFUNKTIONER ==========================================================

#' Sammanfatta värden för klassindelning
#'
#' Visar statistik och föreslår lämpliga klassgränser.
#' Hjälper dig välja metod och antal klasser.
#'
#' @param values Numerisk vektor att analysera
#' @param n_classes Antal klasser att föreslå (default: 5)
#'
#' @return Inget (skriver ut till konsol)
#'
#' @examples
#' # Enkel användning
#' summarize_for_breaks(befolkning$täthet)
#'
#' # Föreslå 7 klasser
#' summarize_for_breaks(befolkning$inkomst, n_classes = 7)
summarize_for_breaks <- function(values, n_classes = 5) {
  
  if (!is.numeric(values)) {
    stop("values måste vara numerisk")
  }
  
  values_clean <- values[!is.na(values)]
  n_na <- sum(is.na(values))
  
  if (length(values_clean) == 0) {
    stop("Inga giltiga värden (alla är NA)")
  }
  
  # Beräkna statistik
  cat("\n=== VÄRDESAMMANFATTNING ===\n\n")
  
  cat("Observationer:\n")
  cat("  Totalt:   ", length(values), "\n")
  cat("  Giltiga:  ", length(values_clean), "\n")
  if (n_na > 0) {
    cat("  NA:       ", n_na, "\n")
  }
  cat("  Unika:    ", length(unique(values_clean)), "\n\n")
  
  cat("Centralmått:\n")
  cat("  Medel:    ", round(mean(values_clean), 2), "\n")
  cat("  Median:   ", round(median(values_clean), 2), "\n")
  cat("  Std.avv:  ", round(sd(values_clean), 2), "\n\n")
  
  cat("Spridning:\n")
  cat("  Min:      ", round(min(values_clean), 2), "\n")
  cat("  Q1:       ", round(quantile(values_clean, 0.25), 2), "\n")
  cat("  Q3:       ", round(quantile(values_clean, 0.75), 2), "\n")
  cat("  Max:      ", round(max(values_clean), 2), "\n")
  cat("  Range:    ", round(max(values_clean) - min(values_clean), 2), "\n\n")
  
  # Föreslå breaks med olika metoder
  cat("=== FÖRSLAG PÅ BREAKS (", n_classes, " klasser) ===\n\n", sep = "")
  
  methods <- c("equal", "quantile", "pretty", "fisher", "jenks")
  
  for (method in methods) {
    tryCatch({
      brks <- create_breaks(values_clean, method, n_classes)
      cat(sprintf("%-12s", paste0(method, ":")),
          paste(round(brks, 1), collapse = ", "), "\n")
    }, error = function(e) {
      cat(sprintf("%-12s", paste0(method, ":")), "Kunde inte beräknas\n")
    })
  }
  
  cat("\n")
  
  invisible(NULL)
}


#' Jämför klassindelningsmetoder visuellt
#'
#' Skapar histogram som visar hur olika metoder delar in värdena.
#' Hjälper dig välja lämplig metod och antal klasser.
#'
#' @param values Numerisk vektor att klassindela
#' @param methods Metoder att jämföra (default: c("equal", "quantile", "fisher"))
#' @param n_classes Antal klasser (default: 5)
#'
#' @return ggplot2-objekt
#'
#' @examples
#' # Jämför tre metoder
#' compare_methods(befolkning$täthet)
#'
#' # Jämför med 7 klasser
#' compare_methods(befolkning$inkomst, n_classes = 7)
#'
#' # Testa bara quantile med olika antal klasser
#' compare_methods(
#'   befolkning$täthet,
#'   methods = "quantile",
#'   n_classes = c(4, 5, 6, 7)
#' )
compare_methods <- function(values,
                            methods = c("equal", "quantile", "fisher"),
                            n_classes = 5) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Paketet 'ggplot2' krävs. Installera med: install.packages('ggplot2')")
  }
  
  if (!is.numeric(values)) {
    stop("values måste vara numerisk")
  }
  
  values_clean <- values[!is.na(values)]
  
  if (length(values_clean) == 0) {
    stop("Inga giltiga värden")
  }
  
  # Skapa kombinationer av metoder och n_classes
  if (length(methods) == 1 && length(n_classes) > 1) {
    combinations <- expand.grid(
      method = methods,
      n = n_classes,
      stringsAsFactors = FALSE
    )
  } else if (length(methods) > 1 && length(n_classes) == 1) {
    combinations <- expand.grid(
      method = methods,
      n = n_classes,
      stringsAsFactors = FALSE
    )
  } else {
    combinations <- data.frame(
      method = methods[1],
      n = n_classes[1],
      stringsAsFactors = FALSE
    )
  }
  
  # Skapa klassindelningar
  results_list <- list()
  
  for (i in 1:nrow(combinations)) {
    method <- combinations$method[i]
    n <- combinations$n[i]
    
    tryCatch({
      brks <- create_breaks(values_clean, method, n)
      lbls <- create_labels(brks, "range", decimals = 0)
      
      classes <- apply_classification(values_clean, brks, lbls)
      
      df <- data.frame(
        value = values_clean,
        class = classes,
        panel_label = paste0(method, " (n=", length(brks) - 1, ")")
      )
      
      results_list[[i]] <- df
      
    }, error = function(e) {
      warning("Kunde inte beräkna ", method, " med n=", n)
      NULL
    })
  }
  
  # Kombinera resultat
  if (length(results_list) == 0) {
    stop("Kunde inte skapa någon klassindelning")
  }
  
  results_df <- do.call(rbind, results_list)
  results_df$panel_label <- factor(
    results_df$panel_label,
    levels = unique(results_df$panel_label)
  )
  
  # Skapa plot
  p <- ggplot2::ggplot(results_df, ggplot2::aes(x = value, fill = class)) +
    ggplot2::geom_histogram(bins = 30, color = "white", linewidth = 0.3) +
    ggplot2::facet_wrap(~panel_label, ncol = 1, scales = "free_y") +
    ggplot2::labs(
      title = "Jämförelse av klassindelningsmetoder",
      x = "Värde",
      y = "Antal observationer",
      fill = "Klass"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "right",
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      strip.text = ggplot2::element_text(face = "bold", size = 11)
    )
  
  # Lägg till färger om gbg_palette finns
  if (exists("gbg_palette")) {
    n_colors <- length(unique(results_df$class))
    if (n_colors <= 7) {
      colors <- gbg_palette("sequential", "blue", n = n_colors)
    } else {
      base_colors <- gbg_palette("sequential", "blue", n = 7)
      colors <- colorRampPalette(base_colors)(n_colors)
    }
    p <- p + ggplot2::scale_fill_manual(values = colors)
  }
  
  return(p)
}