# =============================================================================
# CREATE_STATIC_MAP() - Statiska kartor för publicering
# =============================================================================

library(sf)
library(dplyr)
library(ggplot2)

#' Skapa statisk karta enligt Göteborgs Stads grafiska profil
#'
#' VARFÖR create_static_map?: Skapar professionella kartor för publicering,
#' rapporter och presentationer med full kontroll över design.
#'
#' @section 1. STATISTIK & GEODATA:
#' @param map_data sf-objekt med data redan merged (alternativ till geo_layer + stat_data)
#' @param stat_data Data.frame med statistik (om inte map_data används)
#' @param geo_layer Namn på kartlager eller sf-objekt (om inte map_data används)
#' @param value_col Kolumn att visualisera
#' @param by Kopplingskolumn (om separata filer)
#' @param by_map Kopplingskolumn i kartlager (om olika från by)
#' @param by_stat Kopplingskolumn i statistik (om olika från by)
#'
#' @section 2. FÄRGER:
#' @param palette_type "sequential", "diverging", "categorical"
#' @param palette_name Palett-namn från gothenburg_colors
#' @param palette_direction 1 = normal, -1 = omvänd
#' @param continuous Använd kontinuerlig gradient? (default: FALSE)
#' @param custom_colors Vektor med hex-färger (överrider paletter)
#'
#' @section 3. KLASSIFICERING:
#' @param classify_method "quantile", "equal", "fisher" (rekommenderad), "jenks", "pretty", "sd", "kmeans", "hclust", "dpih", "headtails", "maximum", "box", "manual"
#' @param n_classes Antal klasser (2-15, ignoreras för dpih/headtails/box som väljer själva)
#' @param breaks Manuella breaks (om classify_method = "manual")
#' @param continuous_limits c(min, max) - Manuella gränser för gradient (NULL = auto från data)
#' @param continuous_n_labels Antal labels i gradient-legend (default: 5)
#' @param continuous_breaks Manuella breaks för gradient-labels (NULL = jämnt fördelade)
#'
#' @section 4. LABELS:
#' @param label_style "range", "ruler", "custom"
#' @param label_custom Egna labels (om label_style = "custom")
#' @param label_decimals Antal decimaler
#' @param label_unit Enhet (t.ex. "%", "inv/km²")
#'
#' @section 5. LEGEND:
#' @param legend_show Visa legend? (default: TRUE)
#' @param legend_position "right", "topleft", "bottomright", c(x,y), "none"
#' @param legend_title Titel (NULL = använd value_col)
#' @param legend_size "small", "medium", "large"
#' @param legend_key_width Bredd på färgrutor i mm (NULL = auto baserat på size)
#' @param legend_key_height Höjd på färgrutor i mm (NULL = auto baserat på size)
#' @param legend_title_size Textstorlek för legend-titel i pt (NULL = auto: 9/11/13)
#' @param legend_text_size Textstorlek för legend-text i pt (NULL = auto: 7/9/11)
#' @param legend_background Bakgrundsfärg för legend ("white", "transparent", hex-färg) (default: "white")
#' @param legend_show_n Visa antal observationer per klass? (default: FALSE)
#' @param legend_n_format Format för antal, t.ex. "[%d]" eller "(n=%d)". VIKTIGT: Ange UTAN \n - separator läggs till automatiskt! (default: "[%d]")
#'
#' @section 6. DESIGN:
#' @param title Kartans titel
#' @param subtitle Undertitel
#' @param caption Källhänvisning
#' @param boundary_color Färg på områdesgränser
#' @param boundary_size Tjocklek på gränser
#' @param background_color Bakgrundsfärg
#'
#' @section 7. EXTRA LAGER:
#' @param extra_layers Lista med extra lager. Varje lager är en lista med:
#'   - name: Namn på lagret (optional, för tydlig output)
#'   - data: sf-objekt eller lagernamn
#'   - fill: Fyllnadsfärg (default: NA)
#'   - color: Linjefärg (default: NA)
#'   - size: Linjetjocklek (default: 0.5)
#'   - alpha: Transparens (default: 1)
#'   - layer_order: Position (huvudkarta = 50, lägre = under, högre = över)
#'
#' @section 8. TEXTINFORMATION:
#' @param print_top_bottom Skriv ut topp/botten i konsol?
#' @param top_n Antal områden i topp/botten
#' @param show_labels Visa etiketter i kartan?
#' @param label_n Antal områden att etikettera
#' @param label_type "name", "value", "name_value"
#' @param label_size Textstorlek
#' @param label_force Repulsion force (ggrepel)
#' @param label_max_overlaps Max överlappningar
#'
#' @section AVANCERAT:
#' @param simplify Förenkla geometri?
#' @param simplify_tolerance Tolerans i meter
#' @param validate_geometry Validera och fixa geometri?
#'
#' @return ggplot-objekt
#'
#' @examples
#' # EXEMPEL 1: Med redan merged data
#' map_data <- join_stat_to_map(geo, stat, by = "kod")
#' p1 <- create_static_map(map_data = map_data, value_col = "befolkning")
#'
#' # EXEMPEL 2: Med separata filer
#' p2 <- create_static_map(
#'   stat_data = befolkning,
#'   geo_layer = "primaromraden",
#'   value_col = "Folkmängd",
#'   by = "omrade_kod"
#' )
#'
#' # EXEMPEL 3: Med extra lager
#' p3 <- create_static_map(
#'   map_data = data,
#'   value_col = "befolkning",
#'   extra_layers = list(
#'     list(name = "Göta Älv", data = "alv_goteborg", fill = "lightblue", layer_order = 1),
#'     list(data = "stadsdelar", color = "black", size = 1.2)  # Auto: layer_order = 51
#'   )
#' )
#'
#' @export
create_static_map <- function(
    # 1. STATISTIK & GEODATA
  map_data = NULL,
  stat_data = NULL,
  geo_layer = NULL,
  value_col,
  by = NULL,
  by_map = NULL,
  by_stat = NULL,
  
  # 2. FÄRGER
  palette_type = "sequential",
  palette_name = "blue",
  palette_direction = 1,
  continuous = FALSE,
  custom_colors = NULL,
  
  # 3. KLASSIFICERING
  classify_method = "fisher",
  n_classes = 5,
  breaks = NULL,
  continuous_limits = NULL,
  continuous_n_labels = 5,
  continuous_breaks = NULL,
  
  # 4. LABELS
  label_style = "range",
  label_custom = NULL,
  label_decimals = 1,  # ÄNDRAT: 1 decimal som default för att undvika dubbletter
  label_unit = NULL,
  
  # 5. LEGEND
  legend_show = TRUE,
  legend_position = "right",
  legend_title = NULL,
  legend_size = "medium",
  legend_key_width = NULL,
  legend_key_height = NULL,
  legend_title_size = NULL,
  legend_text_size = NULL,
  legend_background = "white",
  legend_show_n = FALSE,
  legend_n_format = "[%d]",
  
  # 6. DESIGN
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  boundary_color = "black",
  boundary_size = 0.1,
  background_color = "white",
  
  # 7. EXTRA LAGER
  extra_layers = NULL,
  
  # 8. TEXTINFORMATION
  print_top_bottom = FALSE,
  top_n = 5,
  show_labels = FALSE,
  label_n = 10,
  label_type = "value",
  label_size = 3,
  label_force = 1,
  label_max_overlaps = 15,
  
  # AVANCERAT
  simplify = FALSE,
  simplify_tolerance = 100,
  validate_geometry = TRUE
) {
  
  # ==========================================================================
  # STEG 1: HANTERA DATA
  # ==========================================================================
  # VARFÖR två alternativ?: Flexibilitet - ibland har du redan mergat data,
  # ibland vill du att funktionen gör det.
  
  message("Steg 1/8: Laddar geodata...")
  
  has_map_data <- !is.null(map_data)
  has_separate <- !is.null(geo_layer) && !is.null(stat_data)
  
  if (has_map_data && has_separate) {
    warning("Både map_data OCH geo_layer+stat_data angivna. Använder map_data.")
  }
  
  if (has_map_data) {
    # Redan merged data
    if (!inherits(map_data, "sf")) {
      stop("map_data måste vara ett sf-objekt med geometri")
    }
    
    if (!value_col %in% names(map_data)) {
      stop(
        "Kolumnen '", value_col, "' finns inte i map_data.\n",
        "Tillgängliga: ", paste(names(map_data), collapse = ", ")
      )
    }
    
    if (validate_geometry) {
      invalid <- !st_is_valid(map_data)
      if (any(invalid)) {
        message("  Fixar ogiltig geometri...")
        map_data <- st_make_valid(map_data)
      }
    }
    
    if (simplify) {
      message("  Förenklar geometri...")
      map_data <- simplify_geometry(map_data, tolerance = simplify_tolerance)
    }
    
    message("  ✓ ", nrow(map_data), " områden (redan merged)")
    
  } else if (has_separate) {
    # Separata filer - merge internt
    if (is.character(geo_layer)) {
      geo_data <- load_prepared_map(geo_layer)
    } else if (inherits(geo_layer, "sf")) {
      geo_data <- geo_layer
    } else {
      stop("geo_layer måste vara lagernamn eller sf-objekt")
    }
    
    if (simplify) {
      message("  Förenklar geometri...")
      geo_data <- simplify_geometry(geo_data, tolerance = simplify_tolerance)
    }
    
    if (validate_geometry) {
      invalid <- !st_is_valid(geo_data)
      if (any(invalid)) {
        message("  Fixar ogiltig geometri...")
        geo_data <- st_make_valid(geo_data)
      }
    }
    
    message("  Mergar med statistik...")
    map_data <- join_stat_to_map(
      map_data = geo_data,
      stat_data = stat_data,
      by = by,
      by_map = by_map,
      by_stat = by_stat
    )
    
    if (!value_col %in% names(map_data)) {
      stop(
        "Kolumnen '", value_col, "' finns inte efter merge.\n",
        "Tillgängliga: ", paste(names(map_data), collapse = ", ")
      )
    }
    
    message("  ✓ ", nrow(map_data), " områden efter merge")
    
  } else {
    stop(
      "Du måste ange ANTINGEN:\n",
      "  - map_data (redan merged sf-objekt), ELLER\n",
      "  - geo_layer + stat_data (separata filer att merga)"
    )
  }
  
  # ==========================================================================
  # STEG 2: KLASSIFICERA DATA
  # ==========================================================================
  # VARFÖR separata funktioner?: Ger flexibilitet att testa breaks och labels
  # oberoende av varandra.
  
  if (!continuous) {
    message("\nSteg 2/8: Klassificerar data (", classify_method, ", ", n_classes, " klasser)...")
    
    class_breaks <- create_breaks(
      values = map_data[[value_col]],
      method = classify_method,
      n_classes = n_classes,
      breaks = breaks
    )
    
    if (label_style == "custom" && !is.null(label_custom)) {
      if (length(label_custom) != (length(class_breaks) - 1)) {
        warning("Antal custom labels matchar inte n_classes")
        class_labels <- create_labels(class_breaks, style = "range", decimals = label_decimals, unit = label_unit)
      } else {
        class_labels <- label_custom
      }
    } else {
      class_labels <- create_labels(
        breaks = class_breaks,
        style = label_style,
        decimals = label_decimals,
        unit = label_unit
      )
    }
    
    map_data$map_class <- apply_classification(
      values = map_data[[value_col]],
      breaks = class_breaks,
      labels = class_labels
    )
    
    # Lägg till antal observationer i labels om requested
    # VARFÖR efter apply_classification?: Vi behöver räkna hur många obs i varje klass först
    if (legend_show_n) {
      # Räkna observationer per klass
      class_counts <- table(map_data$map_class)
      
      # Bestäm separator baserat på legend_position
      # VARFÖR olika separator?: Vertikal legend har plats på samma rad, horisontell behöver ny rad
      is_horizontal <- legend_position %in% c("top", "bottom")
      n_separator <- if (is_horizontal) "\n" else "  "  # Newline för horisontell, mellanslag för vertikal
      
      # VIKTIGT: Ta bort eventuell \n från legend_n_format
      # VARFÖR?: Vi lägger till separator automatiskt baserat på legend_position
      clean_format <- gsub("^\\n", "", legend_n_format)  # Ta bort \n i början
      
      # Uppdatera labels med antal
      # VARFÖR sprintf?: Använder legend_n_format som mall (t.ex. "[%d]")
      class_labels_with_n <- sapply(class_labels, function(label) {
        count <- as.numeric(class_counts[label])
        if (is.na(count)) count <- 0
        paste0(label, n_separator, sprintf(clean_format, count))
      })
      
      # Uppdatera map_class med nya labels
      # VARFÖR factor med levels?: Säkerställer rätt ordning
      map_data$map_class <- factor(
        as.character(map_data$map_class),
        levels = class_labels,
        labels = class_labels_with_n
      )
      
      # Uppdatera class_labels för senare användning
      class_labels <- class_labels_with_n
      
      message("  ✓ Lade till antal observationer i legend-labels")
    }
    
    message("  ✓ Klassificering klar")
  } else {
    message("\nSteg 2/8: Använder kontinuerlig skala...")
  }
  
  # ==========================================================================
  # STEG 3: VÄLJ FÄRGER
  # ==========================================================================
  # VARFÖR gothenburg_colors?: Säkerställer konsekvent design enligt grafisk profil
  
  message("\nSteg 3/8: Väljer färgpalett...")
  
  if (!is.null(custom_colors)) {
    if (!is.character(custom_colors)) {
      stop("custom_colors måste vara character vektor")
    }
    valid_hex <- grepl("^#[0-9A-Fa-f]{6}$", custom_colors)
    if (!all(valid_hex)) {
      stop("Ogiltiga hex-färger i custom_colors")
    }
    if (!continuous && length(custom_colors) != n_classes) {
      stop("Antal custom_colors matchar inte n_classes")
    }
    colors <- custom_colors
    message("  ✓ Använder ", length(colors), " custom färger")
  } else {
    if (continuous && palette_type != "sequential") {
      message("  OBS: Continuous kräver palette_type = 'sequential'")
      palette_type <- "sequential"
      sequential_palettes <- c("blue", "green", "yellow_red", "dark_blue",
                               "cyan", "yellow", "red", "pink", "purple",
                               "yellow_green", "yellow_green_dark")
      if (!palette_name %in% sequential_palettes) {
        palette_name <- "blue"
        message("       Använder palette_name = 'blue'")
      }
    }
    colors <- gbg_palette(
      type = palette_type,
      palette = palette_name,
      n = if (!continuous) n_classes else NULL,
      direction = palette_direction
    )
    message("  ✓ Färger från gothenburg_colors (", palette_type, " - ", palette_name, ")")
  }
  
  # ==========================================================================
  # STEG 4: HANTERA EXTRA LAGER
  # ==========================================================================
  # VARFÖR layer_order system?: Ger full kontroll över exakt var varje lager
  # placeras relativt huvudkartan (50).
  
  message("\nSteg 4/8: Hanterar extra lager...")
  
  if (!is.null(extra_layers)) {
    
    # Auto-tilldela layer_order
    next_auto_order <- 51
    for (i in seq_along(extra_layers)) {
      if (is.null(extra_layers[[i]]$layer_order)) {
        extra_layers[[i]]$layer_order <- next_auto_order
        extra_layers[[i]]$auto_assigned <- TRUE
        next_auto_order <- next_auto_order + 1
      } else {
        extra_layers[[i]]$auto_assigned <- FALSE
      }
    }
    
    # Hantera duplicates
    layer_orders <- sapply(extra_layers, function(x) x$layer_order)
    duplicates <- duplicated(layer_orders) | duplicated(layer_orders, fromLast = TRUE)
    
    if (any(duplicates)) {
      dup_positions <- unique(layer_orders[duplicates])
      message("\n⚠ VARNING: Flera lager har samma position")
      message("   Justerar automatiskt med små steg (0.1, 0.2, ...)\n")
      
      for (pos in dup_positions) {
        indices <- which(layer_orders == pos)
        if (length(indices) > 1) {
          message("   Position ", pos, ": ", length(indices), " lager")
          for (j in seq_along(indices)) {
            idx <- indices[j]
            new_pos <- pos + (j - 1) * 0.1
            extra_layers[[idx]]$layer_order <- new_pos
            extra_layers[[idx]]$adjusted <- TRUE
            layer_name <- .get_layer_name(extra_layers[[idx]], idx)
            message("     → ", layer_name, " justerad till ", format(new_pos, nsmall = 1))
          }
        }
      }
      message("")
    }
    
    # Sortera
    layer_orders <- sapply(extra_layers, function(x) x$layer_order)
    extra_layers <- extra_layers[order(layer_orders)]
    
    # Visa lagerordning
    message(strrep("=", 60))
    message("LAGERORDNING")
    message(strrep("=", 60))
    
    main_layer_name <- if (has_map_data) "map_data" else if (is.character(geo_layer)) geo_layer else "geodata"
    message("Huvudlager: ", main_layer_name, " (position 50)")
    message("")
    
    if (length(extra_layers) > 0) {
      message("Extra lager:")
      for (i in seq_along(extra_layers)) {
        layer <- extra_layers[[i]]
        pos <- layer$layer_order
        relation <- if (pos < 50) "UNDER huvudlager" else if (pos == 50) "SAMMA som huvudlager" else "ÖVER huvudlager"
        layer_name <- .get_layer_name(layer, i)
        
        marks <- character(0)
        if (layer$auto_assigned) marks <- c(marks, "auto")
        if (!is.null(layer$adjusted) && layer$adjusted) marks <- c(marks, "justerad")
        mark_text <- if (length(marks) > 0) paste0(" [", paste(marks, collapse = ", "), "]") else ""
        
        message(sprintf("  %d. %-30s (position %5s)%s → %s", 
                        i, substr(layer_name, 1, 30), format(pos, nsmall = 1), mark_text, relation))
      }
      message("")
      message("Tips: Sätt 'name' för tydligare beskrivning")
      message("      Sätt 'layer_order' för exakt kontroll")
    }
    message(strrep("=", 60), "\n")
  } else {
    message("  Inga extra lager")
  }
  
  # ==========================================================================
  # STEG 5: SKAPA BASKARTA
  # ==========================================================================
  
  message("Steg 5/8: Bygger karta...")
  
  p <- ggplot()
  
  # Lager UNDER huvudkartan
  if (!is.null(extra_layers)) {
    layers_below <- Filter(function(x) x$layer_order < 50, extra_layers)
    layers_at_50 <- Filter(function(x) x$layer_order == 50, extra_layers)
    
    if (length(layers_at_50) > 0) {
      warning("Du har ", length(layers_at_50), " lager på position 50. Detta kan ge oväntade resultat!")
    }
    
    if (length(layers_below) > 0) {
      message("  ✓ Lägger till ", length(layers_below), " lager UNDER huvudkartan")
      for (layer in layers_below) {
        # VARFÖR kolla render?: Label-lager har egen render-funktion
        if (!is.null(layer$render)) {
          # Label-lager med custom rendering
          p <- p + layer$render(layer$data)
        } else {
          # Standard geom_sf lager
          layer_data <- .load_layer_data(layer)
          if (!is.null(layer_data)) {
            p <- p + geom_sf(
              data = layer_data,
              fill = layer$fill %||% NA,
              color = layer$color %||% NA,
              linewidth = layer$size %||% 0.5,
              alpha = layer$alpha %||% 1,
              inherit.aes = FALSE
            )
          }
        }
      }
    }
  }
  
  # HUVUDKARTAN
  message("  ✓ Huvudkarta (tematisk, position 50)")
  if (!continuous) {
    # DISKRET: Använd map_class med manuella färger
    p <- p +
      geom_sf(data = map_data, aes(fill = map_class), color = boundary_color, linewidth = boundary_size) +
      scale_fill_manual(values = colors)
  } else {
    # CONTINUOUS: Använd gradient
    # VARFÖR fill direkt på value_col?: För smooth gradient utan klasser
    p <- p +
      geom_sf(data = map_data, aes(fill = .data[[value_col]]), color = boundary_color, linewidth = boundary_size)
    
    # Bestäm limits (min/max för gradient)
    # VARFÖR limits?: Styr färgskalans spann och ger snygga heltal
    if (is.null(continuous_limits)) {
      continuous_limits <- range(map_data[[value_col]], na.rm = TRUE)
    }
    
    # Bestäm breaks för legend
    # VARFÖR breaks?: Visar labels på gradienten (t.ex. 0, 25, 50, 75, 100)
    if (is.null(continuous_breaks)) {
      continuous_breaks <- seq(continuous_limits[1], continuous_limits[2], 
                               length.out = continuous_n_labels)
    }
    
    # Applicera gradient-scale baserat på palette_type
    if (palette_type == "sequential") {
      p <- p + scale_fill_gbg_sequential(
        palette = palette_name, 
        discrete = FALSE, 
        direction = palette_direction,
        limits = continuous_limits,
        breaks = continuous_breaks
      )
    } else if (palette_type == "diverging") {
      p <- p + scale_fill_gbg_diverging(
        palette = palette_name, 
        discrete = FALSE, 
        direction = palette_direction,
        limits = continuous_limits,
        breaks = continuous_breaks
      )
    }
  }
  
  # Lager ÖVER huvudkartan
  if (!is.null(extra_layers)) {
    layers_above <- Filter(function(x) x$layer_order > 50, extra_layers)
    if (length(layers_above) > 0) {
      message("  ✓ Lägger till ", length(layers_above), " lager ÖVER huvudkartan")
      for (layer in layers_above) {
        # VARFÖR kolla render?: Label-lager har egen render-funktion
        if (!is.null(layer$render)) {
          # Label-lager med custom rendering
          p <- p + layer$render(layer$data)
        } else {
          # Standard geom_sf lager
          layer_data <- .load_layer_data(layer)
          if (!is.null(layer_data)) {
            p <- p + geom_sf(
              data = layer_data,
              fill = layer$fill %||% NA,
              color = layer$color %||% NA,
              linewidth = layer$size %||% 0.5,
              alpha = layer$alpha %||% 1,
              inherit.aes = FALSE
            )
          }
        }
      }
    }
  }
  
  # ==========================================================================
  # STEG 6: APPLICERA DESIGN (FÖRE LEGEND)
  # ==========================================================================
  
  message("\nSteg 6/8: Applicerar design...")
  
  # Sätt legend-titel (men applicera inte legend än)
  if (is.null(legend_title)) legend_title <- value_col
  p <- p + labs(fill = legend_title)
  
  # Titlar
  if (!is.null(title) || !is.null(subtitle) || !is.null(caption)) {
    p <- p + labs(title = title, subtitle = subtitle, caption = caption)
  }
  
  # Applicera theme_void FÖRST
  p <- p + theme_void()
  
  # Sen lägg till custom theme (utan legend)
  # VARFÖR hjust = 0 för caption?: Vänsterjusterad enligt användares önskemål
  p <- p + theme(
    plot.background = element_rect(fill = background_color, color = NA),
    panel.background = element_rect(fill = background_color, color = NA),
    plot.title = element_text(size = 14, face = "bold", hjust = 0, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 11, hjust = 0, margin = margin(b = 10)),
    plot.caption = element_text(size = 9, hjust = 0, color = "gray40", margin = margin(t = 10)),  # Vänsterjusterad!
    plot.margin = margin(10, 10, 10, 10)
  )
  
  # ==========================================================================
  # STEG 7: KONFIGURERA LEGEND (EFTER theme_void!)
  # ==========================================================================
  # VARFÖR efter theme_void?: theme_void() tar bort alla theme-element
  
  message("\nSteg 7/8: Konfigurerar legend...")
  
  if (!legend_show) {
    p <- p + hide_legend()
    message("  ✓ Legend dold")
  } else if (continuous) {
    # CONTINUOUS: Använd guide_colorbar för gradient
    # VARFÖR guide_colorbar?: Visar smooth gradient bar istället för diskreta rutor
    
    # Bestäm bar-storlek baserat på legend_position
    is_horizontal <- legend_position %in% c("top", "bottom")
    
    # Default storlekar för colorbar
    if (is.null(legend_key_width)) {
      legend_key_width <- if (is_horizontal) 100 else 10  # mm
    }
    if (is.null(legend_key_height)) {
      legend_key_height <- if (is_horizontal) 5 else 100  # mm
    }
    
    # Text-storlekar
    text_sizes <- list(small = 7, medium = 9, large = 11)
    title_sizes <- list(small = 9, medium = 11, large = 13)
    if (is.null(legend_text_size)) legend_text_size <- text_sizes[[legend_size]]
    if (is.null(legend_title_size)) legend_title_size <- title_sizes[[legend_size]]
    
    # Applicera colorbar guide
    p <- p + guides(
      fill = guide_colorbar(
        barwidth = unit(legend_key_width, "mm"),
        barheight = unit(legend_key_height, "mm"),
        title.position = if (is_horizontal) "top" else "top",
        label.position = if (is_horizontal) "bottom" else "right",
        frame.colour = "gray80",
        frame.linewidth = 0.3,
        ticks = TRUE,
        ticks.colour = "gray60",
        ticks.linewidth = 0.3
      )
    )
    
    # Applicera legend position och text-storlekar
    p <- p + configure_legend_position(legend_position, orientation = if (is_horizontal) "horizontal" else "vertical")
    p <- p + theme(
      legend.text = element_text(size = legend_text_size, color = "black"),
      legend.title = element_text(size = legend_title_size, color = "black", face = "bold"),
      legend.background = element_rect(fill = legend_background, color = NA)  # Användarkonfigurerbar!
    )
    
    message("  ✓ Legend konfigurerad (continuous gradient, ", legend_position, ", ", legend_size, ")")
    
  } else {
    # DISCRETE: Använd vanliga configure_legend
    p <- p + configure_legend(
      position = legend_position,
      size = legend_size,
      key_width = legend_key_width,
      key_height = legend_key_height,
      title_size = legend_title_size,
      text_size = legend_text_size,
      background = legend_background  # NYTT: Bakgrundsfärg
    )
    
    # Info-meddelande
    key_info <- ""
    if (!is.null(legend_key_width)) key_info <- paste0(", bredd: ", legend_key_width, "mm")
    if (!is.null(legend_key_height)) key_info <- paste0(key_info, ", höjd: ", legend_key_height, "mm")
    if (!is.null(legend_title_size)) key_info <- paste0(key_info, ", titel: ", legend_title_size, "pt")
    if (!is.null(legend_text_size)) key_info <- paste0(key_info, ", text: ", legend_text_size, "pt")
    
    message("  ✓ Legend konfigurerad (", legend_position, ", ", legend_size, key_info, ")")
  }
  
  # ==========================================================================
  # STEG 8: TEXTINFORMATION
  # ==========================================================================
  
  if (print_top_bottom) {
    message("\nSteg 8/8: Skriver ut topp/botten...")
    .print_top_bottom_analysis(map_data, value_col, top_n, label_decimals, by, by_map)
  } else {
    message("\nSteg 8/8: Ingen topp/botten-analys")
  }
  
  if (show_labels) {
    if (!requireNamespace("ggrepel", quietly = TRUE)) {
      warning("Paketet 'ggrepel' krävs för show_labels. Hoppar över.")
    } else {
      message("  Lägger till etiketter...")
      p <- .add_labels_to_map(p, map_data, value_col, label_n, label_type, label_size, 
                              label_force, label_max_overlaps, label_decimals, by, by_map)
      message("  ✓ Etiketter tillagda")
    }
  }
  
  message("\n✓ Statisk karta färdig!\n")
  return(p)
}


# =============================================================================
# INTERNA HJÄLPFUNKTIONER
# =============================================================================

#' Hämta namn på lager
#' @keywords internal
.get_layer_name <- function(layer, index) {
  if (!is.null(layer$name)) return(layer$name)
  if (is.character(layer$data)) return(layer$data)
  if (inherits(layer$data, "sf")) {
    n <- nrow(layer$data)
    return(paste0("sf-objekt (", n, " feature", if (n != 1) "s" else "", ")"))
  }
  return(paste0("lager #", index))
}

#' Ladda lagerdata
#' @keywords internal
.load_layer_data <- function(layer) {
  tryCatch({
    if (is.character(layer$data)) {
      load_prepared_map(layer$data)
    } else if (inherits(layer$data, "sf")) {
      layer$data
    } else {
      warning("Ogiltigt lager, hoppar över")
      NULL
    }
  }, error = function(e) {
    warning("Kunde inte ladda lager: ", e$message)
    NULL
  })
}

#' Skriv ut topp/botten-analys
#' @keywords internal
.print_top_bottom_analysis <- function(map_data, value_col, top_n, decimals, by, by_map) {
  message("\n=== TOPP/BOTTEN ANALYS ===\n")
  join_col <- if (!is.null(by_map)) by_map else if (!is.null(by)) by else names(map_data)[1]
  
  sorted <- map_data %>% st_drop_geometry() %>% arrange(desc(.data[[value_col]]))
  
  message("TOPP ", top_n, " (", value_col, "):")
  top_areas <- sorted %>% slice_head(n = top_n)
  for (i in 1:min(top_n, nrow(top_areas))) {
    message("  ", i, ". ", top_areas[[join_col]][i], ": ", 
            format_legend_labels(top_areas[[value_col]][i], decimals = decimals))
  }
  
  message("\nBOTTEN ", top_n, " (", value_col, "):")
  bottom_areas <- sorted %>% slice_tail(n = top_n) %>% arrange(.data[[value_col]])
  for (i in 1:min(top_n, nrow(bottom_areas))) {
    message("  ", i, ". ", bottom_areas[[join_col]][i], ": ", 
            format_legend_labels(bottom_areas[[value_col]][i], decimals = decimals))
  }
  message("\n========================\n")
}

#' Lägg till etiketter
#' @keywords internal
.add_labels_to_map <- function(p, map_data, value_col, label_n, label_type, label_size,
                               label_force, label_max_overlaps, decimals, by, by_map) {
  join_col <- if (!is.null(by_map)) by_map else if (!is.null(by)) by else names(map_data)[1]
  
  top_areas <- map_data %>% arrange(desc(.data[[value_col]])) %>% slice_head(n = label_n)
  label_data <- top_areas %>% st_drop_geometry() %>%
    bind_cols(st_coordinates(st_centroid(top_areas))) %>% rename(long = X, lat = Y)
  
  if (label_type == "name") {
    label_data$label_text <- label_data[[join_col]]
  } else if (label_type == "value") {
    label_data$label_text <- format_legend_labels(label_data[[value_col]], decimals = decimals)
  } else if (label_type == "name_value") {
    formatted_value <- format_legend_labels(label_data[[value_col]], decimals = decimals)
    label_data$label_text <- paste0(label_data[[join_col]], "\n", formatted_value)
  }
  
  p + ggrepel::geom_label_repel(
    data = label_data, aes(x = long, y = lat, label = label_text),
    size = label_size, force = label_force, max.overlaps = label_max_overlaps,
    box.padding = 0.35, point.padding = 0.5, segment.color = "grey50",
    segment.size = 0.5, arrow = arrow(length = unit(0.01, "npc")), inherit.aes = FALSE
  )
}


# =============================================================================
# HJÄLPFUNKTIONER FÖR ANVÄNDARE
# =============================================================================

#' Förhandsgranska klassindelning
#' @export
preview_classification <- function(stat_data, value_col, methods = c("quantile", "fisher", "equal"), n_classes = 5) {
  compare_methods(values = stat_data[[value_col]], methods = methods, n_classes = n_classes)
}