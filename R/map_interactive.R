# =============================================================================
# CREATE_INTERACTIVE_MAP() - Interaktiva kartor för webb och rapporter
# =============================================================================

library(sf)
library(dplyr)
library(ggplot2)
library(ggiraph)

#' Skapa interaktiv karta enligt Göteborgs Stads grafiska profil
#'
#' VARFÖR create_interactive_map?: Skapar interaktiva kartor för webb, Quarto-
#' rapporter och dashboards med tooltips och hover-effekter.
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
#' @section 8. INTERAKTIVITET (UNIKT FÖR INTERACTIVE):
#' @param tooltip_cols Kolumner att visa i tooltip (NULL = auto)
#' @param tooltip_alias Vektor med alias för kolumnnamn (i samma ordning som tooltip_cols), t.ex. c("Område", "Befolkning"). Kan också vara named list: list(omrade = "Område")
#' @param tooltip_decimals Named list med decimaler per kolumn, t.ex. list(tathet = 1)
#' @param tooltip_units Named list med enheter per kolumn, t.ex. list(tathet = " inv/km²")
#' @param tooltip_style "default", "gbg", "minimal"
#' @param hover_css CSS för hover-effekt
#' @param onclick_action Vad som händer vid klick (utvecklas senare)
#'
#' @section AVANCERAT:
#' @param simplify Förenkla geometri?
#' @param simplify_tolerance Tolerans i meter
#' @param validate_geometry Validera och fixa geometri?
#'
#' @return girafe-objekt (interaktiv HTML-widget)
#'
#' @examples
#' # EXEMPEL 1: Med redan merged data
#' map_data <- join_stat_to_map(geo, stat, by = "kod")
#' p1 <- create_interactive_map(
#'   map_data = map_data,
#'   value_col = "befolkning",
#'   tooltip_cols = c("omrade", "befolkning"),
#'   tooltip_alias = c("Område", "Befolkning")
#' )
#'
#' # EXEMPEL 2: Med separata filer
#' p2 <- create_interactive_map(
#'   stat_data = befolkning,
#'   geo_layer = "primaromraden",
#'   value_col = "Folkmängd",
#'   by = "omrade_kod",
#'   tooltip_style = "gbg"
#' )
#'
#' # EXEMPEL 3: Med extra lager
#' p3 <- create_interactive_map(
#'   map_data = data,
#'   value_col = "befolkning",
#'   extra_layers = list(
#'     list(name = "Göta Älv", data = "alv_goteborg", fill = "lightblue", layer_order = 1),
#'     list(data = "stadsdelar", color = "black", size = 1.2)
#'   ),
#'   tooltip_cols = c("omrade", "befolkning", "area")
#' )
#'
#' @export
create_interactive_map <- function(
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
  
  # 8. INTERAKTIVITET
  tooltip_cols = NULL,
  tooltip_alias = NULL,
  tooltip_decimals = NULL,
  tooltip_units = NULL,
  tooltip_style = "default",
  hover_css = "fill:white;stroke:black;stroke-width:1;",  # Vit fill, svart tunn linje
  onclick_action = NULL,
  
  # AVANCERAT
  simplify = FALSE,
  simplify_tolerance = 100,
  validate_geometry = TRUE
) {
  
  # ==========================================================================
  # STEG 1-4: SAMMA SOM STATIC (data, klassificering, färger, extra lager)
  # ==========================================================================
  
  message("Steg 1/9: Laddar geodata...")
  
  has_map_data <- !is.null(map_data)
  has_separate <- !is.null(geo_layer) && !is.null(stat_data)
  
  if (has_map_data && has_separate) {
    warning("Både map_data OCH geo_layer+stat_data angivna. Använder map_data.")
  }
  
  if (has_map_data) {
    if (!inherits(map_data, "sf")) {
      stop("map_data måste vara ett sf-objekt med geometri")
    }
    if (!value_col %in% names(map_data)) {
      stop("Kolumnen '", value_col, "' finns inte i map_data.\nTillgängliga: ", paste(names(map_data), collapse = ", "))
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
    map_data <- join_stat_to_map(map_data = geo_data, stat_data = stat_data, by = by, by_map = by_map, by_stat = by_stat)
    if (!value_col %in% names(map_data)) {
      stop("Kolumnen '", value_col, "' finns inte efter merge.\nTillgängliga: ", paste(names(map_data), collapse = ", "))
    }
    message("  ✓ ", nrow(map_data), " områden efter merge")
  } else {
    stop("Du måste ange ANTINGEN:\n  - map_data (redan merged sf-objekt), ELLER\n  - geo_layer + stat_data (separata filer att merga)")
  }
  
  # Klassificering
  if (!continuous) {
    message("\nSteg 2/9: Klassificerar data (", classify_method, ", ", n_classes, " klasser)...")
    class_breaks <- create_breaks(values = map_data[[value_col]], method = classify_method, n_classes = n_classes, breaks = breaks)
    if (label_style == "custom" && !is.null(label_custom)) {
      if (length(label_custom) != (length(class_breaks) - 1)) {
        warning("Antal custom labels matchar inte n_classes")
        class_labels <- create_labels(class_breaks, style = "range", decimals = label_decimals, unit = label_unit)
      } else {
        class_labels <- label_custom
      }
    } else {
      class_labels <- create_labels(breaks = class_breaks, style = label_style, decimals = label_decimals, unit = label_unit)
    }
    map_data$map_class <- apply_classification(values = map_data[[value_col]], breaks = class_breaks, labels = class_labels)
    
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
    message("\nSteg 2/9: Använder kontinuerlig skala...")
  }
  
  # Färger
  message("\nSteg 3/9: Väljer färgpalett...")
  if (!is.null(custom_colors)) {
    if (!is.character(custom_colors)) stop("custom_colors måste vara character vektor")
    valid_hex <- grepl("^#[0-9A-Fa-f]{6}$", custom_colors)
    if (!all(valid_hex)) stop("Ogiltiga hex-färger i custom_colors")
    if (!continuous && length(custom_colors) != n_classes) stop("Antal custom_colors matchar inte n_classes")
    colors <- custom_colors
    message("  ✓ Använder ", length(colors), " custom färger")
  } else {
    if (continuous && palette_type != "sequential") {
      message("  OBS: Continuous kräver palette_type = 'sequential'")
      palette_type <- "sequential"
      sequential_palettes <- c("blue", "green", "yellow_red", "dark_blue", "cyan", "yellow", "red", "pink", "purple", "yellow_green", "yellow_green_dark")
      if (!palette_name %in% sequential_palettes) {
        palette_name <- "blue"
        message("       Använder palette_name = 'blue'")
      }
    }
    colors <- gbg_palette(type = palette_type, palette = palette_name, n = if (!continuous) n_classes else NULL, direction = palette_direction)
    message("  ✓ Färger från gothenburg_colors (", palette_type, " - ", palette_name, ")")
  }
  
  # Extra lager (kompakt version)
  message("\nSteg 4/9: Hanterar extra lager...")
  if (!is.null(extra_layers)) {
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
    layer_orders <- sapply(extra_layers, function(x) x$layer_order)
    extra_layers <- extra_layers[order(layer_orders)]
    
    message(strrep("=", 60))
    message("LAGERORDNING")
    message(strrep("=", 60))
    main_layer_name <- if (has_map_data) "map_data" else if (is.character(geo_layer)) geo_layer else "geodata"
    message("Huvudlager: ", main_layer_name, " (position 50)\n")
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
        message(sprintf("  %d. %-30s (position %5s)%s → %s", i, substr(layer_name, 1, 30), format(pos, nsmall = 1), mark_text, relation))
      }
      message("\nTips: Sätt 'name' för tydligare beskrivning\n      Sätt 'layer_order' för exakt kontroll")
    }
    message(strrep("=", 60), "\n")
  } else {
    message("  Inga extra lager")
  }
  
  # ==========================================================================
  # STEG 5: SKAPA TOOLTIPS (UNIKT FÖR INTERACTIVE!)
  # ==========================================================================
  
  message("Steg 5/9: Skapar tooltips...")
  
  # Auto-välj kolumner om NULL
  if (is.null(tooltip_cols)) {
    numeric_cols <- names(map_data)[sapply(map_data, is.numeric)]
    char_cols <- names(map_data)[sapply(map_data, is.character)]
    tooltip_cols <- c(char_cols[1], value_col)
    tooltip_cols <- unique(tooltip_cols[!is.na(tooltip_cols)])
    message("  Auto-valde kolumner: ", paste(tooltip_cols, collapse = ", "))
  }
  
  # Konvertera tooltip_alias från vektor till named list om behövs
  # VARFÖR?: Gör det enklare för användaren - de kan skicka vektor istället för named list
  if (!is.null(tooltip_alias) && !is.list(tooltip_alias)) {
    if (length(tooltip_alias) == length(tooltip_cols)) {
      tooltip_alias <- setNames(as.list(tooltip_alias), tooltip_cols)
      message("  Konverterade tooltip_alias till named list")
    } else {
      warning(
        "tooltip_alias längd (", length(tooltip_alias), ") matchar inte tooltip_cols längd (", 
        length(tooltip_cols), "). Ignorerar tooltip_alias."
      )
      tooltip_alias <- NULL
    }
  }
  
  # VARFÖR skicka alias, col_decimals, col_units?: 
  # Ger användaren full kontroll över tooltip-formatering per kolumn
  map_data$tooltip_text <- .create_tooltip_html(
    data = map_data, 
    cols = tooltip_cols, 
    style = tooltip_style, 
    decimals = label_decimals,        # Global default
    alias = tooltip_alias,            # Kolumnnamn-alias
    col_decimals = tooltip_decimals,  # Per-kolumn decimaler
    col_units = tooltip_units         # Per-kolumn enheter
  )
  
  join_col <- if (!is.null(by_map)) by_map else if (!is.null(by)) by else names(map_data)[1]
  map_data$data_id <- if (join_col %in% names(map_data)) as.character(map_data[[join_col]]) else as.character(1:nrow(map_data))
  message("  ✓ Tooltips skapade för ", length(tooltip_cols), " kolumner")
  
  # ==========================================================================
  # STEG 6: SKAPA INTERAKTIV KARTA
  # ==========================================================================
  
  message("\nSteg 6/9: Bygger interaktiv karta...")
  p <- ggplot()
  
  if (!is.null(extra_layers)) {
    layers_below <- Filter(function(x) x$layer_order < 50, extra_layers)
    layers_at_50 <- Filter(function(x) x$layer_order == 50, extra_layers)
    if (length(layers_at_50) > 0) warning("Du har ", length(layers_at_50), " lager på position 50. Detta kan ge oväntade resultat!")
    if (length(layers_below) > 0) {
      message("  ✓ Lägger till ", length(layers_below), " lager UNDER huvudkartan")
      for (layer in layers_below) {
        layer_data <- .load_layer_data(layer)
        if (!is.null(layer_data)) {
          p <- p + geom_sf(data = layer_data, fill = layer$fill %||% NA, color = layer$color %||% NA,
                           linewidth = layer$size %||% 0.5, alpha = layer$alpha %||% 1, inherit.aes = FALSE)
        }
      }
    }
  }
  
  message("  ✓ Huvudkarta (interaktiv, position 50)")
  if (!continuous) {
    # DISKRET: Använd map_class med manuella färger
    p <- p + geom_sf_interactive(data = map_data, aes(fill = map_class, tooltip = tooltip_text, data_id = data_id),
                                 color = boundary_color, linewidth = boundary_size) + 
      scale_fill_manual(values = colors)
  } else {
    # CONTINUOUS: Använd gradient
    # VARFÖR fill direkt på value_col?: För smooth gradient utan klasser
    p <- p + geom_sf_interactive(data = map_data, aes(fill = .data[[value_col]], tooltip = tooltip_text, data_id = data_id),
                                 color = boundary_color, linewidth = boundary_size)
    
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
  
  if (!is.null(extra_layers)) {
    layers_above <- Filter(function(x) x$layer_order > 50, extra_layers)
    if (length(layers_above) > 0) {
      message("  ✓ Lägger till ", length(layers_above), " lager ÖVER huvudkartan")
      for (layer in layers_above) {
        layer_data <- .load_layer_data(layer)
        if (!is.null(layer_data)) {
          p <- p + geom_sf(data = layer_data, fill = layer$fill %||% NA, color = layer$color %||% NA,
                           linewidth = layer$size %||% 0.5, alpha = layer$alpha %||% 1, inherit.aes = FALSE)
        }
      }
    }
  }
  
  # ==========================================================================
  # STEG 7: APPLICERA DESIGN (FÖRE LEGEND)
  # ==========================================================================
  
  message("\nSteg 7/9: Applicerar design...")
  
  # Sätt legend-titel (men applicera inte legend än)
  if (is.null(legend_title)) legend_title <- value_col
  p <- p + labs(fill = legend_title)
  
  # Titlar
  if (!is.null(title) || !is.null(subtitle) || !is.null(caption)) {
    p <- p + labs(title = title, subtitle = subtitle, caption = caption)
  }
  
  # Applicera theme_void FÖRST (tar bort alla theme-element)
  p <- p + theme_void()
  
  # Sen lägg till custom theme (utan legend-inställningar)
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
  # STEG 8: KONFIGURERA LEGEND (EFTER theme_void!)
  # ==========================================================================
  # VARFÖR efter theme_void?: theme_void() tar bort alla theme-element,
  # så legend måste läggas till EFTER för att inte bli raderad
  
  message("\nSteg 8/9: Konfigurerar legend...")
  
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
    # VARFÖR skicka key_width/height?: Ger användaren kontroll över färgrutornas storlek
    # särskilt viktigt för horisontell legend med lång text
    # VARFÖR title_size/text_size?: Användaren kan justera textstorlek för legend oberoende av kartan
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
  # STEG 9: SKAPA GIRAFE
  # ==========================================================================
  
  message("\nSteg 9/9: Skapar girafe-objekt...")
  tooltip_css <- .get_tooltip_css(tooltip_style)
  
  # VARFÖR dessa options?: Optimerade för Quarto-rapporter
  # - width_svg/height_svg: NULL → använd ggplot's defaults (fungerar bättre i Quarto)
  # - opts_sizing: responsivt (anpassar sig till container)
  # - opts_tooltip: z-index högt för att ligga över Quarto-element
  
  interactive_map <- girafe(
    ggobj = p,
    width_svg = NULL,  # NULL = använd ggplot's storlek
    height_svg = NULL, # NULL = använd ggplot's storlek
    options = list(
      # Hover-effekt
      opts_hover(css = hover_css),
      
      # Tooltip med hög z-index för Quarto
      # VARFÖR z-index 9999?: Säkerställer att tooltip ligger över Quarto-element
      opts_tooltip(
        css = tooltip_css, 
        opacity = 0.95, 
        use_fill = FALSE, 
        use_stroke = FALSE,
        delay_mouseover = 0, 
        delay_mouseout = 0,
        zindex = 9999  # Högt z-index för Quarto-kompatibilitet
      ),
      
      # Toolbar
      opts_toolbar(saveaspng = FALSE),
      
      # Sizing: Responsivt för Quarto
      # VARFÖR rescale = FALSE?: Behåller aspect ratio i Quarto
      opts_sizing(rescale = FALSE, width = 1)
    )
  )
  message("  ✓ Interaktiv karta färdig!\n")
  return(interactive_map)
}


# =============================================================================
# HJÄLPFUNKTIONER
# =============================================================================

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

#' @keywords internal
.create_tooltip_html <- function(data, cols, style, decimals, 
                                 alias = NULL, col_decimals = NULL, col_units = NULL) {
  # VARFÖR tre decimals-parametrar?
  # - decimals: Global default (från label_decimals)
  # - col_decimals: Per-kolumn overrides (från tooltip_decimals)
  # - col_units: Enhet att lägga till efter värdet (från tooltip_units)
  
  data_clean <- data %>% st_drop_geometry()
  
  tooltips <- sapply(1:nrow(data_clean), function(i) {
    rows <- sapply(cols, function(col) {
      if (!col %in% names(data_clean)) return("")
      
      # Hämta värde
      value <- data_clean[[col]][i]
      
      # Bestäm kolumnnamn (med alias om finns)
      # VARFÖR alias?: Visa "Område" istället för "omrade"
      display_name <- if (!is.null(alias) && col %in% names(alias)) {
        alias[[col]]
      } else {
        col
      }
      
      # Formatera värde
      if (is.numeric(value) && !is.na(value)) {
        # VARFÖR !is.na?: Måste kolla NA innan abs() och format()
        
        # Bestäm decimaler för denna kolumn
        # VARFÖR per-kolumn decimaler?: Befolkning (0 dec) vs täthet (1 dec)
        col_dec <- if (!is.null(col_decimals) && col %in% names(col_decimals)) {
          col_decimals[[col]]
        } else {
          decimals  # Global default
        }
        
        # Formatera med tusentalsavskiljare för stora tal
        # VARFÖR tusentalsavskiljare?: 10000 → 10 000 (lättare att läsa)
        if (abs(value) >= 10000) {
          formatted_value <- format(round(value, col_dec), 
                                    big.mark = " ",      # Mellanslag som tusentalsavskiljare
                                    decimal.mark = ",",  # Komma som decimaltecken (svenskt)
                                    nsmall = col_dec)
        } else {
          formatted_value <- format_legend_labels(value, decimals = col_dec)
        }
        
        # Lägg till enhet om finns
        # VARFÖR enheter?: Tydliggör vad värdet betyder (t.ex. " inv/km²")
        if (!is.null(col_units) && col %in% names(col_units)) {
          formatted_value <- paste0(formatted_value, col_units[[col]])
        }
        
      } else if (is.na(value)) {
        # VARFÖR visa "Saknas"?: Tydligt för användaren att data saknas
        formatted_value <- "Saknas"
        
      } else {
        # Icke-numeriska värden: visa som de är
        formatted_value <- as.character(value)
      }
      
      # Bygg HTML baserat på style
      if (style == "gbg") {
        # Tabell-format (clean och organiserat)
        paste0("<tr><td style='padding:3px 8px;'><b>", display_name, 
               ":</b></td><td style='padding:3px 8px;'>", formatted_value, "</td></tr>")
      } else if (style == "minimal") {
        # Minimal format (kompakt)
        paste0("<b>", display_name, ":</b> ", formatted_value, "<br>")
      } else {
        # Default format (enkel)
        paste0("<b>", display_name, ":</b> ", formatted_value, "<br>")
      }
    })
    
    # Kombinera alla rader
    if (style == "gbg") {
      paste0("<div style='padding:5px;'><table style='border-collapse:collapse;'>", 
             paste(rows, collapse = ""), "</table></div>")
    } else {
      paste0("<div style='padding:5px;'>", paste(rows, collapse = ""), "</div>")
    }
  })
  
  return(tooltips)
}

#' @keywords internal
.get_tooltip_css <- function(style) {
  # VARFÖR !important?: Säkerställer att vår CSS vinner över Quarto's CSS
  # Quarto kan ha globala styles som krockar med tooltip-styling
  
  if (style == "gbg") {
    paste0(
      "background-color:#0076bc !important;",
      "color:white !important;",
      "font-family:Open Sans,Arial,sans-serif !important;",
      "font-size:13px !important;",
      "padding:8px !important;",
      "border-radius:4px !important;",
      "box-shadow:0 2px 4px rgba(0,0,0,0.2) !important;",
      "border:none !important;"  # Förhindra Quarto border
    )
  } else if (style == "minimal") {
    paste0(
      "background-color:white !important;",
      "color:#333 !important;",
      "font-family:Arial,sans-serif !important;",
      "font-size:12px !important;",
      "padding:5px !important;",
      "border:1px solid #ccc !important;",
      "border-radius:2px !important;"
    )
  } else {
    paste0(
      "background-color:#333 !important;",
      "color:white !important;",
      "font-family:Arial,sans-serif !important;",
      "font-size:12px !important;",
      "padding:8px !important;",
      "border-radius:3px !important;",
      "box-shadow:0 2px 4px rgba(0,0,0,0.3) !important;",
      "border:none !important;"
    )
  }
}