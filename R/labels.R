# =============================================================================
# MAP LABELS - Etiketter på kartor (inte legend-labels!)
# =============================================================================

library(sf)
library(dplyr)
library(ggplot2)
library(ggrepel)


# 1. VALUE LABELS =============================================================

#' Lägg till värde-etiketter i områden
#' 
#' VARFÖR add_value_labels?: Visa statistikvärden direkt i kartområden,
#' användbart för att snabbt läsa av exakta värden.
#' 
#' @param data sf-objekt med områden och värden
#' @param value_col Kolumn med värden att visa
#' @param areas Vilka områden att visa labels för (NULL = alla, eller vektor med ID/namn)
#' @param size Textstorlek (default: 3)
#' @param color Textfärg ("auto" = kontrast med bakgrund, "conditional" = röd för negativa, svart för positiva, eller hex-färg)
#' @param decimals Antal decimaler (default: 0)
#' @param prefix Text före värdet (t.ex. "+", "kr ")
#' @param suffix Text efter värdet (t.ex. "%", " inv")
#' @param min_area Minsta area (km²) för att visa label (filtrerar små områden)
#' @param layer_order Position i lagerordning (default: 70, över huvudkarta men under highlights)
#' @param fontface "plain", "bold", "italic", "bold.italic"
#' @param alpha Transparens 0-1 (default: 1)
#' 
#' @return Lista med data och parametrar för extra_layers
#' 
#' @examples
#' # Visa befolkning i alla områden
#' add_value_labels(
#'   data = befolkning_data,
#'   value_col = "befolkning",
#'   decimals = 0
#' )
#' 
#' # Visa förändring i procent för utvalda områden
#' add_value_labels(
#'   data = befolkning_data,
#'   value_col = "forandring_procent",
#'   areas = c("Centrum", "Hisingen"),
#'   decimals = 1,
#'   suffix = "%",
#'   color = "#333333"
#' )
#' 
#' @export
add_value_labels <- function(data,
                             value_col,
                             areas = NULL,
                             size = 3,
                             color = "auto",
                             decimals = 0,
                             prefix = "",
                             suffix = "",
                             min_area = 0,
                             layer_order = 70,
                             fontface = "plain",
                             alpha = 1) {
  
  # VARFÖR returnera lista?: För att passa in i extra_layers systemet
  # som förväntar sig en lista med data + parametrar
  
  # Validering
  if (!inherits(data, "sf")) {
    stop("data måste vara ett sf-objekt")
  }
  
  if (!value_col %in% names(data)) {
    stop("Kolumnen '", value_col, "' finns inte i data")
  }
  
  # Filtrera på areas om specificerat
  # VARFÖR filtrera?: Användaren kanske bara vill visa vissa områden
  label_data <- data
  
  if (!is.null(areas)) {
    # Försök matcha mot olika kolumner (flexibelt)
    # VARFÖR flera kolumner?: Användaren kanske anger ID eller namn
    match_cols <- c("name", "namn", "omrade", "omrade_namn", "kod", "id", names(data)[1])
    matched <- FALSE
    
    for (col in match_cols) {
      if (col %in% names(label_data)) {
        if (any(label_data[[col]] %in% areas)) {
          label_data <- label_data %>% filter(.data[[col]] %in% areas)
          matched <- TRUE
          break
        }
      }
    }
    
    if (!matched) {
      warning("Kunde inte matcha 'areas' mot någon kolumn. Visar alla områden.")
    }
  }
  
  # Filtrera på area-storlek om specificerat
  # VARFÖR min_area?: Små områden blir lätt överfulla med text
  if (min_area > 0) {
    label_data <- label_data %>%
      mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6) %>%
      filter(area_km2 >= min_area)
  }
  
  # Skapa label-text
  # VARFÖR formatering här?: Görs en gång istället för i varje geom
  label_data <- label_data %>%
    mutate(
      label_text = paste0(
        prefix,
        format(round(.data[[value_col]], decimals), 
               big.mark = " ",      # Tusentalsavskiljare
               decimal.mark = ",",  # Svenskt decimaltecken
               nsmall = decimals),
        suffix
      )
    )
  
  # Beräkna centroider för placering
  # VARFÖR centroider?: Labels placeras i mitten av varje polygon
  label_data <- label_data %>%
    mutate(
      centroid = st_centroid(st_geometry(.)),
      x = st_coordinates(centroid)[, 1],
      y = st_coordinates(centroid)[, 2]
    )
  
  # Auto-color baserat på bakgrund eller värde
  # VARFÖR tre alternativ?: Flexibilitet för olika användningsfall
  if (color == "auto") {
    # TODO: Implementera smart kontrast-beräkning baserat på kartfärg
    # För nu: använd svart som default
    color <- "black"
  } else if (color == "conditional") {
    # Conditional: Röd för negativa, svart för positiva/noll
    # VARFÖR conditional?: Tydligt visa negativa vs positiva förändringar
    label_data <- label_data %>%
      mutate(label_color = ifelse(.data[[value_col]] < 0, "#d32f2f", "black"))
    color <- "conditional"  # Flagga för render-funktion
  } else {
    # Använd specificerad färg
    # (color är redan satt)
  }
  
  # Returnera lista som passar extra_layers
  # VARFÖR denna struktur?: Matchar create_map's extra_layers förväntan
  return(list(
    type = "value_labels",  # För intern identifiering
    data = label_data,
    value_col = value_col,
    size = size,
    color = color,
    fontface = fontface,
    alpha = alpha,
    layer_order = layer_order,
    
    # Rendering-funktion
    # VARFÖR funktion?: Ger full kontroll över hur lagret ritas
    render = function(layer_data) {
      # Kolla om conditional färg används
      # VARFÖR kolla här?: color variabeln kan vara "conditional" flagga
      if (color == "conditional") {
        # Använd label_color kolumnen som skapades ovan
        list(
          geom_text(
            data = layer_data,
            aes(x = x, y = y, label = label_text, color = label_color),
            size = size,
            fontface = fontface,
            alpha = alpha,
            inherit.aes = FALSE
          ),
          scale_color_identity()  # VIKTIGT: Låt färgerna vara exakt som i data
        )
      } else {
        # Standard färg
        geom_text(
          data = layer_data,
          aes(x = x, y = y, label = label_text),
          size = size,
          color = color,
          fontface = fontface,
          alpha = alpha,
          inherit.aes = FALSE
        )
      }
    }
  ))
}


# 2. HIGHLIGHT LABELS =========================================================

#' Lägg till highlight-etiketter för utvalda områden
#' 
#' VARFÖR add_highlight_labels?: Peka ut specifika områden med både namn och värde,
#' användbart för att visa extremvärden eller viktiga områden.
#' 
#' @param data sf-objekt med områden
#' @param label_col Kolumn med namn att visa
#' @param value_col Kolumn med värden att visa
#' @param areas Manuell lista med områden (NULL = använd top_n/bottom_n)
#' @param top_n Antal högsta värden att highlighta
#' @param bottom_n Antal lägsta värden att highlighta
#' @param format Format för label, t.ex. "{label}: {value}" eller "{label}\n{value}"
#' @param decimals Antal decimaler för värdet
#' @param suffix Enhet efter värdet (t.ex. "%")
#' @param box Visa box runt text? (default: TRUE)
#' @param box_fill Boxfärg (default: "white")
#' @param box_color Border-färg (default: "black")
#' @param box_alpha Transparens för box (default: 0.9)
#' @param text_size Textstorlek (default: 3.5)
#' @param text_color Textfärg ("conditional" = röd för negativa, svart för positiva, eller färg)
#' @param segment Visa linje från label till område? (default: TRUE)
#' @param segment_color Linjefärg (default: "gray50")
#' @param nudge_x Flytta labels horisontellt
#' @param nudge_y Flytta labels vertikalt
#' @param force ggrepel force-parameter (default: 2)
#' @param layer_order Position i lagerordning (default: 100, högst upp)
#' 
#' @return Lista för extra_layers
#' 
#' @examples
#' # Visa top 2 och bottom 2
#' add_highlight_labels(
#'   data = kommuner,
#'   label_col = "kommun",
#'   value_col = "andel_utrikes",
#'   top_n = 2,
#'   bottom_n = 2,
#'   format = "{label}: {value}%"
#' )
#' 
#' @export
add_highlight_labels <- function(data,
                                 label_col,
                                 value_col,
                                 areas = NULL,
                                 top_n = NULL,
                                 bottom_n = NULL,
                                 format = "{label}: {value}",
                                 decimals = 1,
                                 suffix = "",
                                 box = TRUE,
                                 box_fill = "white",
                                 box_color = "black",
                                 box_alpha = 0.9,
                                 text_size = 3.5,
                                 text_color = "black",
                                 segment = TRUE,
                                 segment_color = "gray50",
                                 nudge_x = 0,
                                 nudge_y = 0,
                                 force = 2,
                                 layer_order = 100) {
  
  # Validering
  if (!inherits(data, "sf")) stop("data måste vara ett sf-objekt")
  if (!label_col %in% names(data)) stop("Kolumnen '", label_col, "' finns inte")
  if (!value_col %in% names(data)) stop("Kolumnen '", value_col, "' finns inte")
  
  # Välj områden
  # VARFÖR tre sätt?: Flexibilitet - manuellt, top/bottom, eller kombinerat
  label_data <- data
  
  if (!is.null(areas)) {
    # Manuell lista
    # VARFÖR before/after count?: För debugging - se om filtrering fungerade
    n_before <- nrow(label_data)
    label_data <- label_data %>% filter(.data[[label_col]] %in% areas)
    n_after <- nrow(label_data)
    
    if (n_after == 0) {
      warning("Inga områden matchade 'areas' i kolumn '", label_col, "'.\n",
              "  Angivna områden: ", paste(areas, collapse = ", "), "\n",
              "  Tillgängliga i data: ", paste(head(unique(data[[label_col]]), 10), collapse = ", "))
      return(NULL)
    }
    message("  Filtrerade från ", n_before, " till ", n_after, " områden (areas)")
  } else if (!is.null(top_n) || !is.null(bottom_n)) {
    # Top och/eller bottom
    selected <- data.frame()
    
    if (!is.null(top_n)) {
      top <- data %>%
        st_drop_geometry() %>%
        arrange(desc(.data[[value_col]])) %>%
        head(top_n)
      selected <- bind_rows(selected, top)
    }
    
    if (!is.null(bottom_n)) {
      bottom <- data %>%
        st_drop_geometry() %>%
        arrange(.data[[value_col]]) %>%
        head(bottom_n)
      selected <- bind_rows(selected, bottom)
    }
    
    # Merge tillbaka med geometri
    label_data <- data %>% 
      filter(.data[[label_col]] %in% selected[[label_col]])
  } else {
    warning("Inga områden specificerade (areas, top_n, eller bottom_n). Visar inga labels.")
    label_data <- label_data %>% slice(0)
  }
  
  if (nrow(label_data) == 0) {
    warning("Inga områden att highlighta")
    return(NULL)
  }
  
  # Skapa label-text enligt format
  # VARFÖR rowwise?: gsub behöver appliceras rad-för-rad på varje label/value
  label_data <- label_data %>%
    rowwise() %>%
    mutate(
      formatted_value = format(round(.data[[value_col]], decimals),
                               big.mark = " ",
                               decimal.mark = ",",
                               nsmall = decimals),
      label_text = gsub("\\{label\\}", as.character(.data[[label_col]]), format),
      label_text = gsub("\\{value\\}", paste0(formatted_value, suffix), label_text)
    ) %>%
    ungroup()  # VIKTIGT: Ta bort rowwise efter mutate
  
  # Beräkna centroider
  label_data <- label_data %>%
    mutate(
      centroid = st_centroid(st_geometry(.)),
      x = st_coordinates(centroid)[, 1],
      y = st_coordinates(centroid)[, 2]
    )
  
  # Conditional färg för negativa/positiva värden
  # VARFÖR här?: Efter alla andra mutates är klara
  if (text_color == "conditional") {
    label_data <- label_data %>%
      mutate(label_color = ifelse(.data[[value_col]] < 0, "#d32f2f", "black"))
  }
  
  # Returnera lista för extra_layers
  return(list(
    type = "highlight_labels",
    data = label_data,
    box = box,
    box_fill = box_fill,
    box_color = box_color,
    box_alpha = box_alpha,
    text_size = text_size,
    text_color = text_color,
    segment = segment,
    segment_color = segment_color,
    nudge_x = nudge_x,
    nudge_y = nudge_y,
    force = force,
    layer_order = layer_order,
    
    # Rendering
    render = function(layer_data) {
      # Kolla conditional färg
      use_conditional <- text_color == "conditional"
      
      if (box) {
        # Med box
        if (use_conditional) {
          list(
            geom_label_repel(
              data = layer_data,
              aes(x = x, y = y, label = label_text, color = label_color),
              size = text_size,
              fill = box_fill,
              label.r = unit(0.2, "lines"),
              label.padding = unit(0.25, "lines"),
              box.padding = unit(0.5, "lines"),
              point.padding = unit(0.5, "lines"),
              segment.color = if (segment) segment_color else NA,
              nudge_x = nudge_x,
              nudge_y = nudge_y,
              force = force,
              alpha = box_alpha,
              inherit.aes = FALSE
            ),
            scale_color_identity()
          )
        } else {
          geom_label_repel(
            data = layer_data,
            aes(x = x, y = y, label = label_text),
            size = text_size,
            color = text_color,
            fill = box_fill,
            label.r = unit(0.2, "lines"),
            label.padding = unit(0.25, "lines"),
            box.padding = unit(0.5, "lines"),
            point.padding = unit(0.5, "lines"),
            segment.color = if (segment) segment_color else NA,
            nudge_x = nudge_x,
            nudge_y = nudge_y,
            force = force,
            alpha = box_alpha,
            inherit.aes = FALSE
          )
        }
      } else {
        # Utan box
        if (use_conditional) {
          list(
            geom_text_repel(
              data = layer_data,
              aes(x = x, y = y, label = label_text, color = label_color),
              size = text_size,
              segment.color = if (segment) segment_color else NA,
              nudge_x = nudge_x,
              nudge_y = nudge_y,
              force = force,
              inherit.aes = FALSE
            ),
            scale_color_identity()
          )
        } else {
          geom_text_repel(
            data = layer_data,
            aes(x = x, y = y, label = label_text),
            size = text_size,
            color = text_color,
            segment.color = if (segment) segment_color else NA,
            nudge_x = nudge_x,
            nudge_y = nudge_y,
            force = force,
            inherit.aes = FALSE
          )
        }
      }
    }
  ))
}


# 3. AREA LABELS ==============================================================

#' Lägg till områdesnamn-etiketter
#' 
#' VARFÖR add_area_labels?: Visa namn på större områden (t.ex. stadsdelar)
#' ovanpå detaljerad karta (t.ex. basområden).
#' 
#' @param data sf-objekt med områden
#' @param label_col Kolumn med namn
#' @param areas Vilka områden (NULL = alla)
#' @param style "box", "plain", "shadow"
#' @param size Textstorlek (default: 3.5)
#' @param color Textfärg (default: "black")
#' @param fill Boxfärg för style="box" (default: "white")
#' @param alpha Transparens (default: 0.9)
#' @param nudge_x Flytta horisontellt
#' @param nudge_y Flytta vertikalt
#' @param force ggrepel force (default: 1)
#' @param layer_order Position (default: 100)
#' 
#' @return Lista för extra_layers
#' 
#' @examples
#' # Visa stadsdelsnamn över basområdeskarta
#' add_area_labels(
#'   data = stadsdelar,
#'   label_col = "namn",
#'   style = "box"
#' )
#' 
#' @export
add_area_labels <- function(data,
                            label_col,
                            areas = NULL,
                            style = "box",
                            size = 3.5,
                            color = "black",
                            fill = "white",
                            alpha = 0.9,
                            nudge_x = 0,
                            nudge_y = 0,
                            force = 1,
                            layer_order = 100) {
  
  # Validering
  if (!inherits(data, "sf")) stop("data måste vara ett sf-objekt")
  if (!label_col %in% names(data)) stop("Kolumnen '", label_col, "' finns inte")
  
  # Filtrera områden
  label_data <- data
  if (!is.null(areas)) {
    label_data <- label_data %>% filter(.data[[label_col]] %in% areas)
  }
  
  # Centroider
  label_data <- label_data %>%
    mutate(
      centroid = st_centroid(st_geometry(.)),
      x = st_coordinates(centroid)[, 1],
      y = st_coordinates(centroid)[, 2],
      label_text = .data[[label_col]]
    )
  
  # Returnera lista
  return(list(
    type = "area_labels",
    data = label_data,
    style = style,
    size = size,
    color = color,
    fill = fill,
    alpha = alpha,
    nudge_x = nudge_x,
    nudge_y = nudge_y,
    force = force,
    layer_order = layer_order,
    
    # Rendering
    render = function(layer_data) {
      if (style == "box") {
        geom_label_repel(
          data = layer_data,
          aes(x = x, y = y, label = label_text),
          size = size,
          color = color,
          fill = fill,
          alpha = alpha,
          label.r = unit(0.15, "lines"),
          label.padding = unit(0.2, "lines"),
          box.padding = unit(0.3, "lines"),
          nudge_x = nudge_x,
          nudge_y = nudge_y,
          force = force,
          inherit.aes = FALSE
        )
      } else if (style == "shadow") {
        list(
          geom_text_repel(
            data = layer_data,
            aes(x = x, y = y, label = label_text),
            size = size,
            color = "white",
            nudge_x = nudge_x + 0.001,
            nudge_y = nudge_y - 0.001,
            force = force,
            inherit.aes = FALSE
          ),
          geom_text_repel(
            data = layer_data,
            aes(x = x, y = y, label = label_text),
            size = size,
            color = color,
            nudge_x = nudge_x,
            nudge_y = nudge_y,
            force = force,
            inherit.aes = FALSE
          )
        )
      } else {
        # plain
        geom_text_repel(
          data = layer_data,
          aes(x = x, y = y, label = label_text),
          size = size,
          color = color,
          nudge_x = nudge_x,
          nudge_y = nudge_y,
          force = force,
          inherit.aes = FALSE
        )
      }
    }
  ))
}
