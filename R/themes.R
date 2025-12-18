# =============================================================================
# THEME_GOTHENBURG_MAP() - Enkelt och smidigt tema för kartor
# =============================================================================
# DEFAULT: Horisontell legend längst ner, en rad, smala avlånga rutor
#
# ANVÄNDNING:
# + theme_gothenburg_map()  # Perfekt default
# + theme_gothenburg_map(legend_direction = "vertical")  # Vertikal med kvadratiska rutor
# + theme_gothenburg_map(legend_position = c(0.98, 0.98))  # Flytta till hörn
# =============================================================================

library(ggplot2)

#' Tema för kartor enligt Göteborgs Stads grafiska profil
#'
#' DEFAULT: Horisontell legend längst ner med smala avlånga rutor i en rad.
#' 
#' @param base_family Typsnitt (default: "")
#' @param base_size Textstorlek (default: 11)
#' @param plot_title_size Titel (default: 16)
#' @param plot_subtitle_size Undertitel (default: 11)
#' @param plot_caption_size Källa (default: 9)
#' @param legend_position Position ("bottom", "right", eller c(x,y))
#' @param legend_direction "horizontal" (default) eller "vertical"
#' @param legend_title_size Legend-titel (default: 10)
#' @param legend_text_size Legend-text (default: 9)
#' @param legend_key_width Bredd färgrutor i mm (default: NULL = auto)
#' @param legend_key_height Höjd färgrutor i mm (default: NULL = auto)
#' @param plot_margin Marginaler i mm (default: c(15, 10, 15, 10))
#' @param panel_background Bakgrund (default: "white")
#'
#' @return ggplot2-tema
#' @export
theme_gothenburg_map <- function(
    base_family = "",
    base_size = 11,
    plot_title_size = 16,
    plot_subtitle_size = 11,
    plot_caption_size = 9,
    legend_position = "bottom",
    legend_direction = "horizontal",
    legend_title_size = 10,
    legend_text_size = 9,
    legend_key_width = NULL,
    legend_key_height = NULL,
    plot_margin = c(15, 10, 15, 10),
    panel_background = "white"
) {
  
  # Auto-font
  if (base_family == "") {
    base_family <- get_best_font()
  }
  
  # Konvertera named positions
  legend_pos_final <- legend_position
  legend_just <- "center"
  
  if (is.character(legend_position) && length(legend_position) == 1) {
    if (legend_position == "topleft") {
      legend_pos_final <- c(0.02, 0.98)
      legend_just <- c(0, 1)
    } else if (legend_position == "topright") {
      legend_pos_final <- c(0.98, 0.98)
      legend_just <- c(1, 1)
    } else if (legend_position == "bottomleft") {
      legend_pos_final <- c(0.02, 0.02)
      legend_just <- c(0, 0)
    } else if (legend_position == "bottomright") {
      legend_pos_final <- c(0.98, 0.02)
      legend_just <- c(1, 0)
    }
  }
  
  # Auto-sätt key storlekar
  # HORISONTELL (default): Smala avlånga rutor 15x4mm
  # VERTIKAL: Kvadratiska rutor 6x6mm
  if (is.null(legend_key_width)) {
    legend_key_width <- if (legend_direction == "horizontal") 15 else 6
  }
  if (is.null(legend_key_height)) {
    legend_key_height <- if (legend_direction == "horizontal") 2.5 else 6
  }
  
  # Bygg tema
  tema <- theme_void(base_family = base_family, base_size = base_size) +
    theme(
      # TEXT - Svart
      text = element_text(family = base_family, color = "#000000"),
      
      # TITEL - Bold, vänster
      plot.title = element_text(
        size = plot_title_size,
        face = "bold",
        hjust = 0,
        margin = margin(b = 5)
      ),
      
      # UNDERTITEL - Plain, vänster
      plot.subtitle = element_text(
        size = plot_subtitle_size,
        hjust = 0,
        margin = margin(b = 10)
      ),
      
      # KÄLLA - Liten, höger
      plot.caption = element_text(
        size = plot_caption_size,
        hjust = 1
      ),
      
      # MARGINALER
      plot.margin = margin(
        t = plot_margin[1],
        r = plot_margin[2],
        b = plot_margin[3],
        l = plot_margin[4],
        unit = "mm"
      ),
      
      # LEGEND TITEL - Bold, över färgrutor
      legend.title = element_text(
        size = legend_title_size,
        face = "bold"
      ),
      
      # LEGEND TEXT
      legend.text = element_text(
        size = legend_text_size
      ),
      
      # LEGEND POSITION
      legend.position = legend_pos_final,
      legend.justification = legend_just,
      legend.direction = legend_direction,
      
      # LEGEND UTSEENDE - Transparent bakgrund
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.key = element_rect(fill = NA, color = NA),
      legend.key.width = unit(legend_key_width, "mm"),
      legend.key.height = unit(legend_key_height, "mm"),
      legend.key.spacing.x = unit(1, "mm"),
      legend.key.spacing.y = unit(1, "mm"),
      
      # BAKGRUND
      panel.background = element_rect(fill = panel_background, color = NA),
      plot.background = element_rect(fill = panel_background, color = NA)
    )
  
  # VIKTIGT: För horisontell legend, lägg till guides för en rad + text under
  if (legend_direction == "horizontal") {
    return(list(
      tema,
      guides(fill = guide_legend(
        nrow = 1,
        byrow = TRUE,
        title.position = "top",
        label.position = "bottom"
      ))
    ))
  }
  
  # För vertikal, bara returnera tema (text ligger automatiskt till höger)
  return(tema)
}


#' Hitta bästa typsnitt
get_best_font <- function() {
  fonts <- c("Open Sans", "Arial", "Helvetica", "sans")
  
  if (requireNamespace("systemfonts", quietly = TRUE)) {
    available <- systemfonts::system_fonts()$family
    for (font in fonts) {
      if (any(grepl(font, available, ignore.case = TRUE))) {
        return(font)
      }
    }
  }
  
  return("")
}


#' Kontrollera Open Sans
#' @export
check_open_sans <- function() {
  if (requireNamespace("systemfonts", quietly = TRUE)) {
    fonts <- systemfonts::system_fonts()
    has_it <- any(grepl("Open Sans", fonts$family, ignore.case = TRUE))
    
    if (!has_it) {
      message("VARNING: Open Sans inte installerat. Använder fallback.")
    }
    return(has_it)
  }
  return(NA)
}

