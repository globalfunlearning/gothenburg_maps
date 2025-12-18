# =============================================================================
# LEGEND-KONFIGURATION FÖR TEMATISKA KARTOR
# =============================================================================


# 1. POSITION =================================================================

#' Konfigurera legend-position
#'
#' Sätter var legenden ska placeras på kartan. Returnerar theme()-objekt
#' som kan läggas till direkt med +.
#'
#' VARFÖR denna funktion?: ggplot2 har krånglig legend-positionering,
#' denna gör det enkelt med presets och auto-orientering.
#'
#' POSITION PRESETS:
#' - "right": Höger sida (ggplot2 default)
#' - "bottom": Botten
#' - "top": Topp
#' - "left": Vänster sida
#' - "topleft": Övre vänster hörn (bra för Göteborg-kartor)
#' - "topright": Övre höger hörn
#' - "bottomleft": Nedre vänster hörn
#' - "bottomright": Nedre höger hörn
#' - "none": Dölj legend
#' - c(x, y): Exakta koordinater (0-1, där 0,0 = nedre vänster)
#'
#' @param position Position (se ovan)
#' @param orientation "vertical", "horizontal", eller NULL för auto
#' @param nrow Antal rader för horisontell legend (NULL = auto, 1 = alla i en rad)
#' @param ncol Antal kolumner för vertikal legend (NULL = auto)
#'
#' @return theme()-objekt att lägga till på ggplot med +
#'
#' @examples
#' # Enkel användning
#' ggplot(data, aes(fill = var)) +
#'   geom_sf() +
#'   configure_legend_position("topleft")
#'
#' # Med orientering
#' configure_legend_position("bottom", orientation = "horizontal")
#'
#' # Custom position
#' configure_legend_position(c(0.15, 0.85))
#'
#' @export
configure_legend_position <- function(position = "right",
                                      orientation = NULL,
                                      nrow = NULL,
                                      ncol = NULL) {
  
  # STEG 1: Översätt presets till koordinater ----
  # VARFÖR: Ger användaren enkla namn istället för koordinater
  
  position_converted <- switch(
    as.character(position[1]),
    
    # Standard ggplot2
    "right" = "right",
    "left" = "left", 
    "top" = "top",
    "bottom" = "bottom",
    "none" = "none",
    
    # Kartor (inside plot area)
    "topleft" = c(0.02, 0.98),
    "topright" = c(0.98, 0.98),
    "bottomleft" = c(0.02, 0.02),
    "bottomright" = c(0.98, 0.02),
    
    # Numeric - använd direkt
    if (is.numeric(position) && length(position) == 2) {
      position
    } else {
      stop(
        "Okänd position: '", position, "'\n",
        "Välj: right, left, top, bottom, none,\n",
        "      topleft, topright, bottomleft, bottomright,\n",
        "      eller c(x, y) koordinater"
      )
    }
  )
  
  # STEG 2: Auto-orientering ----
  # VARFÖR: Horisontell legend passar bättre längst upp/ner,
  # vertikal passar bättre på sidorna
  
  if (is.null(orientation)) {
    if (is.character(position_converted)) {
      orientation <- if (position_converted %in% c("top", "bottom")) {
        "horizontal"
      } else {
        "vertical"
      }
    } else {
      # Numeric position - gissa baserat på y-koordinat
      y <- position_converted[2]
      orientation <- if (y > 0.8 || y < 0.2) "horizontal" else "vertical"
    }
  }
  
  # STEG 3: Bestäm justification för inside plot ----
  # VARFÖR justification?: När legend är inside (numeric position),
  # måste vi säga hur den ska justeras relativt punkten
  
  if (is.numeric(position_converted)) {
    x <- position_converted[1]
    y <- position_converted[2]
    
    just_x <- if (x < 0.3) 0 else if (x > 0.7) 1 else 0.5
    just_y <- if (y < 0.3) 0 else if (y > 0.7) 1 else 0.5
    
    justification <- c(just_x, just_y)
  } else {
    justification <- NULL
  }
  
  # STEG 4: Auto-sätt nrow för horisontell ----
  # VARFÖR nrow = 1?: Alla färgrutor i en rad för horisontell legend
  
  if (orientation == "horizontal" && is.null(nrow)) {
    nrow <- 1  # En rad = alla färgrutor bredvid varandra
  }
  
  # STEG 5: Bygg theme ----
  # VARFÖR returnera theme()?: Funkar direkt med + i ggplot2
  
  theme_list <- list(
    legend.position = position_converted,
    legend.direction = orientation
  )
  
  if (!is.null(justification)) {
    theme_list$legend.justification <- justification
  }
  
  # VIKTIGT: För horisontell legend - byline = FALSE gör att labels hamnar UNDER
  # VARFÖR byrow?: Färgrutorna fylls på rad för rad (horisontellt)
  if (orientation == "horizontal") {
    theme_list$legend.byrow = TRUE
  }
  
  return(do.call(theme, theme_list))
}


# 2. STORLEK OCH UTSEENDE =====================================================

#' Konfigurera legend-utseende
#'
#' Sätter storlek, färg och spacing på legend enligt Göteborgs grafiska profil.
#' Returnerar theme()-objekt som kan läggas till direkt med +.
#'
#' VARFÖR denna funktion?: Göteborgs grafiska profil kräver specifika storlekar
#' och spacing. Denna funktion ger konsekventa legends.
#'
#' @param size "small", "medium", "large"
#' @param spacing "tight", "normal", "loose"
#' @param show_title Visa legend-titel? (default: TRUE)
#' @param key_width Bredd på färgnycklar i mm (NULL = auto)
#' @param key_height Höjd på färgnycklar i mm (NULL = auto)
#' @param text_size Text-storlek i points (NULL = auto)
#' @param title_size Titel-storlek i points (NULL = auto)
#'
#' @return theme()-objekt att lägga till på ggplot med +
#'
#' @examples
#' # Medium (default)
#' ggplot(data, aes(fill = var)) +
#'   geom_sf() +
#'   configure_legend_style()
#'
#' # Stor legend
#' configure_legend_style(size = "large")
#'
#' # Kompakt utan titel
#' configure_legend_style(size = "small", spacing = "tight", show_title = FALSE)
#'
#' @export
configure_legend_style <- function(size = "medium",
                                   spacing = "normal",
                                   show_title = TRUE,
                                   key_width = NULL,
                                   key_height = NULL,
                                   text_size = NULL,
                                   title_size = NULL,
                                   background = "white") {  # NYTT: Bakgrundsfärg
  
  # STEG 1: Bestäm text-storlekar ----
  # VARFÖR dessa storlekar?: Testade för läsbarhet i både webb och print
  
  text_sizes <- list(
    small = 8,
    medium = 10,
    large = 12
  )
  
  if (is.null(text_size)) {
    if (!size %in% names(text_sizes)) {
      stop("size måste vara 'small', 'medium' eller 'large'")
    }
    text_size <- text_sizes[[size]]
  }
  
  if (is.null(title_size)) {
    title_size <- text_size + 1
  }
  
  # STEG 2: Bestäm key-storlekar ----
  # VARFÖR kvadratiska?: Ser professionella ut och är lättare att läsa
  # än avlånga rektangulära rutor för både horisontell och vertikal legend
  
  key_sizes <- list(
    small = c(width = 5, height = 5),    # Kvadratisk - liten
    medium = c(width = 6, height = 6),   # Kvadratisk - medium  
    large = c(width = 7, height = 7)     # Kvadratisk - stor
  )
  
  if (is.null(key_width) || is.null(key_height)) {
    if (!size %in% names(key_sizes)) {
      stop("size måste vara 'small', 'medium' eller 'large'")
    }
    if (is.null(key_width)) key_width <- key_sizes[[size]]["width"]
    if (is.null(key_height)) key_height <- key_sizes[[size]]["height"]
  }
  
  # STEG 3: Bestäm spacing ----
  # VARFÖR spacing?: Påverkar hur tight legenden är
  
  spacing_values <- list(
    tight = 0.2,
    normal = 0.3,
    loose = 0.5
  )
  
  if (!spacing %in% names(spacing_values)) {
    stop("spacing måste vara 'tight', 'normal' eller 'loose'")
  }
  
  spacing_val <- spacing_values[[spacing]]
  
  # STEG 4: Bygg theme ----
  # VARFÖR returnera theme()?: Funkar direkt med + i ggplot2
  
  theme(
    # Text
    legend.text = element_text(
      size = text_size,
      color = "black"
    ),
    
    legend.title = if (show_title) {
      element_text(
        size = title_size,
        color = "black",
        face = "bold"
      )
    } else {
      element_blank()
    },
    
    # Keys (färgrutorna)
    # VARFÖR tunn linje?: Professionellt utan att ta för mycket uppmärksamhet
    legend.key = element_rect(
      fill = NA, 
      color = "gray80",      # Tunn ljusgrå linje
      linewidth = 0.3        # Mycket tunn linje
    ),
    
    # VIKTIGT: Fixerad storlek för alla keys
    # VARFÖR samma width/height?: Gör alla rutor EXAKT lika stora
    legend.key.width = unit(key_width, "mm"),
    legend.key.height = unit(key_height, "mm"),
    legend.key.size = unit(key_width, "mm"),  # Extra säkerhet för fixerad storlek
    
    # Bakgrund
    # VARFÖR konfigurerbar?: Transparent eller matchande bakgrund för olika designs
    legend.background = element_rect(fill = background, color = NA),
    
    # Spacing
    # VARFÖR mindre x-spacing?: Horisontell legend behöver mindre mellanrum mellan rutor
    legend.spacing.x = unit(spacing_val * 0.3, "lines"),  # Mindre x-spacing för horisontell
    legend.spacing.y = unit(spacing_val, "lines"),
    legend.box.spacing = unit(spacing_val, "lines"),
    
    # Margin
    legend.box.margin = margin(6, 6, 6, 6),
    
    # VIKTIGT för horisontell legend: labels UNDER färgrutor
    legend.text.align = 0,           # Vänsterjusterad text
    legend.title.align = 0           # Vänsterjusterad titel
  )
}


# 3. KOMBINERAD FUNKTION (WRAPPER) ============================================

#' Konfigurera komplett legend
#'
#' Kombinerar position, style och guide-settings i en funktion för bekvämlighet.
#' Returnerar kombinerat objekt som kan läggas till direkt med +.
#'
#' VARFÖR wrapper?: Enklare att använda när du vill sätta både position och style.
#' Men du kan också använda configure_legend_position() och configure_legend_style()
#' separat för mer kontroll.
#'
#' @param position Position (se configure_legend_position)
#' @param orientation "vertical", "horizontal", NULL för auto
#' @param size "small", "medium", "large"
#' @param spacing "tight", "normal", "loose"
#' @param show_title Visa legend-titel? (default: TRUE)
#' @param key_width Bredd på färgrutor i mm (NULL = auto baserat på size)
#' @param key_height Höjd på färgrutor i mm (NULL = auto baserat på size)
#' @param title_size Textstorlek för legend-titel i pt (NULL = auto)
#' @param text_size Textstorlek för legend-text i pt (NULL = auto)
#' @param ... Ytterligare parametrar till configure_legend_style()
#'
#' @return Kombinerat objekt (theme + guides) att lägga till på ggplot med +
#'
#' @examples
#' # Enklast - bara position
#' ggplot(data, aes(fill = var)) +
#'   geom_sf() +
#'   configure_legend("topleft")
#'
#' # Med storlek och textstorlekar
#' configure_legend("bottomright", 
#'                  size = "large",
#'                  title_size = 14,
#'                  text_size = 11)
#'
#' # Komplett horisontell med custom allt
#' configure_legend(
#'   position = "bottom",
#'   size = "medium",
#'   key_width = 45,     # mm - bred nog för lång text
#'   key_height = 6,     # mm - låg rektangulär form
#'   title_size = 13,    # pt - större titel
#'   text_size = 10      # pt - större text
#' )
#'
#' @export
configure_legend <- function(position = "right",
                             orientation = NULL,
                             size = "medium",
                             spacing = "normal",
                             show_title = TRUE,
                             key_width = NULL,
                             key_height = NULL,
                             title_size = NULL,
                             text_size = NULL,
                             background = "white",  # NYTT: Bakgrundsfärg
                             ...) {
  
  # Konfigurera position och style
  pos_theme <- configure_legend_position(position, orientation)
  style_theme <- configure_legend_style(
    size = size, 
    spacing = spacing, 
    show_title = show_title,
    key_width = key_width,      # Skicka vidare custom width
    key_height = key_height,    # Skicka vidare custom height
    title_size = title_size,    # Skicka vidare custom title size
    text_size = text_size,      # Skicka vidare custom text size
    background = background,    # NYTT: Bakgrundsfärg
    ...
  )
  
  # Bestäm orientering för guides
  # VARFÖR behövs detta?: För att veta om vi ska ha labels under (horisontell) eller vid sidan (vertikal)
  if (is.null(orientation)) {
    if (is.character(position)) {
      orientation <- if (position %in% c("top", "bottom")) "horizontal" else "vertical"
    } else {
      orientation <- "vertical"
    }
  }
  
  # Konfigurera guides baserat på orientering
  # VARFÖR guides?: Kontrollerar hur labels placeras (under färgrutor för horisontell)
  # VARFÖR title.position = "top"?: Titel ska vara ÖVER legenden vid horisontell layout
  # VARFÖR override.aes?: Gör alla färgrutor EXAKT lika stora (ignore textbredd)
  
  if (orientation == "horizontal") {
    guide_config <- guides(
      fill = guide_legend(
        label.position = "bottom",      # Labels UNDER färgrutorna
        byrow = TRUE,                   # Fyll horisontellt
        title.position = "top",         # Titel ÖVER legenden
        override.aes = list(size = 1)  # Gör alla keys lika stora (fixad storlek)
      )
    )
  } else {
    guide_config <- guides(
      fill = guide_legend(
        label.position = "right",
        byrow = TRUE
      )
    )
  }
  
  # Kombinera allt
  # VARFÖR lista?: ggplot kan ta emot lista och applicera alla delar
  return(list(pos_theme, style_theme, guide_config))
}


# 4. LABEL-FORMATERING ========================================================

#' Formatera legend-labels med svenska stil
#'
#' Formaterar numeriska värden med mellanslag som tusentalsavgränsare
#' och komma som decimaltecken (svensk standard).
#'
#' VARFÖR denna funktion?: ggplot2 använder amerikansk formatering default.
#' Göteborgs kartor ska ha svensk stil.
#'
#' @param values Numeriska värden att formatera
#' @param decimals Antal decimaler (default: 0)
#' @param unit Enhet att lägga till (t.ex. "%", "kr", "inv/km²")
#' @param big_mark Tusentalsavgränsare (default: " ")
#' @param decimal_mark Decimaltecken (default: ",")
#'
#' @return Character-vektor med formaterade labels
#'
#' @examples
#' # Grundläggande
#' format_legend_labels(c(1000, 2500, 5000))
#' # → "1 000", "2 500", "5 000"
#'
#' # Med enhet
#' format_legend_labels(c(10.5, 25.3), decimals = 1, unit = "%")
#' # → "10,5 %", "25,3 %"
#'
#' # Valuta
#' format_legend_labels(c(100000, 250000), unit = "kr")
#' # → "100 000 kr", "250 000 kr"
#'
#' @export
format_legend_labels <- function(values,
                                 decimals = 0,
                                 unit = NULL,
                                 big_mark = " ",
                                 decimal_mark = ",") {
  
  if (!is.numeric(values)) {
    stop("values måste vara numerisk")
  }
  
  # STEG 1: Avrunda ----
  rounded <- round(values, decimals)
  
  # STEG 2: Formatera med svensk stil ----
  # VARFÖR format()?: Ger kontroll över tusentalsavgränsare och decimaltecken
  
  formatted <- format(
    rounded,
    nsmall = decimals,
    big.mark = big_mark,
    decimal.mark = decimal_mark,
    trim = TRUE,
    scientific = FALSE
  )
  
  # STEG 3: Ta bort extra mellanslag ----
  formatted <- trimws(formatted)
  
  # STEG 4: Lägg till enhet ----
  if (!is.null(unit)) {
    formatted <- paste(formatted, unit)
  }
  
  # STEG 5: Hantera NA ----
  formatted[is.na(values)] <- NA_character_
  
  return(formatted)
}


# 5. HJÄLPFUNKTIONER ==========================================================

#' Dölj legend
#'
#' Snabb funktion för att dölja legend helt.
#' Returnerar theme()-objekt.
#'
#' @return theme()-objekt
#'
#' @examples
#' ggplot(data, aes(fill = var)) +
#'   geom_sf() +
#'   hide_legend()
#'
#' @export
hide_legend <- function() {
  theme(legend.position = "none")
}