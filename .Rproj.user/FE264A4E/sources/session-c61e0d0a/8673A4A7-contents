# Göteborgs Stads Färgsystem - ggplot2 Integration
# Version: 1.1
# Skapad: 2025-10-31
# Uppdaterad: 2025-11-23
#
# Detta skript innehåller ggplot2 scale-funktioner för Göteborgs Stads
# grafiska profil.
#
# KRÄVER: 
# - colors_gothenburg.R (måste laddas först)
# - ggplot2
#
# ANVÄNDNING:
# source("colors_gothenburg.R")
# source("ggplot_scales_gothenburg.R")
#
# ÄNDRINGSLOGG v1.1:
# - Förbättrad input-validering i alla scale-funktioner
# - Bättre felmeddelanden
# - Konsekventa parameterkontroller


# KONTROLLERA BEROENDEN =================================================

if (!exists("CATEGORICAL_PALETTES")) {
  stop(
    "colors_gothenburg.R måste laddas före ggplot_scales_gothenburg.R\n",
    "Kör: source('colors_gothenburg.R')"
  )
}

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Paketet 'ggplot2' krävs för ggplot_scales_gothenburg.R")
}


# 1. SEQUENTIAL SCALES ==================================================

#' ggplot2 scale för fill - Sequential
#'
#' Använd för numeriska värden med ordning (låg → hög)
#'
#' @param palette Namn på palett: "blue", "green", "yellow_red", "dark_blue", 
#'   "cyan", "yellow", "red", "pink", "purple", "yellow_green", "yellow_green_dark"
#' @param n Antal färger (NULL = automatisk från data)
#' @param direction 1 = normal ordning, -1 = omvänd ordning
#' @param discrete Tvinga diskret (TRUE) eller kontinuerlig (FALSE) skala (NULL = automatisk)
#' @param ... Övriga argument till scale_fill_gradientn eller scale_fill_manual
#' @return ggplot2 scale-objekt
#' @examples
#' # Kontinuerlig skala
#' ggplot(data, aes(fill = population)) + 
#'   geom_sf() + 
#'   scale_fill_gbg_sequential("blue")
#' 
#' # Diskret skala med 5 steg
#' ggplot(data, aes(fill = category)) + 
#'   geom_sf() + 
#'   scale_fill_gbg_sequential("green", n = 5, discrete = TRUE)
#' @export
scale_fill_gbg_sequential <- function(palette = "blue", 
                                      n = NULL, 
                                      direction = 1,
                                      discrete = NULL,
                                      ...) {
  
  # Validera palette
  valid_palettes <- names(SEQUENTIAL_PALETTES)
  if (!is.character(palette) || length(palette) != 1) {
    stop("'palette' måste vara en character string")
  }
  if (!palette %in% valid_palettes) {
    stop(
      "Ogiltig sequential palett: '", palette, "'\n",
      "Giltiga paletter: ", paste(valid_palettes, collapse = ", ")
    )
  }
  
  # Validera direction
  if (!is.numeric(direction) || length(direction) != 1 || !direction %in% c(-1, 1)) {
    stop("'direction' måste vara 1 (normal) eller -1 (omvänd)")
  }
  
  # Validera n om angivet
  if (!is.null(n)) {
    if (!is.numeric(n) || length(n) != 1 || n < 2 || n != as.integer(n)) {
      stop("'n' måste vara ett heltal >= 2")
    }
  }
  
  # Validera discrete om angivet
  if (!is.null(discrete) && !is.logical(discrete)) {
    stop("'discrete' måste vara TRUE, FALSE eller NULL")
  }
  
  # Hämta baspaletten
  base_colors <- gbg_palette("sequential", palette, n = NULL, direction = direction)
  
  # Om discrete = FALSE explicit, använd kontinuerlig
  if (!is.null(discrete) && discrete == FALSE) {
    colors <- gbg_palette("sequential", palette, n = n, direction = direction)
    return(ggplot2::scale_fill_gradientn(colors = colors, ...))
  }
  
  # Om discrete = TRUE explicit, använd diskret
  if (!is.null(discrete) && discrete == TRUE) {
    colors <- gbg_palette("sequential", palette, n = n, direction = direction)
    return(ggplot2::scale_fill_manual(values = colors, ...))
  }
  
  # Om n angivet, avgör baserat på n
  if (!is.null(n)) {
    colors <- gbg_palette("sequential", palette, n = n, direction = direction)
    if (n <= 10) {
      return(ggplot2::scale_fill_manual(values = colors, ...))
    } else {
      return(ggplot2::scale_fill_gradientn(colors = colors, ...))
    }
  }
  
  # Default: kontinuerlig skala (för numeriska värden)
  # Detta är det vanligaste användningsfallet
  ggplot2::scale_fill_gradientn(colors = base_colors, ...)
}

#' ggplot2 scale för color - Sequential
#'
#' Använd för numeriska värden med ordning (låg → hög)
#'
#' @param palette Namn på palett: "blue", "green", "yellow_red", "dark_blue", 
#'   "cyan", "yellow", "red", "pink", "purple", "yellow_green", "yellow_green_dark"
#' @param n Antal färger (NULL = automatisk från data)
#' @param direction 1 = normal ordning, -1 = omvänd ordning
#' @param discrete Tvinga diskret (TRUE) eller kontinuerlig (FALSE) skala (NULL = automatisk)
#' @param ... Övriga argument till scale_color_gradientn eller scale_color_manual
#' @return ggplot2 scale-objekt
#' @examples
#' # Kontinuerlig skala
#' ggplot(data, aes(x = year, y = value, color = temperature)) + 
#'   geom_line() + 
#'   scale_color_gbg_sequential("yellow_red")
#' @export
scale_color_gbg_sequential <- function(palette = "blue", 
                                       n = NULL, 
                                       direction = 1,
                                       discrete = NULL,
                                       ...) {
  
  # Validera palette
  valid_palettes <- names(SEQUENTIAL_PALETTES)
  if (!is.character(palette) || length(palette) != 1) {
    stop("'palette' måste vara en character string")
  }
  if (!palette %in% valid_palettes) {
    stop(
      "Ogiltig sequential palett: '", palette, "'\n",
      "Giltiga paletter: ", paste(valid_palettes, collapse = ", ")
    )
  }
  
  # Validera direction
  if (!is.numeric(direction) || length(direction) != 1 || !direction %in% c(-1, 1)) {
    stop("'direction' måste vara 1 (normal) eller -1 (omvänd)")
  }
  
  # Validera n om angivet
  if (!is.null(n)) {
    if (!is.numeric(n) || length(n) != 1 || n < 2 || n != as.integer(n)) {
      stop("'n' måste vara ett heltal >= 2")
    }
  }
  
  # Validera discrete om angivet
  if (!is.null(discrete) && !is.logical(discrete)) {
    stop("'discrete' måste vara TRUE, FALSE eller NULL")
  }
  
  # Hämta baspaletten
  base_colors <- gbg_palette("sequential", palette, n = NULL, direction = direction)
  
  # Om discrete = FALSE explicit, använd kontinuerlig
  if (!is.null(discrete) && discrete == FALSE) {
    colors <- gbg_palette("sequential", palette, n = n, direction = direction)
    return(ggplot2::scale_color_gradientn(colors = colors, ...))
  }
  
  # Om discrete = TRUE explicit, använd diskret
  if (!is.null(discrete) && discrete == TRUE) {
    colors <- gbg_palette("sequential", palette, n = n, direction = direction)
    return(ggplot2::scale_color_manual(values = colors, ...))
  }
  
  # Om n angivet, avgör baserat på n
  if (!is.null(n)) {
    colors <- gbg_palette("sequential", palette, n = n, direction = direction)
    if (n <= 10) {
      return(ggplot2::scale_color_manual(values = colors, ...))
    } else {
      return(ggplot2::scale_color_gradientn(colors = colors, ...))
    }
  }
  
  # Default: kontinuerlig skala (för numeriska värden)
  ggplot2::scale_color_gradientn(colors = base_colors, ...)
}

# 2. DIVERGING SCALES ===================================================

#' ggplot2 scale för fill - Diverging
#'
#' Använd för värden med negativ-neutral-positiv riktning
#'
#' @param palette Namn på palett: "red_green", "blue_red", "purple_green"
#' @param n Antal färger (NULL = automatisk från data)
#' @param direction 1 = normal ordning, -1 = omvänd ordning
#' @param discrete Tvinga diskret (TRUE) eller kontinuerlig (FALSE) skala (NULL = automatisk)
#' @param ... Övriga argument till scale_fill_gradientn eller scale_fill_manual
#' @return ggplot2 scale-objekt
#' @examples
#' # Kontinuerlig skala för avvikelse
#' ggplot(data, aes(fill = change_percent)) + 
#'   geom_sf() + 
#'   scale_fill_gbg_diverging("red_green")
#' 
#' # Diskret skala med 7 steg
#' ggplot(data, aes(fill = category)) + 
#'   geom_sf() + 
#'   scale_fill_gbg_diverging("blue_red", n = 7, discrete = TRUE)
#' @export
scale_fill_gbg_diverging <- function(palette = "red_green", 
                                     n = NULL, 
                                     direction = 1,
                                     discrete = NULL,
                                     ...) {
  
  # Validera palette
  valid_palettes <- names(DIVERGING_PALETTES)
  if (!is.character(palette) || length(palette) != 1) {
    stop("'palette' måste vara en character string")
  }
  if (!palette %in% valid_palettes) {
    stop(
      "Ogiltig diverging palett: '", palette, "'\n",
      "Giltiga paletter: ", paste(valid_palettes, collapse = ", ")
    )
  }
  
  # Validera direction
  if (!is.numeric(direction) || length(direction) != 1 || !direction %in% c(-1, 1)) {
    stop("'direction' måste vara 1 (normal) eller -1 (omvänd)")
  }
  
  # Validera n om angivet
  if (!is.null(n)) {
    if (!is.numeric(n) || length(n) != 1 || n < 2 || n != as.integer(n)) {
      stop("'n' måste vara ett heltal >= 2")
    }
  }
  
  # Validera discrete om angivet
  if (!is.null(discrete) && !is.logical(discrete)) {
    stop("'discrete' måste vara TRUE, FALSE eller NULL")
  }
  
  # Hämta baspaletten
  base_colors <- gbg_palette("diverging", palette, n = NULL, direction = direction)
  
  # Om discrete = FALSE explicit, använd kontinuerlig
  if (!is.null(discrete) && discrete == FALSE) {
    colors <- gbg_palette("diverging", palette, n = n, direction = direction)
    return(ggplot2::scale_fill_gradientn(colors = colors, ...))
  }
  
  # Om discrete = TRUE explicit, använd diskret
  if (!is.null(discrete) && discrete == TRUE) {
    colors <- gbg_palette("diverging", palette, n = n, direction = direction)
    return(ggplot2::scale_fill_manual(values = colors, ...))
  }
  
  # Om n angivet, avgör baserat på n
  if (!is.null(n)) {
    colors <- gbg_palette("diverging", palette, n = n, direction = direction)
    if (n <= 10) {
      return(ggplot2::scale_fill_manual(values = colors, ...))
    } else {
      return(ggplot2::scale_fill_gradientn(colors = colors, ...))
    }
  }
  
  # Default: kontinuerlig skala (för numeriska värden)
  ggplot2::scale_fill_gradientn(colors = base_colors, ...)
}

#' ggplot2 scale för color - Diverging
#'
#' Använd för värden med negativ-neutral-positiv riktning
#'
#' @param palette Namn på palett: "red_green", "blue_red", "purple_green"
#' @param n Antal färger (NULL = automatisk från data)
#' @param direction 1 = normal ordning, -1 = omvänd ordning
#' @param discrete Tvinga diskret (TRUE) eller kontinuerlig (FALSE) skala (NULL = automatisk)
#' @param ... Övriga argument till scale_color_gradientn eller scale_color_manual
#' @return ggplot2 scale-objekt
#' @examples
#' # Kontinuerlig skala
#' ggplot(data, aes(x = year, y = value, color = deviation)) + 
#'   geom_line() + 
#'   scale_color_gbg_diverging("red_green")
#' @export
scale_color_gbg_diverging <- function(palette = "red_green", 
                                      n = NULL, 
                                      direction = 1,
                                      discrete = NULL,
                                      ...) {
  
  # Validera palette
  valid_palettes <- names(DIVERGING_PALETTES)
  if (!is.character(palette) || length(palette) != 1) {
    stop("'palette' måste vara en character string")
  }
  if (!palette %in% valid_palettes) {
    stop(
      "Ogiltig diverging palett: '", palette, "'\n",
      "Giltiga paletter: ", paste(valid_palettes, collapse = ", ")
    )
  }
  
  # Validera direction
  if (!is.numeric(direction) || length(direction) != 1 || !direction %in% c(-1, 1)) {
    stop("'direction' måste vara 1 (normal) eller -1 (omvänd)")
  }
  
  # Validera n om angivet
  if (!is.null(n)) {
    if (!is.numeric(n) || length(n) != 1 || n < 2 || n != as.integer(n)) {
      stop("'n' måste vara ett heltal >= 2")
    }
  }
  
  # Validera discrete om angivet
  if (!is.null(discrete) && !is.logical(discrete)) {
    stop("'discrete' måste vara TRUE, FALSE eller NULL")
  }
  
  # Hämta baspaletten
  base_colors <- gbg_palette("diverging", palette, n = NULL, direction = direction)
  
  # Om discrete = FALSE explicit, använd kontinuerlig
  if (!is.null(discrete) && discrete == FALSE) {
    colors <- gbg_palette("diverging", palette, n = n, direction = direction)
    return(ggplot2::scale_color_gradientn(colors = colors, ...))
  }
  
  # Om discrete = TRUE explicit, använd diskret
  if (!is.null(discrete) && discrete == TRUE) {
    colors <- gbg_palette("diverging", palette, n = n, direction = direction)
    return(ggplot2::scale_color_manual(values = colors, ...))
  }
  
  # Om n angivet, avgör baserat på n
  if (!is.null(n)) {
    colors <- gbg_palette("diverging", palette, n = n, direction = direction)
    if (n <= 10) {
      return(ggplot2::scale_color_manual(values = colors, ...))
    } else {
      return(ggplot2::scale_color_gradientn(colors = colors, ...))
    }
  }
  
  # Default: kontinuerlig skala (för numeriska värden)
  ggplot2::scale_color_gradientn(colors = base_colors, ...)
}

# 3. CATEGORICAL SCALES =================================================

#' ggplot2 scale för fill - Categorical
#'
#' Använd för kategoriska variabler utan inbördes ordning
#'
#' @param palette Namn på palett: "palette_1" till "palette_7"
#' @param direction 1 = normal ordning, -1 = omvänd ordning
#' @param ... Övriga argument till scale_fill_manual
#' @return ggplot2 scale-objekt
#' @examples
#' # 4 kategorier
#' ggplot(data, aes(fill = district)) + 
#'   geom_sf() + 
#'   scale_fill_gbg_categorical("palette_4")
#' 
#' # 7 kategorier
#' ggplot(data, aes(fill = party)) + 
#'   geom_sf() + 
#'   scale_fill_gbg_categorical("palette_7")
#' @export
scale_fill_gbg_categorical <- function(palette = "palette_4", 
                                       direction = 1,
                                       ...) {
  
  # Validera palette
  valid_palettes <- names(CATEGORICAL_PALETTES)
  if (!is.character(palette) || length(palette) != 1) {
    stop("'palette' måste vara en character string")
  }
  if (!palette %in% valid_palettes) {
    stop(
      "Ogiltig categorical palett: '", palette, "'\n",
      "Giltiga paletter: ", paste(valid_palettes, collapse = ", ")
    )
  }
  
  # Validera direction
  if (!is.numeric(direction) || length(direction) != 1 || !direction %in% c(-1, 1)) {
    stop("'direction' måste vara 1 (normal) eller -1 (omvänd)")
  }
  
  colors <- gbg_palette("categorical", palette, direction = direction)
  
  ggplot2::scale_fill_manual(values = colors, ...)
}

#' ggplot2 scale för color - Categorical
#'
#' Använd för kategoriska variabler utan inbördes ordning
#'
#' @param palette Namn på palett: "palette_1" till "palette_7"
#' @param direction 1 = normal ordning, -1 = omvänd ordning
#' @param ... Övriga argument till scale_color_manual
#' @return ggplot2 scale-objekt
#' @examples
#' # 4 kategorier
#' ggplot(data, aes(x = year, y = value, color = district)) + 
#'   geom_line() + 
#'   scale_color_gbg_categorical("palette_4")
#' @export
scale_color_gbg_categorical <- function(palette = "palette_4", 
                                        direction = 1,
                                        ...) {
  
  # Validera palette
  valid_palettes <- names(CATEGORICAL_PALETTES)
  if (!is.character(palette) || length(palette) != 1) {
    stop("'palette' måste vara en character string")
  }
  if (!palette %in% valid_palettes) {
    stop(
      "Ogiltig categorical palett: '", palette, "'\n",
      "Giltiga paletter: ", paste(valid_palettes, collapse = ", ")
    )
  }
  
  # Validera direction
  if (!is.numeric(direction) || length(direction) != 1 || !direction %in% c(-1, 1)) {
    stop("'direction' måste vara 1 (normal) eller -1 (omvänd)")
  }
  
  colors <- gbg_palette("categorical", palette, direction = direction)
  
  ggplot2::scale_color_manual(values = colors, ...)
}