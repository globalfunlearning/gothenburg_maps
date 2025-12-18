# =============================================================================
# GEODATA.R
# GIS- och geodata-funktioner för kartvisualisering
# =============================================================================
#
# Detta är det centrala verktyget för att ladda, validera och manipulera
# geodata i kartprojektet. Funktionerna bygger på sf-paketet och följer
# Göteborgs Stads kodningsstandarder.
#
# NYTT: Funktioner för DeSO och RegSO från SCB
#
# =============================================================================

library(sf)
library(dplyr)

# =============================================================================
# LADDA FÖRBEREDDA KARTLAGER
# =============================================================================

#' Ladda förberedda kartlager
#'
#' Läser in ett förberett kartlager i rds-format. Mycket snabbare än att
#' läsa shapefiler varje gång.
#'
#' @param map_name Namn på kartlagret (inklusive mapp, utan .rds-suffix)
#' @param data_dir Mapp där förberedda kartlager finns (default: "input/prepared_maps")
#'
#' @return sf-objekt
#'
#' @examples
#' # Göteborg
#' primaromraden <- load_prepared_map("goteborg/primaromraden")
#' stadsdelar <- load_prepared_map("goteborg/stadsdelar")
#'
#' # DeSO och RegSO
#' deso_gbg <- load_prepared_map("goteborg/deso_goteborg")
#' regso_gbg <- load_prepared_map("goteborg/regso_goteborg")
#'
#' # Sverige
#' kommuner <- load_prepared_map("sverige/kommuner")
#' deso_gr <- load_prepared_map("sverige/deso_gr")
load_prepared_map <- function(map_name,
                              data_dir = "input/prepared_maps") {
  
  # Kontrollera att map_name är en sträng
  if (!is.character(map_name) || length(map_name) != 1) {
    stop("map_name måste vara en textsträng, t.ex. 'goteborg/primaromraden'")
  }
  
  # Bygg filsökväg
  file_path <- file.path(data_dir, paste0(map_name, ".rds"))
  
  # Kontrollera att filen finns
  if (!file.exists(file_path)) {
    stop(
      "Kartlager finns inte: ", file_path, "\n",
      "Tillgängliga kartlager:\n",
      paste("  -", list_prepared_maps(data_dir)$map_name, collapse = "\n")
    )
  }
  
  # Läs in .rds-filen
  # VARFÖR rds?: Mycket snabbare än shapefiler, behåller exakt datastruktur
  map_data <- readRDS(file_path)
  
  # Validera att det är ett sf-objekt
  if (!inherits(map_data, "sf")) {
    stop(
      "Filen är inte ett giltigt kartlager (sf-objekt): ", file_path
    )
  }
  
  message("✓ Laddade kartlager: ", map_name, " (", nrow(map_data), " områden)")
  
  return(map_data)
}


#' Lista tillgängliga förberedda kartlager
#'
#' Visar information om alla förberedda kartlager i en tabell.
#' Hjälper dig att hitta rätt kartlager att använda.
#'
#' @param data_dir Mapp där förberedda kartlager finns (default: "input/prepared_maps")
#' @param pattern Regex-mönster för att filtrera vilka filer som ska listas (default: "\\.rds$")
#'
#' @return Data.frame med information om tillgängliga kartlager
#'
#' @examples
#' # Lista alla kartlager
#' list_prepared_maps()
#'
#' # Spara som tabell
#' kartlager_info <- list_prepared_maps()
#' View(kartlager_info)
list_prepared_maps <- function(data_dir = "input/prepared_maps",
                               pattern = "\\.rds$") {
  
  # Hitta alla rds-filer rekursivt
  # VARFÖR recursive?: Vi har nu undermappar (goteborg/, sverige/)
  files <- list.files(
    data_dir,
    pattern = pattern,
    full.names = TRUE,
    recursive = TRUE
  )
  
  if (length(files) == 0) {
    message("Inga förberedda kartlager finns i: ", data_dir)
    message("Skapa förberedda kartlager genom att använda load_geo_layer()")
    return(invisible(NULL))
  }
  
  # Samla information om varje fil
  # Skapa map_name relativt till data_dir (inkluderar undermapp)
  rel_paths <- sub(paste0(data_dir, "/"), "", files)
  map_names <- tools::file_path_sans_ext(rel_paths)
  
  map_info <- data.frame(
    map_name = map_names,
    file_name = basename(files),
    file_path = files,
    size_kb = round(file.size(files) / 1024, 1),
    created = format(file.mtime(files), "%Y-%m-%d %H:%M"),
    stringsAsFactors = FALSE
  )
  
  # Lägg till antal områden för varje kartlager
  # VARFÖR try-catch?: Vissa filer kanske inte är giltiga sf-objekt
  map_info$n_areas <- sapply(files, function(f) {
    tryCatch({
      map_data <- readRDS(f)
      if (inherits(map_data, "sf")) nrow(map_data) else NA
    }, error = function(e) NA)
  })
  
  # Sortera efter namn
  map_info <- map_info[order(map_info$map_name), ]
  rownames(map_info) <- NULL
  
  return(map_info)
}


# =============================================================================
# LADDA GEODATA FRÅN FIL (GENERELL FUNKTION)
# =============================================================================

#' Ladda geodata från fil
#'
#' Generell funktion för att ladda geodata från olika format och förbereda
#' för användning. Stöder Shapefile, GeoJSON, GeoPackage och KML.
#' 
#' Funktionen:
#' - Läser in geodata
#' - Transformerar till rätt projektion
#' - Validerar geometri
#' - Kan förenkla geometri (snabbare rendering)
#' - Kan klippa till avgränsat område
#'
#' VARFÖR denna funktion?: Flexibilitet att läsa egna shapefiles/GeoJSON
#' med standardiserade inställningar. Används också internt av DeSO/RegSO-funktioner.
#'
#' @param path Sökväg till fil (.shp, .geojson, .gpkg, .kml)
#' @param layer Layer-namn (endast för GeoPackage med flera lager)
#' @param crs Målprojektion (default: 3006 = SWEREF99 TM för Sverige)
#' @param simplify Förenkla geometri? Gör rendering snabbare
#' @param tolerance Tolerans för simplify i meter (default: 100)
#' @param clip sf-objekt att klippa till (t.ex. kommungräns)
#' @param validate Validera geometri efter laddning?
#'
#' @return sf-objekt i angiven projektion
#'
#' @examples
#' # Grundläggande användning
#' kommuner <- load_geo_layer("data/kommuner.shp")
#' 
#' # Med förenkling (snabbare, mindre detaljerad)
#' kommuner <- load_geo_layer("data/kommuner.shp", simplify = TRUE)
#' 
#' # Klipp detaljerad karta till kommungräns
#' kommun <- load_geo_layer("kommun.shp")
#' detaljer <- load_geo_layer("detaljer.shp", clip = kommun)
#' 
#' # Annan projektion (t.ex. WGS84 för webbkartor)
#' world <- load_geo_layer("world.geojson", crs = 4326)
#' 
#' @export
load_geo_layer <- function(path,
                           layer = NULL,
                           crs = 3006,
                           simplify = FALSE,
                           tolerance = 100,
                           clip = NULL,
                           validate = TRUE) {
  
  # VARFÖR generell funktion?: Flexibilitet att läsa egna filer
  # samtidigt som vi har standardiserade inställningar
  
  # 1. VALIDERING ----
  
  if (!file.exists(path)) {
    stop("Filen hittades inte: ", path)
  }
  
  # Detektera filformat
  file_ext <- tolower(tools::file_ext(path))
  
  # VARFÖR validera format?: Tydligt felmeddelande istället för kryptiskt st_read-fel
  valid_formats <- c("shp", "geojson", "gpkg", "kml")
  if (!file_ext %in% valid_formats) {
    stop(
      "Filformat stöds inte: .", file_ext, "\n",
      "Stödda format: ", paste(valid_formats, collapse = ", ")
    )
  }
  
  message("Laddar geodata: ", basename(path))
  
  # 2. LADDA GEODATA ----
  
  geo_data <- tryCatch(
    {
      if (!is.null(layer)) {
        st_read(path, layer = layer, quiet = TRUE)
      } else {
        st_read(path, quiet = TRUE)
      }
    },
    error = function(e) {
      stop("Kunde inte läsa fil: ", e$message)
    }
  )
  
  # 3. TRANSFORMERA TILL RÄTT CRS ----
  # VARFÖR alltid transformera?: Säkerställer att alla lager har samma projektion
  
  current_crs <- st_crs(geo_data)
  if (is.na(current_crs) || current_crs != st_crs(crs)) {
    message("  Transformerar till CRS: ", crs)
    geo_data <- st_transform(geo_data, crs)
  }
  
  # 4. VALIDERA GEOMETRI ----
  # VARFÖR validera?: Ogiltig geometri orsakar problem i analyser och visualiseringar
  
  if (validate) {
    invalid <- !st_is_valid(geo_data)
    if (any(invalid)) {
      n_invalid <- sum(invalid)
      warning(n_invalid, " geometrier är ogiltiga. Försöker fixa...")
      geo_data <- st_make_valid(geo_data)
    }
  }
  
  # 5. FÖRENKLA GEOMETRI ----
  # VARFÖR förenkla?: Stor detaljrikedom gör kartor långsamma att rendera
  
  if (simplify) {
    message("  Förenklar geometri (tolerans: ", tolerance, "m)")
    n_coords_before <- sum(sapply(st_geometry(geo_data), length))
    geo_data <- st_simplify(geo_data, dTolerance = tolerance)
    n_coords_after <- sum(sapply(st_geometry(geo_data), length))
    
    reduction_pct <- round((1 - n_coords_after/n_coords_before) * 100, 1)
    message("  → Minskade punkter med ", reduction_pct, "%")
  }
  
  # 6. KLIPP TILL OMRÅDE ----
  # VARFÖR klippa?: Snabbare rendering och fokus på relevant område
  
  if (!is.null(clip)) {
    if (!inherits(clip, "sf")) {
      stop("clip måste vara ett sf-objekt")
    }
    message("  Klipper till angivet område")
    geo_data <- st_intersection(geo_data, clip)
  }
  
  message("✓ Laddade ", nrow(geo_data), " geometrier")
  
  return(geo_data)
}



# =============================================================================
# DESO/REGSO-FUNKTIONER - 2025 ÅRS VERSION
# =============================================================================
#
# Uppdaterat 2025-12-16: Använder DeSO 2025 och RegSO 2025
# - Hämtar direkt från SCB:s WFS-tjänst
# - Inga zip-filer, snabbare nedladdning
# - Senaste versionen (2025-01-01)
#
# =============================================================================

#' Ladda DeSO 2025 från SCB
#'
#' Hämtar Demografiska statistikområden (DeSO 2025) direkt från SCB:s 
#' WFS-tjänst och transformerar till SWEREF99 TM (EPSG:3006).
#'
#' DeSO 2025 är den nya versionen som gäller från 2025-01-01.
#' Statistik som publiceras under 2025 använder denna indelning.
#'
#' VARFÖR denna funktion?: Alltid senaste versionen från SCB,
#' snabbt via WFS (ingen zip-hantering).
#'
#' @param crs Målprojektion (default: 3006 = SWEREF99 TM)
#' @param simplify Förenkla geometri? (default: FALSE)
#' @param tolerance Tolerans för simplify i meter (default: 100)
#'
#' @return sf-objekt med DeSO 2025-polygoner
#'
#' @examples
#' # Ladda alla DeSO 2025
#' deso_alla <- load_deso_from_scb()
#'
#' # Filtrera till Göteborg
#' deso_gbg <- deso_alla |>
#'   filter(kommun == "1480")
#'
#' # Med förenklad geometri
#' deso_enkel <- load_deso_from_scb(simplify = TRUE)
#'
#' @export
load_deso_from_scb <- function(crs = 3006,
                               simplify = FALSE,
                               tolerance = 100) {
  
  message("Laddar DeSO 2025 från SCB via WFS...")
  
  # SCB:s WFS-URL för DeSO 2025
  # VARFÖR WFS?: Snabbare än zip, ingen temporär filhantering
  wfs_url <- "https://geodata.scb.se/geoserver/stat/wfs?service=WFS&REQUEST=GetFeature&version=1.1.0&TYPENAMES=stat:DeSO_2025&outputFormat=geopackage"
  
  # 1. LADDA NER TILL TEMPORÄR FIL ----
  
  temp_file <- tempfile(fileext = ".gpkg")
  
  message("  Laddar ner GeoPackage...")
  tryCatch(
    {
      download.file(wfs_url, destfile = temp_file, quiet = TRUE, mode = "wb")
    },
    error = function(e) {
      stop("Kunde inte ladda ner DeSO 2025 från SCB: ", e$message)
    }
  )
  
  # 2. LÄS GEOPACKAGE ----
  
  message("  Läser geodata...")
  deso_data <- st_read(temp_file, quiet = TRUE)
  
  # 3. BYTA KOLUMNNAMN TILL STANDARD ----
  # VARFÖR?: SCB kan använda olika namn, vi vill ha konsekventa namn
  
  if ("geom" %in% names(deso_data)) {
    deso_data <- deso_data |>
      dplyr::rename(geometry = geom)
  }
  
  # 4. TRANSFORMERA CRS ----
  
  message("  Transformerar till CRS: ", crs)
  deso_data <- st_transform(deso_data, crs)
  
  # 5. FÖRENKLA (OM ÖNSKAT) ----
  
  if (simplify) {
    message("  Förenklar geometri (tolerans: ", tolerance, "m)")
    n_coords_before <- sum(sapply(st_geometry(deso_data), length))
    deso_data <- st_simplify(deso_data, dTolerance = tolerance)
    n_coords_after <- sum(sapply(st_geometry(deso_data), length))
    reduction_pct <- round((1 - n_coords_after/n_coords_before) * 100, 1)
    message("  → Minskade punkter med ", reduction_pct, "%")
  }
  
  # 6. STÄDA UPP ----
  
  unlink(temp_file)
  
  message("✓ Laddade ", nrow(deso_data), " DeSO 2025-områden")
  
  return(deso_data)
}


#' Ladda RegSO 2025 från SCB
#'
#' Hämtar Regionala statistikområden (RegSO 2025) direkt från SCB:s
#' WFS-tjänst och transformerar till SWEREF99 TM (EPSG:3006).
#'
#' RegSO 2025 är den nya versionen som gäller från 2025-01-01.
#' RegSO är en grövre indelning än DeSO.
#'
#' VARFÖR denna funktion?: Alltid senaste versionen från SCB,
#' snabbt via WFS (ingen zip-hantering).
#'
#' @param crs Målprojektion (default: 3006 = SWEREF99 TM)
#' @param simplify Förenkla geometri? (default: FALSE)
#' @param tolerance Tolerans för simplify i meter (default: 100)
#'
#' @return sf-objekt med RegSO 2025-polygoner
#'
#' @examples
#' # Ladda alla RegSO 2025
#' regso_alla <- load_regso_from_scb()
#'
#' # Filtrera till Göteborg
#' regso_gbg <- regso_alla |>
#'   filter(kommun == "1480")
#'
#' # Med förenklad geometri
#' regso_enkel <- load_regso_from_scb(simplify = TRUE)
#'
#' @export
load_regso_from_scb <- function(crs = 3006,
                                simplify = FALSE,
                                tolerance = 100) {
  
  message("Laddar RegSO 2025 från SCB via WFS...")
  
  # SCB:s WFS-URL för RegSO 2025
  wfs_url <- "https://geodata.scb.se/geoserver/stat/wfs?service=WFS&REQUEST=GetFeature&version=1.1.0&TYPENAMES=stat:RegSO_2025&outputFormat=geopackage"
  
  # 1. LADDA NER TILL TEMPORÄR FIL ----
  
  temp_file <- tempfile(fileext = ".gpkg")
  
  message("  Laddar ner GeoPackage...")
  tryCatch(
    {
      download.file(wfs_url, destfile = temp_file, quiet = TRUE, mode = "wb")
    },
    error = function(e) {
      stop("Kunde inte ladda ner RegSO 2025 från SCB: ", e$message)
    }
  )
  
  # 2. LÄS GEOPACKAGE ----
  
  message("  Läser geodata...")
  regso_data <- st_read(temp_file, quiet = TRUE)
  
  # 3. BYTA KOLUMNNAMN TILL STANDARD ----
  
  if ("geom" %in% names(regso_data)) {
    regso_data <- regso_data |>
      dplyr::rename(geometry = geom)
  }
  
  # 4. TRANSFORMERA CRS ----
  
  message("  Transformerar till CRS: ", crs)
  regso_data <- st_transform(regso_data, crs)
  
  # 5. FÖRENKLA (OM ÖNSKAT) ----
  
  if (simplify) {
    message("  Förenklar geometri (tolerans: ", tolerance, "m)")
    n_coords_before <- sum(sapply(st_geometry(regso_data), length))
    regso_data <- st_simplify(regso_data, dTolerance = tolerance)
    n_coords_after <- sum(sapply(st_geometry(regso_data), length))
    reduction_pct <- round((1 - n_coords_after/n_coords_before) * 100, 1)
    message("  → Minskade punkter med ", reduction_pct, "%")
  }
  
  # 6. STÄDA UPP ----
  
  unlink(temp_file)
  
  message("✓ Laddade ", nrow(regso_data), " RegSO 2025-områden")
  
  return(regso_data)
}


#' Ladda kopplingstabell DeSO-RegSO från SCB
#'
#' Hämtar kopplingstabellen mellan DeSO 2025 och RegSO 2025 från SCB.
#' Används för att koppla RegSO-information till DeSO-områden.
#'
#' VARFÖR denna funktion?: DeSO och RegSO är olika indelningar,
#' denna tabell visar vilket RegSO varje DeSO tillhör.
#'
#' @return Data.frame med kolumnerna: 
#'   - Kommun (kommunkod)
#'   - Kommunnamn
#'   - DeSO_2025 (DeSO-kod)
#'   - RegSO_2025 (RegSO-namn)
#'   - RegSOkod_2025 (RegSO-kod)
#'
#' @examples
#' # Ladda kopplingstabell
#' koppling <- load_deso_regso_koppling()
#'
#' # Koppla till DeSO-data
#' deso_med_regso <- deso_data |>
#'   left_join(koppling, by = c("deso" = "DeSO_2025"))
#'
#' @export
load_deso_regso_koppling <- function() {
  
  message("Laddar kopplingstabell DeSO-RegSO från SCB...")
  
  # SCB:s URL för kopplingstabellen
  scb_url <- "https://www.scb.se/contentassets/e3b2f06da62046ba93ff58af1b845c7e/koppling-deso2025-regso2025.xlsx"
  
  # 1. LADDA NER TILL TEMPORÄR FIL ----
  
  temp_file <- tempfile(fileext = ".xlsx")
  
  message("  Laddar ner Excel-fil...")
  tryCatch(
    {
      download.file(scb_url, destfile = temp_file, quiet = TRUE, mode = "wb")
    },
    error = function(e) {
      stop("Kunde inte ladda ner kopplingstabell från SCB: ", e$message)
    }
  )
  
  # 2. LÄS EXCEL ----
  # VARFÖR skip = 3?: Kolumnnamn är på rad 4, så skippa första 3 raderna
  
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Paketet 'readxl' krävs för att läsa kopplingstabellen.\n",
         "Installera med: install.packages('readxl')")
  }
  
  message("  Läser Excel-fil...")
  koppling <- readxl::read_excel(temp_file, skip = 3)
  
  # 3. VALIDERA KOLUMNER ----
  
  expected_cols <- c("Kommun", "Kommunnamn", "DeSO_2025", "RegSO_2025", "RegSOkod_2025")
  
  if (!all(expected_cols %in% names(koppling))) {
    warning("Kolumnnamn har ändrats i SCB:s fil. Förväntat: ", 
            paste(expected_cols, collapse = ", "),
            "\nHittade: ",
            paste(names(koppling), collapse = ", "))
  }
  
  # 4. STÄDA UPP ----
  
  unlink(temp_file)
  
  message("✓ Laddade ", nrow(koppling), " rader från kopplingstabell")
  
  return(koppling)
}

# =============================================================================
# VATTENLAGER
# =============================================================================

#' Ladda vattenlager
#'
#' Läser in förberedda vattenlager (älvar, sjöar, hav) för visualisering.
#' Vatten renderas ofta annorlunda än övrig geografi, därför egen funktion.
#'
#' @param water_name Namn på vattenlagret (inklusive mapp, utan .rds-suffix)
#' @param data_dir Mapp där förberedda vattenlager finns (default: "input/prepared_maps")
#'
#' @return sf-objekt med vattengeometrier, eller NULL om fil saknas
#'
#' @examples
#' # Göteborg
#' alv <- load_water_layer("goteborg/alv_goteborg")
#' hav <- load_water_layer("goteborg/hav_goteborg")
#'
#' # Sverige
#' vatten <- load_water_layer("sverige/vatten_sverige")
#'
#' # Använd i karta
#' ggplot() +
#'   geom_sf(data = load_prepared_map("goteborg/primaromraden"), aes(fill = area_km2)) +
#'   geom_sf(data = load_water_layer("goteborg/alv_goteborg"), fill = "lightblue", color = NA) +
#'   theme_void()
load_water_layer <- function(water_name,
                             data_dir = "input/prepared_maps") {
  
  # Använd samma logik som load_prepared_map()
  # VARFÖR egen funktion?: Tydliggör att det är vatten, kan lägga till
  # vattenspecifik funktionalitet senare (t.ex. automatisk styling)
  
  file_path <- file.path(data_dir, paste0(water_name, ".rds"))
  
  if (!file.exists(file_path)) {
    warning(
      "Vattenlager finns inte: ", file_path, "\n",
      "Tillgängliga lager:\n",
      paste("  -", list_prepared_maps(data_dir)$map_name, collapse = "\n")
    )
    return(NULL)
  }
  
  water_data <- readRDS(file_path)
  
  if (!inherits(water_data, "sf")) {
    warning("Filen är inte ett giltigt sf-objekt: ", file_path)
    return(NULL)
  }
  
  message("✓ Laddade vattenlager: ", water_name)
  
  return(water_data)
}


# =============================================================================
# KOPPLA STATISTIK TILL KARTA
# =============================================================================

#' Koppla statistikdata till kartlager
#'
#' Kopplar tabelldatakällor (CSV, Excel, dataframe) till kartgeometrier.
#' Säkerställer matchning mellan data och geografi.
#'
#' @param map_data sf-objekt med kartgeometrier
#' @param stat_data dataframe med statistik
#' @param by Kolumnnamn att matcha på (om samma i båda)
#' @param by_map Kolumnnamn i map_data (om olika från stat_data)
#' @param by_stat Kolumnnamn i stat_data (om olika från map_data)
#'
#' @return sf-objekt med både geometri och statistik
#'
#' @examples
#' # Enkel matchning (samma kolumnnamn)
#' karta <- join_stat_to_map(
#'   map_data = primaromraden,
#'   stat_data = befolkning,
#'   by = "omrade_kod"
#' )
#'
#' # Olika kolumnnamn
#' karta <- join_stat_to_map(
#'   map_data = kommuner,
#'   stat_data = scb_data,
#'   by_map = "kommun_kod",
#'   by_stat = "kod"
#' )
join_stat_to_map <- function(map_data,
                             stat_data,
                             by = NULL,
                             by_map = NULL,
                             by_stat = NULL) {
  
  # Validera input
  if (!inherits(map_data, "sf")) {
    stop("map_data måste vara ett sf-objekt")
  }
  
  if (!is.data.frame(stat_data)) {
    stop("stat_data måste vara en dataframe")
  }
  
  # Bestäm join-kolumner
  # VARFÖR denna logik?: Flexibilitet - både enkla och komplexa joins
  if (!is.null(by)) {
    join_by_map <- by
    join_by_stat <- by
  } else if (!is.null(by_map) && !is.null(by_stat)) {
    join_by_map <- by_map
    join_by_stat <- by_stat
  } else {
    stop(
      "Måste ange antingen 'by' (om samma kolumnnamn)\n",
      "eller både 'by_map' och 'by_stat' (om olika kolumnnamn)"
    )
  }
  
  # Validera att kolumnerna finns
  if (!join_by_map %in% names(map_data)) {
    stop("Kolumnen '", join_by_map, "' finns inte i map_data")
  }
  
  if (!join_by_stat %in% names(stat_data)) {
    stop("Kolumnen '", join_by_stat, "' finns inte i stat_data")
  }
  
  # Formatera join-kolumner till samma typ
  # VARFÖR?: Förhindra join-problem pga olika datatyper
  map_data[[join_by_map]] <- as.character(map_data[[join_by_map]])
  stat_data[[join_by_stat]] <- as.character(stat_data[[join_by_stat]])
  
  message("Matchar på kolumn: '", join_by_map, "' = '", join_by_stat, "'")
  message("  Formaterade matchningskolumner till text")
  
  # Koppla ihop
  result <- map_data |>
    dplyr::left_join(
      stat_data,
      by = setNames(join_by_stat, join_by_map)
    )
  
  # Kontrollera matchning
  n_map <- nrow(map_data)
  n_stat <- nrow(stat_data)
  n_matched <- sum(!is.na(result[[names(stat_data)[1]]]))
  
  match_pct <- round(n_matched / n_map * 100, 1)
  
  if (match_pct == 100) {
    message("✓ Perfekt matchning: ", n_matched, "/", n_map, " områden (100%)")
  } else if (match_pct >= 95) {
    message("✓ Bra matchning: ", n_matched, "/", n_map, " områden (", match_pct, "%)")
  } else if (match_pct >= 80) {
    warning("⚠ Okej matchning: ", n_matched, "/", n_map, " områden (", match_pct, "%)")
  } else {
    warning("❌ Dålig matchning: ", n_matched, "/", n_map, " områden (", match_pct, "%)")
  }
  
  # Visa vilka som inte matchade
  unmatched_map <- setdiff(
    map_data[[join_by_map]],
    stat_data[[join_by_stat]]
  )
  
  unmatched_stat <- setdiff(
    stat_data[[join_by_stat]],
    map_data[[join_by_map]]
  )
  
  if (length(unmatched_map) > 0 && length(unmatched_map) <= 10) {
    message("  Info: ", length(unmatched_map), " områden i kartan finns inte i statistiken:")
    message("    ", paste(head(unmatched_map, 10), collapse = ", "))
  } else if (length(unmatched_map) > 10) {
    message("  Info: ", length(unmatched_map), " områden i kartan finns inte i statistiken")
  }
  
  if (length(unmatched_stat) > 0 && length(unmatched_stat) <= 10) {
    message("  Info: ", length(unmatched_stat), " värden i statistiken finns inte i kartlagret:")
    message("    ", paste(head(unmatched_stat, 10), collapse = ", "))
  } else if (length(unmatched_stat) > 10) {
    message("  Info: ", length(unmatched_stat), " värden i statistiken finns inte i kartlagret")
  }
  
  # Identifiera icke-numeriska kolumner
  non_numeric <- names(result)[!sapply(result, is.numeric) & names(result) != "geometry"]
  if (length(non_numeric) > 0) {
    message("  Info: Icke-numeriska kolumner: ", paste(non_numeric, collapse = ", "))
  }
  
  message("  ✓ ", nrow(result), " områden efter merge")
  
  return(result)
}


# =============================================================================
# SPATIAL JOINS
# =============================================================================

#' Koppla punktdata till områden
#'
#' Spatial join - kopplar punkter (t.ex. skolor, affärer) till polygon-områden
#' och räknar antal per område. Mycket användbart för tematiska kartor.
#'
#' @param polygons sf-objekt med områden (polygon)
#' @param points sf-objekt med punkter
#' @param count_column Namn på ny kolumn för antal (default: "n_points")
#'
#' @return sf-objekt (polygons) med ny kolumn för antal punkter
#'
#' @examples
#' # Räkna skolor per stadsdel
#' stadsdelar <- join_points_to_areas(stadsdelar, skolor, "antal_skolor")
#'
#' # Visualisera
#' ggplot(stadsdelar, aes(fill = antal_skolor)) +
#'   geom_sf() +
#'   scale_fill_gbg_sequential("blue")
join_points_to_areas <- function(polygons,
                                 points,
                                 count_column = "n_points") {
  
  # VARFÖR denna funktion?: Vanlig uppgift för tematiska kartor att visa
  # "antal X per område"
  
  if (!inherits(polygons, "sf") || !inherits(points, "sf")) {
    stop("Både polygons och points måste vara sf-objekt")
  }
  
  # Validera geometrityper
  poly_type <- unique(as.character(st_geometry_type(polygons)))
  point_type <- unique(as.character(st_geometry_type(points)))
  
  if (!all(poly_type %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("polygons måste ha POLYGON-geometri")
  }
  
  if (!all(point_type %in% c("POINT", "MULTIPOINT"))) {
    stop("points måste ha POINT-geometri")
  }
  
  # Säkerställ samma CRS
  if (st_crs(polygons) != st_crs(points)) {
    message("Transformerar points till samma CRS som polygons")
    points <- st_transform(points, st_crs(polygons))
  }
  
  # Räkna punkter per polygon
  # VARFÖR st_intersects?: Ger lista med index för punkter i varje polygon
  counts <- lengths(st_intersects(polygons, points))
  
  # Lägg till som ny kolumn
  polygons[[count_column]] <- counts
  
  message(
    "✓ Kopplade ", nrow(points), " punkter till ", nrow(polygons), " områden\n",
    "  Medel: ", round(mean(counts), 1), " punkter per område\n",
    "  Max: ", max(counts), " punkter i ett område"
  )
  
  return(polygons)
}