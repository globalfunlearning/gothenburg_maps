# =============================================================================
# SETUP: LADDA ALLA FUNKTIONER FÖR GOTHENBURG_MAPS
# =============================================================================
#
# Detta script laddar alla funktioner från gothenburg_maps-projektet.
#
# Användning:
#   # Från gothenburg_maps-projektet:
#   source("R/00_setup.R")
#
#   # Från andra projekt (med biblioteksklon):
#   source("~/Documents/R-bibliotek/gothenburg_maps/R/00_setup.R")
#
# =============================================================================

# Hitta var detta script ligger och navigera till projektets root
script_path <- sys.frame(1)$ofile

if (is.null(script_path)) {
  # Om script_path är NULL (kan hända i vissa miljöer), försök alternativ metod
  script_path <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  script_path <- sub(file_arg, "", script_path[grep(file_arg, script_path)])
}

# Om vi fortfarande inte hittar sökvägen, anta att vi kör från projekt-root
if (length(script_path) == 0 || is.null(script_path) || script_path == "") {
  project_root <- getwd()
} else {
  script_dir <- dirname(script_path)
  project_root <- dirname(script_dir)  # Gå upp från R/ till projektets rot
}

# Kontrollera att vi kan hitta filerna
classify_path <- file.path(project_root, "R", "classify.R")
if (!file.exists(classify_path)) {
  stop(
    "Kan inte hitta R-funktionerna.\n",
    "Sökte i: ", project_root, "\n",
    "Kontrollera att sökvägen till gothenburg_maps är korrekt."
  )
}

cat("\n")
cat("=============================================================================\n")
cat("  GOTHENBURG_MAPS - Laddar funktioner...\n")
cat("=============================================================================\n")
cat("Projekt-root: ", project_root, "\n\n")

# Sätt global variabel för var kartlagren finns
# VARFÖR?: load_prepared_map() använder detta för att hitta kartlager när den körs från andra projekt
options(gothenburg_maps_root = project_root)
message("✓ Satt gothenburg_maps_root = ", project_root, "\n")

# Lista över filer att ladda (i rätt ordning)
files_to_load <- c(
  "R/geodata.R",          # Geodata-hantering (först - används av andra)
  "R/themes.R",           # ggplot2-teman för kartor
  "R/classify.R",         # Klassindelning
  "R/legend.R",           # Legend-konfiguration
  "R/labels.R",           # Kartlabels
  "R/map_static.R",       # Statiska kartor
  "R/map_interactive.R",  # Interaktiva kartor
  "R/export.R"            # Export
)

# Ladda varje fil med fullständig sökväg
for (file in files_to_load) {
  full_path <- file.path(project_root, file)
  if (file.exists(full_path)) {
    source(full_path, encoding = "UTF-8")
    cat("✓", basename(file), "\n")
  } else {
    warning("Kunde inte hitta: ", full_path)
  }
}

cat("\n")
cat("=============================================================================\n")
cat("  KLART! Funktioner laddade.\n")
cat("=============================================================================\n\n")

# Visa huvudfunktioner
cat("Huvudfunktioner:\n")
cat("  - load_prepared_map()        Ladda kartlager\n")
cat("  - load_deso_from_scb()       Ladda DeSO från SCB\n")
cat("  - load_regso_from_scb()      Ladda RegSO från SCB\n")
cat("  - join_stat_to_map()         Koppla statistik till karta\n")
cat("  - create_breaks()            Skapa klassgränser\n")
cat("  - create_labels()            Skapa labels\n")
cat("  - theme_gothenburg_map()     ggplot2-tema\n")
cat("  - create_static_map()        Skapa statisk karta\n")
cat("  - create_interactive_map()   Skapa interaktiv karta\n")
cat("  - save_map()                 Spara karta\n")
cat("\n")

# Kolla om gothenburg_colors finns
if (!exists("gbg_palette")) {
  cat("💡 Tips: Ladda gothenburg_colors för färgpaletter:\n")
  cat("   source('path/to/gothenburg_colors/colors.R')\n")
  cat("\n")
}

# Kolla vilka paket som behövs
required_packages <- c("sf", "dplyr", "ggplot2")
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  cat("⚠ Saknade paket (krävs för att funktionerna ska fungera):\n")
  cat("   install.packages(c(", paste0("'", missing_packages, "'", collapse = ", "), "))\n")
  cat("\n")
}

# Visa tillgängliga kartlager
prepared_maps_dir <- file.path(project_root, "input", "prepared_maps")
if (dir.exists(prepared_maps_dir)) {
  goteborg_dir <- file.path(prepared_maps_dir, "goteborg")
  sverige_dir <- file.path(prepared_maps_dir, "sverige")
  
  goteborg_maps <- character(0)
  sverige_maps <- character(0)
  
  if (dir.exists(goteborg_dir)) {
    goteborg_maps <- list.files(goteborg_dir, pattern = "\\.rds$")
    # Filtrera bort backup-mappen
    goteborg_maps <- goteborg_maps[!grepl("backup", goteborg_maps)]
  }
  
  if (dir.exists(sverige_dir)) {
    sverige_maps <- list.files(sverige_dir, pattern = "\\.rds$")
    # Filtrera bort backup-mappen
    sverige_maps <- sverige_maps[!grepl("backup", sverige_maps)]
  }
  
  if (length(goteborg_maps) > 0 || length(sverige_maps) > 0) {
    cat("Tillgängliga kartlager:\n")
    
    if (length(goteborg_maps) > 0) {
      cat("  Göteborg (", length(goteborg_maps), "):\n", sep = "")
      for (map in goteborg_maps) {
        map_name <- tools::file_path_sans_ext(map)
        cat("    - goteborg/", map_name, "\n", sep = "")
      }
    }
    
    if (length(sverige_maps) > 0) {
      cat("  Sverige (", length(sverige_maps), "):\n", sep = "")
      for (map in sverige_maps) {
        map_name <- tools::file_path_sans_ext(map)
        cat("    - sverige/", map_name, "\n", sep = "")
      }
    }
    cat("\n")
  }
}

cat("Redo att skapa kartor! 🗺️\n\n")