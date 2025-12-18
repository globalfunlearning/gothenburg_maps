# =============================================================================
# SETUP: LADDA ALLA FUNKTIONER FÖR GOTHENBURG_MAPS
# =============================================================================
#
# Detta script laddar alla funktioner från gothenburg_maps-projektet.
#
# Användning:
#   source("R/00_setup.R")
#
# =============================================================================

# Kontrollera att vi kan hitta filerna
if (!file.exists("R/classify.R")) {
  stop(
    "Kan inte hitta R-funktionerna.\n",
    "Kör detta script från projektets root-mapp (där .Rproj-filen ligger)."
  )
}

cat("\n")
cat("=============================================================================\n")
cat("  GOTHENBURG_MAPS - Laddar funktioner...\n")
cat("=============================================================================\n\n")

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

# Ladda varje fil
for (file in files_to_load) {
  if (file.exists(file)) {
    source(file, encoding = "UTF-8")
    cat("✓", basename(file), "\n")
  } else {
    warning("Kunde inte hitta: ", file)
  }
}

cat("\n")
cat("=============================================================================\n")
cat("  KLART! Funktioner laddade.\n")
cat("=============================================================================\n\n")

# Visa huvudfunktioner
cat("Huvudfunktioner:\n")
cat("  - load_prepared_map()        Ladda kartlager\n")
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
if (dir.exists("input/prepared_maps")) {
  goteborg_dir <- "input/prepared_maps/goteborg"
  sverige_dir <- "input/prepared_maps/sverige"
  
  goteborg_maps <- character(0)
  sverige_maps <- character(0)
  
  if (dir.exists(goteborg_dir)) {
    goteborg_maps <- list.files(goteborg_dir, pattern = "\\.rds$")
  }
  
  if (dir.exists(sverige_dir)) {
    sverige_maps <- list.files(sverige_dir, pattern = "\\.rds$")
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
