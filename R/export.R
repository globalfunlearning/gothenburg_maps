# =============================================================================
# EXPORT UTILITIES - Spara och exportera kartor
# =============================================================================

library(ggplot2)
library(ggiraph)


#' Spara karta till fil
#' 
#' VARFÖR save_map?: Enkel funktion för att spara både statiska och
#' interaktiva kartor i rätt format med bra defaults.
#' 
#' @param plot Kart-objekt (ggplot för statiska, girafe för interaktiva)
#' @param filename Filnamn (med eller utan extension)
#' @param format Format att spara i: "png", "pdf", "svg", "html", "auto", eller vektor med flera
#' @param width Bredd i inches (default: 10)
#' @param height Höjd i inches (default: 8)
#' @param dpi DPI för PNG (default: 300)
#' @param path Mapp att spara i (default: "output/maps")
#' @param self_contained För HTML: embedded CSS/JS? (default: TRUE)
#' @param overwrite Skriv över om fil finns? (default: FALSE)
#' @param verbose Visa meddelanden? (default: TRUE)
#' 
#' @return Invisibly returnerar sökväg(ar) till sparad(e) fil(er)
#' 
#' @examples
#' # EXEMPEL 1: Spara som PNG (auto-detect från extension)
#' p <- create_static_map(...)
#' save_map(p, "befolkning.png")
#' 
#' # EXEMPEL 2: Specificera format
#' save_map(p, "befolkning", format = "pdf")
#' 
#' # EXEMPEL 3: Batch export (flera format)
#' save_map(p, "befolkning", format = c("png", "pdf", "svg"))
#' 
#' # EXEMPEL 4: Interaktiv karta
#' p <- create_interactive_map(...)
#' save_map(p, "befolkning.html")
#' 
#' # EXEMPEL 5: Custom storlek och DPI
#' save_map(p, "poster.png", width = 16, height = 12, dpi = 600)
#' 
#' @export
save_map <- function(plot,
                     filename,
                     format = "auto",
                     width = 10,
                     height = 8,
                     dpi = 300,
                     path = "output/maps",
                     self_contained = TRUE,
                     overwrite = FALSE,
                     verbose = TRUE) {
  
  # ==========================================================================
  # STEG 1: VALIDERA INPUT
  # ==========================================================================
  
  if (missing(plot)) {
    stop("Du måste ange ett plot-objekt (ggplot eller girafe)")
  }
  
  if (missing(filename)) {
    stop("Du måste ange ett filnamn")
  }
  
  # Identifiera plot-typ
  # VARFÖR kolla typ?: Olika funktioner för statiska vs interaktiva
  is_interactive <- inherits(plot, "girafe")
  is_static <- inherits(plot, "ggplot") || inherits(plot, "gg")
  
  if (!is_interactive && !is_static) {
    stop("plot måste vara ett ggplot eller girafe objekt.\n",
         "  Hittade: ", class(plot)[1])
  }
  
  # ==========================================================================
  # STEG 2: BESTÄM FORMAT
  # ==========================================================================
  
  # Auto-detect från filnamn extension
  # VARFÖR auto?: Bekvämt att bara skriva "karta.png"
  if (length(format) == 1 && format == "auto") {
    ext <- tools::file_ext(filename)
    if (ext == "") {
      # Ingen extension → default baserat på plot-typ
      format <- if (is_interactive) "html" else "png"
      if (verbose) message("Ingen extension angiven, använder: ", format)
    } else {
      format <- tolower(ext)
    }
  }
  
  # Validera format
  valid_formats <- c("png", "pdf", "svg", "html")
  invalid <- setdiff(format, valid_formats)
  if (length(invalid) > 0) {
    stop("Ogiltiga format: ", paste(invalid, collapse = ", "), "\n",
         "  Tillåtna: ", paste(valid_formats, collapse = ", "))
  }
  
  # Kolla kompatibilitet
  # VARFÖR?: Interaktiva kartor kan bara bli HTML
  if (is_interactive && any(!format %in% "html")) {
    stop("Interaktiva kartor (girafe) kan bara sparas som HTML.\n",
         "  Du försökte: ", paste(format, collapse = ", "))
  }
  
  # ==========================================================================
  # STEG 3: FÖRBERED FILNAMN OCH MAPP
  # ==========================================================================
  
  # Ta bort extension från filename om det finns
  # VARFÖR?: Vi lägger till rätt extension själva
  filename_base <- tools::file_path_sans_ext(filename)
  
  # Skapa output-mapp om den inte finns
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
    if (verbose) message("Skapade mapp: ", path)
  }
  
  # ==========================================================================
  # STEG 4: SPARA I VARJE FORMAT
  # ==========================================================================
  
  saved_files <- character()
  
  for (fmt in format) {
    # Bygg fullständig sökväg
    full_filename <- paste0(filename_base, ".", fmt)
    full_path <- file.path(path, full_filename)
    
    # Kolla om fil finns
    if (file.exists(full_path) && !overwrite) {
      warning("Filen '", full_path, "' finns redan. Hoppar över.\n",
              "  Använd overwrite = TRUE för att skriva över.")
      next
    }
    
    # Spara baserat på format
    # VARFÖR olika funktioner?: Olika verktyg för olika format
    
    if (fmt == "png") {
      # PNG: raster-format, bra för webb och rapporter
      ggsave(
        filename = full_path,
        plot = plot,
        width = width,
        height = height,
        dpi = dpi,
        device = "png",
        bg = "white"  # Vit bakgrund (inte transparent)
      )
      
    } else if (fmt == "pdf") {
      # PDF: vektor-format, perfekt för print
      ggsave(
        filename = full_path,
        plot = plot,
        width = width,
        height = height,
        device = "pdf"
      )
      
    } else if (fmt == "svg") {
      # SVG: vektor-format, bra för webb och Illustrator
      ggsave(
        filename = full_path,
        plot = plot,
        width = width,
        height = height,
        device = "svg"
      )
      
    } else if (fmt == "html") {
      # HTML: interaktiva kartor
      if (!is_interactive) {
        warning("Statiska kartor (ggplot) kan inte sparas som HTML. Hoppar över.")
        next
      }
      
      # Spara med htmlwidgets
      # VARFÖR htmlwidgets?: Standard för att spara HTML widgets
      htmlwidgets::saveWidget(
        widget = plot,
        file = normalizePath(full_path, mustWork = FALSE),
        selfcontained = self_contained,
        title = tools::file_path_sans_ext(full_filename)
      )
    }
    
    # Lägg till i sparade filer
    saved_files <- c(saved_files, full_path)
    
    if (verbose) {
      file_size <- file.size(full_path) / 1024  # KB
      message("✓ Sparad: ", full_path, " (", round(file_size), " KB)")
    }
  }
  
  # ==========================================================================
  # STEG 5: RETURNERA
  # ==========================================================================
  
  if (length(saved_files) == 0) {
    warning("Inga filer sparades")
    return(invisible(NULL))
  }
  
  if (verbose && length(saved_files) > 1) {
    message("\n✓ Totalt ", length(saved_files), " fil(er) sparade")
  }
  
  invisible(saved_files)
}


#' Spara senast skapade karta
#' 
#' VARFÖR save_last_map?: Convenience-funktion för att snabbt spara
#' utan att behöva referera till plot-objektet.
#' 
#' @param filename Filnamn (NULL = auto-genererat med timestamp)
#' @param format Format (default: "png")
#' @param ... Övriga argument till save_map()
#' 
#' @return Invisibly returnerar sökväg till sparad fil
#' 
#' @examples
#' # Skapa karta
#' create_static_map(...)
#' 
#' # Spara direkt
#' save_last_map()  # → map_2024-12-07_14-30-45.png
#' 
#' # Med custom namn
#' save_last_map("befolkning.pdf")
#' 
#' @export
save_last_map <- function(filename = NULL,
                          format = "png",
                          ...) {
  
  # Försök hämta senaste plot
  # VARFÖR last_plot()?: ggplot2's funktion för att få senaste plot
  plot <- tryCatch(
    last_plot(),
    error = function(e) NULL
  )
  
  if (is.null(plot)) {
    stop("Kunde inte hitta något plot att spara.\n",
         "  Skapa en karta först med create_static_map() eller create_interactive_map()")
  }
  
  # Auto-generera filnamn om NULL
  # VARFÖR timestamp?: Undvik att skriva över tidigare kartor
  if (is.null(filename)) {
    timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
    filename <- paste0("map_", timestamp)
    message("Auto-genererat filnamn: ", filename, ".", format)
  }
  
  # Spara
  save_map(
    plot = plot,
    filename = filename,
    format = format,
    ...
  )
}


#' Exportera till PowerPoint (framtida feature)
#' 
#' @keywords internal
save_to_powerpoint <- function(plot, filename, ...) {
  # TODO: Implementera med officer-paketet
  stop("PowerPoint export ännu inte implementerat.\n",
       "  Använd save_map() för att spara som PNG/PDF och infoga manuellt.")
}


#' Exportera till Google Drive (framtida feature)
#' 
#' @keywords internal
save_to_drive <- function(plot, filename, folder = NULL, ...) {
  # TODO: Implementera med googledrive-paketet
  stop("Google Drive export ännu inte implementerat.\n",
       "  Använd save_map() för att spara lokalt först.")
}


#' Hjälpfunktion: Rekommenderade inställningar per användningsfall
#' 
#' @export
export_presets <- function() {
  list(
    # Webb
    web = list(
      format = "png",
      width = 10,
      height = 8,
      dpi = 150
    ),
    
    # Print (hög kvalitet)
    print = list(
      format = "pdf",
      width = 10,
      height = 8
    ),
    
    # Poster/presentation
    poster = list(
      format = "png",
      width = 16,
      height = 12,
      dpi = 600
    ),
    
    # Rapport
    report = list(
      format = c("png", "pdf"),
      width = 8,
      height = 6,
      dpi = 300
    ),
    
    # Social media
    social = list(
      format = "png",
      width = 8,
      height = 8,  # Kvadratisk
      dpi = 150
    ),
    
    # Interaktiv webb
    interactive = list(
      format = "html",
      width = 10,
      height = 8,
      self_contained = TRUE
    )
  )
}


#' Använd preset
#' 
#' @export
#' @examples
#' p <- create_static_map(...)
#' save_with_preset(p, "karta", preset = "web")
save_with_preset <- function(plot, filename, preset = "web", ...) {
  presets <- export_presets()
  
  if (!preset %in% names(presets)) {
    stop("Okänd preset: ", preset, "\n",
         "  Tillgängliga: ", paste(names(presets), collapse = ", "))
  }
  
  settings <- presets[[preset]]
  
  # Merge med user-provided arguments (user overrides preset)
  user_args <- list(...)
  settings <- modifyList(settings, user_args)
  
  # Lägg till plot och filename
  settings$plot <- plot
  settings$filename <- filename
  
  # Anropa save_map med alla settings
  do.call(save_map, settings)
}
