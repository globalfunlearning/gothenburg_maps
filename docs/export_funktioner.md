# 📖 Export och Kartfunktioner

Funktioner för att exportera kartor och skapa kompletta statiska/interaktiva kartor med en funktion.

---

## Innehållsförteckning

### Export (export.R)
1. [save_map()](#save_map) - Spara karta till fil
2. [save_map_formats()](#save_map_formats) - Spara i flera format samtidigt
3. [create_output_path()](#create_output_path) - Skapa output-sökväg

### Statiska kartor (map_static.R)
4. [create_static_map()](#create_static_map) - Skapa komplett statisk karta
5. [add_map_layers()](#add_map_layers) - Lägg till extra kartlager
6. [add_scale_bar()](#add_scale_bar) - Lägg till skala

### Interaktiva kartor (map_interactive.R)
7. [create_interactive_map()](#create_interactive_map) - Skapa interaktiv webbkarta
8. [add_tooltip()](#add_tooltip) - Anpassa tooltips
9. [save_interactive()](#save_interactive) - Spara interaktiv karta som HTML

---

# EXPORT.R

## save_map() {#save_map}

### Beskrivning

Sparar en ggplot-karta till fil med standardiserade inställningar enligt Göteborgs Stads grafiska profil.

### Syntax

```r
save_map(
  plot,
  filename,
  width = 12,
  height = 8,
  dpi = 300,
  format = "png",
  path = "output/maps"
)
```

### Parametrar

- **plot** (ggplot): ggplot-objekt att spara
- **filename** (character): Filnamn (utan sökväg, med eller utan filändelse)
- **width** (numeric): Bredd i tum (default: 12)
- **height** (numeric): Höjd i tum (default: 8)
- **dpi** (numeric): Upplösning i dots per inch (default: 300)
- **format** (character): Filformat - "png", "pdf", "svg", "jpg" (default: "png")
- **path** (character): Output-mapp (default: "output/maps")

### Returnerar

Sökväg till sparad fil (invisible)

### Exempel

```r
# Skapa karta
p <- ggplot(karta, aes(fill = befolkning)) +
  geom_sf() +
  scale_fill_gbg_sequential("blue", n = 5) +
  theme_gothenburg_map()

# Spara som PNG (standard)
save_map(p, "befolkning_gbg.png")
# → Sparad: output/maps/befolkning_gbg.png

# Spara som PDF
save_map(p, "befolkning_gbg.pdf", format = "pdf")
# → Sparad: output/maps/befolkning_gbg.pdf

# Anpassad storlek för A4-utskrift
save_map(
  p,
  "befolkning_a4.png",
  width = 8.27,   # A4-bredd i tum
  height = 11.69, # A4-höjd i tum
  dpi = 300
)

# Hög upplösning för trycksak
save_map(p, "befolkning_print.png", dpi = 600)

# Annan output-mapp
save_map(p, "test.png", path = "drafts")
```

### Format och användning

| Format | Användning | DPI-rekommendation |
|--------|-----------|-------------------|
| **PNG** | Webb, rapporter, allmänt | 150-300 |
| **PDF** | Vektorgrafik, utskrift | 300 |
| **SVG** | Webb, editerbara vektorer | - (vektor) |
| **JPG** | Webb (små filer) | 72-150 |

### Best Practices

#### 1. Standardstorlekar

```r
# Webbkarta (bred)
save_map(p, "webb.png", width = 12, height = 8, dpi = 150)

# A4-utskrift (stående)
save_map(p, "a4.pdf", width = 8.27, height = 11.69, dpi = 300)

# Presentation (16:9)
save_map(p, "presentation.png", width = 10, height = 5.625, dpi = 150)

# Instagram (1:1)
save_map(p, "instagram.png", width = 8, height = 8, dpi = 150)
```

#### 2. Val av format

```r
# Webb/digital → PNG
save_map(p, "webb.png", dpi = 150)

# Utskrift/trycksak → PDF
save_map(p, "tryck.pdf", dpi = 300)

# Editerbar vektor → SVG
save_map(p, "editerbar.svg")

# Liten filstorlek → JPG
save_map(p, "liten.jpg", dpi = 72)
```

#### 3. Namngivning

```r
# BRA: Beskrivande namn
save_map(p, "befolkning_goteborg_2025.png")
save_map(p, "inkomst_deso_gr.pdf")

# UNDVIK: Vaga namn
save_map(p, "karta1.png")
save_map(p, "test.png")
```

---

## save_map_formats() {#save_map_formats}

### Beskrivning

Sparar samma karta i flera format samtidigt. Användbart när du behöver både webb-version (PNG) och print-version (PDF).

### Syntax

```r
save_map_formats(
  plot,
  basename,
  formats = c("png", "pdf"),
  width = 12,
  height = 8,
  dpi = 300,
  path = "output/maps"
)
```

### Parametrar

- **plot** (ggplot): ggplot-objekt att spara
- **basename** (character): Basnamn (utan filändelse)
- **formats** (character): Vektor med format att spara (default: c("png", "pdf"))
- **width**, **height**, **dpi**, **path**: Samma som `save_map()`

### Returnerar

Named list med sökvägar till sparade filer

### Exempel

```r
# Skapa karta
p <- ggplot(karta, aes(fill = befolkning)) +
  geom_sf() +
  scale_fill_gbg_sequential("blue") +
  theme_gothenburg_map()

# Spara i både PNG och PDF
files <- save_map_formats(p, "befolkning_gbg", formats = c("png", "pdf"))
# → Sparad: output/maps/befolkning_gbg.png
# → Sparad: output/maps/befolkning_gbg.pdf

# Spara i alla format
files <- save_map_formats(
  p,
  "befolkning_komplett",
  formats = c("png", "pdf", "svg", "jpg")
)

# Webb + Print med olika DPI
save_map_formats(p, "befolkning_webb", formats = "png", dpi = 150)
save_map_formats(p, "befolkning_print", formats = "pdf", dpi = 600)
```

---

## create_output_path() {#create_output_path}

### Beskrivning

Skapar output-sökväg med automatisk mappstruktur baserat på datum eller kategori.

### Syntax

```r
create_output_path(
  filename,
  subfolder = NULL,
  base_path = "output/maps",
  create_dir = TRUE
)
```

### Parametrar

- **filename** (character): Filnamn
- **subfolder** (character): Undermapp (t.ex. datum, projekt) (default: NULL)
- **base_path** (character): Basmapp (default: "output/maps")
- **create_dir** (logical): Skapa mapp om den inte finns? (default: TRUE)

### Returnerar

Character string med fullständig sökväg

### Exempel

```r
# Enkel sökväg
path <- create_output_path("karta.png")
# → "output/maps/karta.png"

# Med undermapp
path <- create_output_path("karta.png", subfolder = "2025-12")
# → "output/maps/2025-12/karta.png"

# Projektspecifik mapp
path <- create_output_path("karta.png", subfolder = "projekt_a")
# → "output/maps/projekt_a/karta.png"

# Användning med save_map()
path <- create_output_path("befolkning.png", subfolder = format(Sys.Date(), "%Y-%m"))
save_map(p, "befolkning.png", path = dirname(path))
```

---

# MAP_STATIC.R

## create_static_map() {#create_static_map}

### Beskrivning

Skapar en komplett statisk karta med klassindelning, färgsättning och styling i en enda funktion. "Allt-i-ett"-funktion för snabb kartproduktion.

### Syntax

```r
create_static_map(
  stat_data,
  geo_layer,
  value_col,
  by,
  classify_method = "fisher",
  n_classes = 5,
  palette_type = "sequential",
  palette_name = "blue",
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  legend_title = NULL,
  legend_position = "right",
  add_water = FALSE,
  add_borders = FALSE,
  border_layer = NULL,
  label_decimals = 0,
  label_unit = "",
  save_to = NULL,
  ...
)
```

### Parametrar

#### Data och geografi
- **stat_data** (data.frame): Statistikdata
- **geo_layer** (character eller sf): Kartlager (namn för prepared map eller sf-objekt)
- **value_col** (character): Kolumnnamn för värde att kartlägga
- **by** (character): Kolumnnamn att matcha på

#### Klassindelning
- **classify_method** (character): Klassindelningsmetod - "fisher", "quantile", "equal", etc. (default: "fisher")
- **n_classes** (numeric): Antal klasser (default: 5)

#### Färgsättning
- **palette_type** (character): Paletttyp - "sequential", "diverging", "categorical" (default: "sequential")
- **palette_name** (character): Palettnamn - "blue", "green", "red_green", etc. (default: "blue")

#### Text
- **title** (character): Karttitel (default: NULL)
- **subtitle** (character): Undertitel (default: NULL)
- **caption** (character): Bildtext/källa (default: NULL)
- **legend_title** (character): Legend-titel (default: NULL)
- **legend_position** (character): Legend-position (default: "right")

#### Extra lager
- **add_water** (logical): Lägg till vattenlager? (default: FALSE)
- **add_borders** (logical): Lägg till gränslager? (default: FALSE)
- **border_layer** (character eller sf): Gränslager att använda

#### Formattering
- **label_decimals** (numeric): Decimaler i labels (default: 0)
- **label_unit** (character): Enhet för labels (default: "")

#### Export
- **save_to** (character): Filnamn att spara till (default: NULL = spara ej)
- **...**: Ytterligare argument till `save_map()`

### Returnerar

ggplot-objekt

### Exempel

#### Enkel karta

```r
# Minimal kod för komplett karta
karta <- create_static_map(
  stat_data = befolkning,
  geo_layer = "goteborg/deso_goteborg",
  value_col = "befolkning",
  by = "desokod",
  title = "Befolkning per DeSO-område",
  legend_title = "Invånare"
)

print(karta)
```

#### Med alla funktioner

```r
karta <- create_static_map(
  stat_data = befolkning,
  geo_layer = "goteborg/deso_goteborg",
  value_col = "forandring",
  by = "desokod",
  classify_method = "fisher",
  n_classes = 6,
  palette_type = "diverging",
  palette_name = "red_green",
  title = "Befolkningsförändring 2020-2025",
  subtitle = "DeSO-områden i Göteborg",
  caption = "Källa: SCB",
  legend_title = "Förändring",
  legend_position = "right",
  add_water = TRUE,
  add_borders = TRUE,
  border_layer = "goteborg/stadsdelar",
  label_decimals = 0,
  label_unit = "invånare",
  save_to = "befolkning_forandring.png",
  width = 12,
  height = 10,
  dpi = 300
)
```

#### Göteborgsregionen

```r
karta <- create_static_map(
  stat_data = inkomst_gr,
  geo_layer = "sverige/deso_gr",
  value_col = "medelinkomst",
  by = "desokod",
  classify_method = "quantile",
  n_classes = 5,
  palette_type = "sequential",
  palette_name = "green",
  title = "Medianinkomst - Göteborgsregionen",
  legend_title = "Inkomst (kr)",
  label_unit = "kr"
)
```

### Best Practices

```r
# 1. Sequential för värden låg→hög
create_static_map(
  ...,
  palette_type = "sequential",
  palette_name = "blue"
)

# 2. Diverging för avvikelser från centrum
create_static_map(
  ...,
  palette_type = "diverging",
  palette_name = "red_green"
)

# 3. Lägg alltid till källa
create_static_map(
  ...,
  caption = "Källa: SCB"
)

# 4. Spara direkt till fil
create_static_map(
  ...,
  save_to = "output.png",
  dpi = 300
)
```

---

## add_map_layers() {#add_map_layers}

### Beskrivning

Lägger till extra kartlager (vatten, gränser) till befintlig ggplot-karta.

### Syntax

```r
add_map_layers(
  plot,
  water = NULL,
  borders = NULL,
  water_fill = "#b3d9ff",
  water_color = NA,
  border_fill = NA,
  border_color = "black",
  border_width = 0.3
)
```

### Exempel

```r
# Bas-karta
p <- ggplot(deso_gbg, aes(fill = befolkning)) +
  geom_sf()

# Lägg till vatten och gränser
p <- add_map_layers(
  p,
  water = load_water_layer("goteborg/alv_goteborg"),
  borders = load_prepared_map("goteborg/stadsdelar")
)
```

---

## add_scale_bar() {#add_scale_bar}

### Beskrivning

Lägger till skala (måttstock) på karta.

### Syntax

```r
add_scale_bar(
  plot,
  location = "bottomleft",
  dist = 1,
  dist_unit = "km",
  transform = TRUE
)
```

### Exempel

```r
p + add_scale_bar(location = "bottomright", dist = 5, dist_unit = "km")
```

---

# MAP_INTERACTIVE.R

## create_interactive_map() {#create_interactive_map}

### Beskrivning

Skapar en interaktiv webbkarta med Leaflet. Användbart för webbrapporter, Quarto-dokument eller fristående HTML-filer.

### Syntax

```r
create_interactive_map(
  stat_data,
  geo_layer,
  value_col,
  by,
  classify_method = "fisher",
  n_classes = 5,
  palette_type = "sequential",
  palette_name = "blue",
  title = NULL,
  tooltip_cols = NULL,
  tooltip_alias = NULL,
  label_decimals = 0,
  label_unit = "",
  opacity = 0.7,
  highlight = TRUE,
  zoom_start = NULL,
  basemap = "CartoDB.Positron",
  save_to = NULL
)
```

### Parametrar

Liknar `create_static_map()` men med:

#### Interaktivitet
- **tooltip_cols** (character): Kolumner att visa i tooltip
- **tooltip_alias** (character): Alias för tooltip-kolumner
- **opacity** (numeric): Opacitet 0-1 (default: 0.7)
- **highlight** (logical): Highlighta vid hover? (default: TRUE)

#### Karta
- **zoom_start** (numeric): Initial zoom-nivå (default: NULL = auto)
- **basemap** (character): Bakgrundskarta (default: "CartoDB.Positron")
- **save_to** (character): Spara som HTML (default: NULL)

### Returnerar

Leaflet-objekt

### Exempel

#### Enkel interaktiv karta

```r
karta <- create_interactive_map(
  stat_data = befolkning,
  geo_layer = "goteborg/deso_goteborg",
  value_col = "befolkning",
  by = "desokod",
  title = "Befolkning per DeSO-område",
  tooltip_cols = c("desokod", "befolkning", "RegSO_2025"),
  tooltip_alias = c("DeSO-kod", "Befolkning", "RegSO")
)

# Visa i RStudio/browser
print(karta)
```

#### Spara som HTML

```r
karta <- create_interactive_map(
  stat_data = befolkning,
  geo_layer = "goteborg/deso_goteborg",
  value_col = "befolkning",
  by = "desokod",
  tooltip_cols = c("desokod", "befolkning"),
  tooltip_alias = c("Område", "Invånare"),
  save_to = "output/befolkning_interaktiv.html"
)
```

#### Göteborgsregionen

```r
karta <- create_interactive_map(
  stat_data = inkomst_gr,
  geo_layer = "sverige/deso_gr",
  value_col = "medelinkomst",
  by = "desokod",
  classify_method = "quantile",
  n_classes = 5,
  palette_type = "sequential",
  palette_name = "green",
  title = "Medianinkomst - Göteborgsregionen",
  tooltip_cols = c("desokod", "kommunnamn", "medelinkomst"),
  tooltip_alias = c("DeSO", "Kommun", "Medelinkomst"),
  label_unit = "kr",
  zoom_start = 10
)
```

### Basemaps

```r
# Ljus (default)
basemap = "CartoDB.Positron"

# Mörk
basemap = "CartoDB.DarkMatter"

# OpenStreetMap
basemap = "OpenStreetMap"

# Topografisk
basemap = "Esri.WorldTopoMap"
```

---

## add_tooltip() {#add_tooltip}

### Beskrivning

Anpassar tooltip-innehåll för interaktiva kartor.

### Syntax

```r
add_tooltip(
  map,
  data,
  cols,
  alias = NULL,
  style = "html"
)
```

### Exempel

```r
# Enkel tooltip
add_tooltip(karta, data, cols = c("namn", "värde"))

# Med alias
add_tooltip(
  karta,
  data,
  cols = c("desokod", "befolkning"),
  alias = c("DeSO-område", "Invånare")
)
```

---

## save_interactive() {#save_interactive}

### Beskrivning

Sparar interaktiv Leaflet-karta som HTML-fil.

### Syntax

```r
save_interactive(
  map,
  filename,
  path = "output/maps",
  selfcontained = TRUE,
  title = "Karta"
)
```

### Parametrar

- **map** (leaflet): Leaflet-karta
- **filename** (character): Filnamn (.html)
- **path** (character): Output-mapp
- **selfcontained** (logical): Inkludera allt i en fil? (default: TRUE)
- **title** (character): HTML-titel

### Exempel

```r
# Skapa karta
karta <- create_interactive_map(...)

# Spara
save_interactive(
  karta,
  "befolkning.html",
  path = "output/webb",
  title = "Befolkningskarta Göteborg"
)
```

---

## Snabbreferens

### Statisk karta (snabbt)

```r
create_static_map(
  stat_data = data,
  geo_layer = "goteborg/deso_goteborg",
  value_col = "värde",
  by = "kod",
  title = "Titel",
  legend_title = "Enhet",
  save_to = "karta.png"
)
```

### Interaktiv karta (snabbt)

```r
create_interactive_map(
  stat_data = data,
  geo_layer = "goteborg/deso_goteborg",
  value_col = "värde",
  by = "kod",
  title = "Titel",
  tooltip_cols = c("namn", "värde"),
  save_to = "karta.html"
)
```

### Export

```r
# Spara en karta
save_map(plot, "fil.png", width = 12, height = 8, dpi = 300)

# Spara i flera format
save_map_formats(plot, "fil", formats = c("png", "pdf"))
```

---

**Version:** 1.0  
**Uppdaterad:** 2025-12-16  
**För:** export.R, map_static.R, map_interactive.R
