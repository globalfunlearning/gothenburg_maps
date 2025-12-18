# Gothenburg Maps with R

Funktioner för att skapa kartor enligt Göteborgs Stads grafiska profil.

## 📦 Innehåll

- **39 funktioner** för kartproduktion
- **Kartlager** för Göteborg och Sverige
- **Exempeldata** för att komma igång
- **Theme** för ggplot2-kartor
- **Färgsystem** enligt grafisk profil
- **DeSO/RegSO-stöd** för SCB-statistik

**OBS om DeSO/RegSO:**
- Förberedda lager (`deso_goteborg`, `regso_goteborg`, `deso_gr`, `regso_gr`) är anpassade med havslager för snyggare kartor
- Vill du ha originalgränser med vatten? Använd `load_deso_from_scb()` och `load_regso_from_scb()`

## 🚀 Snabbstart

```r
# Ladda alla funktioner
source("R/00_setup.R")

# Ladda färgpaletter
source("path/to/gothenburg_colors/colors.R")

# Skapa en enkel karta
primaromraden <- load_prepared_map("goteborg/primaromraden")

ggplot(primaromraden) +
  geom_sf(fill = gbg_colors("blue")) +
  theme_gothenburg_map()
```

## 📚 Arbetsflöde

### 1. Ladda kartlager

```r
# Förberedda kartlager
primaromraden <- load_prepared_map("goteborg/primaromraden")
stadsdelar <- load_prepared_map("goteborg/stadsdelar")
kommuner <- load_prepared_map("sverige/kommuner")

# Vattenlager
alv <- load_water_layer("goteborg/alv_goteborg")
```

**Tillgängliga lager:**

- **Göteborg:** 
primaromraden, stadsdelar, basomraden, kommungrans

- **Sverige:** 
kommuner, lan, regioner, vatten, kust

#### Hämta DeSO/RegSO från SCB
```r
# Hämta aktuella DeSO-områden för Göteborg
deso <- load_deso_from_scb(municipality_codes = "1480", year = 2025)

# Hämta RegSO för GR-kommunerna
regso_gr <- load_regso_from_scb(
  municipality_codes = c("1480", "1481", "1482", "1483", "1484", 
                         "1485", "1486", "1487", "1488", "1489",
                         "1490", "1491", "1492"),
  year = 2025
)

# Hämta koppling mellan DeSO och RegSO
koppling <- load_deso_regso_koppling(municipality_codes = "1480")
```

**Tillgängliga förberedda lager:**
- `goteborg/deso` - DeSO Göteborg (anpassad för havslager)
- `goteborg/regso` - RegSO Göteborg (anpassad för havslager)
- `goteborg/deso_gr` - DeSO GR (OBS: anpassad för kommungränser utan hav)
- `goteborg/regso_gr` - RegSO GR (OBS: anpassad för kommungränser utan hav)

### 2. Koppla statistik

```r
# Läs data
befolkning <- readRDS("examples/data/gbg_primaromrade.rds")
primaromraden <- load_prepared_map("goteborg/primaromraden")

# Koppla till karta
karta <- join_stat_to_map(
  map_data = primaromraden,
  stat_data = befolkning,
  by = "omrade_kod"
)
```

### 3. Skapa klassgränser

```r
# Olika metoder
breaks <- create_breaks(karta$forandring, "fisher", 5)
labels <- create_labels(breaks, "range", decimals = 0)
karta$klass <- apply_classification(karta$forandring, breaks, labels)
```

**Tillgängliga metoder:**

| Metod | Användning | Antal klasser |
|:------|:-----------|:--------------|
| **fisher** | Natural breaks (rekommenderad) | Du väljer |
| **quantile** | Lika många per klass (bra för skev data) | Du väljer |
| **equal** | Lika stora intervall | Du väljer |
| **pretty** | Snygga avrundade tal | Du väljer |
| **jenks** | Natural breaks (använd fisher istället) | Du väljer |
| **kmeans** | K-means clustering | Du väljer |
| **hclust** | Hierarkisk clustering | Du väljer |
| **sd** | Standardavvikelse | Du väljer |
| **dpih** | Histogram-baserad | Automatiskt |
| **headtails** | För heavy-tailed data | Automatiskt |
| **maximum** | Hittar största hoppen | Automatiskt |
| **box** | Boxplot-struktur | Alltid 6 |
| **manual** | Egna gränsvärden | Du väljer |

### 4. Skapa karta

**Med ggplot2 (utnyttja theme_gothenburg_map):**

```r
ggplot(karta, aes(fill = klass)) +
  geom_sf(color = "white", linewidth = 0.1) +
  scale_fill_gbg_sequential("blue", n = 5) +
  labs(
    title = "Befolkningsförändring per primärområde",
    caption = "Källa: Västfolket och Göteborgs Stad",
    fill = "Befolkningsförändring t.om oktober 2025"
  ) +
  theme_gothenburg_map()
```

**Med create_static_map() (snabbanalys):**

```r
karta <- create_static_map(
  stat_data = befolkning,
  geo_layer = primaromraden,
  value_col = "forandring",
  by = "omrade_kod",
  classify_method = "fisher",
  n_classes = 5,
  palette_type = "sequential",
  palette_name = "blue"
)
```

**Med create_interactive_map() (för webb eller quartorapport):**

```r
karta_webb <- create_interactive_map(
  stat_data = befolkning,
  geo_layer = primaromraden,
  value_col = "forandring",
  by = "omrade_kod",
  tooltip_cols = c("omrade_namn", "forandring"),
  tooltip_alias = c("Område", "Förändring")
)
```

## 🎨 Färgsystem

Använder färgpaletter från `gothenburg_colors`:

```r
# Sequential
scale_fill_gbg_sequential("blue", n = 5)
scale_fill_gbg_sequential("green", n = 7)

# Diverging
scale_fill_gbg_diverging("red_green", n = 5)
scale_fill_gbg_diverging("blue_red", n = 7)

# Categorical
scale_fill_gbg_categorical("palette_4", n = 4)
```

**Tillgängliga paletter:**

- **Sequential:** blue, green, yellow_red, dark_blue, cyan, yellow, red, pink, purple, yellow_green, yellow_green_dark

- **Diverging:** red_green, blue_red, blue_brown, purple_green, pink_green

- **Categorical:** palette_3, palette_4, palette_5, palette_6, palette_7

## 🎯 Theme

`theme_gothenburg_map()` ger kartor enligt grafisk profil:

```r
# Default: horisontell legend längst ner
+ theme_gothenburg_map()

# Vertikal legend
theme_gothenburg_map(legend_direction = "vertical")

# Flytta legend
theme_gothenburg_map(legend_position = c(0.98, 0.98))

# Ändra bakgrund
theme_gothenburg_map(panel_background = "#f5f5f5")
```

## 💡 Hjälpfunktioner

```r
# Testa klassindelning
summarize_for_breaks(befolkning$forandring, n_classes = 5)
compare_methods(befolkning$forandring, methods = c("fisher", "quantile", "equal"))

# Lista kartlager
list_prepared_maps()

# Spara karta
save_map(karta, "befolkning.png", dpi = 300)
```

## 📁 Projektstruktur

```
gothenburg_maps/
├── R/                      # Alla funktioner
│   ├── 00_setup.R
│   ├── geodata.R
│   ├── classify.R
│   ├── themes.R
│   └── ...
├── input/
│   └── prepared_maps/      # Kartlager
│       ├── goteborg/
│       └── sverige/
├── output/
│   └── maps/               # Sparade kartor
├── examples/
│   ├── data/               # Exempeldata
│   └── kartor/             # Exempelskript
└── docs/                   # Dokumentation
```

## 📖 Dokumentation

- **SNABBSTART.md** - Konkreta scenarier
- **funktioner_katalog.md** - Fullständig funktionsreferens

## 🔧 Installation

```r
# Nödvändiga paket
install.packages(c("sf", "dplyr", "ggplot2", "tidyr", "classInt"))

# För interaktiva kartor
install.packages(c("ggiraph", "leaflet"))

# För Open Sans typsnitt
install.packages("systemfonts")
```

## 💾 Spara kartor

```r
# PNG
ggsave("output/maps/karta.png", karta, width = 12, height = 8, dpi = 300)

# PDF
ggsave("output/maps/karta.pdf", karta, width = 297, height = 210, units = "mm")

# Interaktiv HTML
htmlwidgets::saveWidget(karta_webb, "output/maps/karta.html")
```

## 📊 Tips

**Välj rätt klassindelning:**

- **fisher** - Bästa natural breaks, använd som standard

- **quantile** - Lika många per klass, bra för skev data

- **equal** - Enkelt att förstå, bra för presentation

- **pretty** - Snygga avrundade tal

- **dpih** eller **headtails** - Osäker på antal klasser? OBS: dpih kan ge många klasser (100+) för stora dataset, bättre för histogram än kartor

**Välj rätt färgpalett:**

- Sequential för värden låg → hög

- Diverging för avvikelser från neutral punkt

- Categorical för kategorier utan ordning

**Antal klasser:**

- 5-6 klasser är standard

- Max 7 för categorical

- 3-11 för sequential/diverging

- dpih/headtails/box väljer själva (kan bli många!)

---

**Version:** 1.0  
**Uppdaterad:** 2025-12-11