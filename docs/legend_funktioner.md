# 📖 Legend - Skapa och anpassa kartlegender

Funktioner för att skapa, formattera och anpassa kartlegender enligt Göteborgs Stads designprinciper.

---

## Innehållsförteckning

1. [create_legend()](#create_legend) - Skapa anpassad legend
2. [legend_position()](#legend_position) - Beräkna legend-position
3. [format_legend_labels()](#format_legend_labels) - Formattera legend-etiketter
4. [remove_legend()](#remove_legend) - Ta bort legend från ggplot

---

## create_legend() {#create_legend}

### Beskrivning

Skapar en fullständigt anpassad legend för ggplot2-kartor enligt Göteborgs Stads grafiska profil. Ger kontroll över position, orientering, storlek, font och färger.

### Syntax

```r
create_legend(
  title = "Legend",
  position = "right",
  direction = "vertical",
  title_position = "top",
  label_position = "right",
  title_size = 12,
  text_size = 10,
  key_size = 1,
  key_height = NULL,
  key_width = NULL,
  spacing = NULL,
  background = "white",
  border_color = NA,
  title_hjust = 0,
  title_vjust = 0,
  nrow = NULL,
  ncol = NULL,
  byrow = FALSE,
  reverse = FALSE
)
```

### Parametrar

#### Grundläggande
- **title** (character): Legend-titel (default: "Legend")
- **position** (character): Position - "right", "left", "top", "bottom", "none", eller c(x, y) koordinater (default: "right")

#### Layout
- **direction** (character): Orientering - "vertical" eller "horizontal" (default: "vertical")
- **title_position** (character): Titel-position - "top", "bottom", "left", "right" (default: "top")
- **label_position** (character): Etikett-position - "right", "left", "top", "bottom" (default: "right")

#### Storlek
- **title_size** (numeric): Titelstorlek i punkter (default: 12)
- **text_size** (numeric): Textstorlek i punkter (default: 10)
- **key_size** (numeric): Nyckelstorlek i cm (default: 1)
- **key_height** (unit): Nyckelhöjd (default: NULL = automatisk)
- **key_width** (unit): Nyckelbredd (default: NULL = automatisk)
- **spacing** (unit): Avstånd mellan element (default: NULL = automatisk)

#### Utseende
- **background** (character): Bakgrundsfärg (default: "white")
- **border_color** (character): Ramfärg (default: NA = ingen ram)
- **title_hjust** (numeric): Horisontell justering av titel 0-1 (default: 0 = vänster)
- **title_vjust** (numeric): Vertikal justering av titel 0-1 (default: 0 = botten)

#### Grid
- **nrow** (numeric): Antal rader i legend-grid (default: NULL)
- **ncol** (numeric): Antal kolumner i legend-grid (default: NULL)
- **byrow** (logical): Fyll grid radvis? (default: FALSE = kolumnvis)
- **reverse** (logical): Omvänd ordning? (default: FALSE)

### Returnerar

Lista med theme-element för ggplot2

### Exempel

#### Standard legend (höger, vertikal)

```r
ggplot(karta, aes(fill = befolkning)) +
  geom_sf() +
  scale_fill_gbg_sequential("blue", n = 5) +
  create_legend(
    title = "Befolkning",
    position = "right"
  )
```

#### Horisontell legend (botten)

```r
ggplot(karta, aes(fill = forandring)) +
  geom_sf() +
  scale_fill_gbg_diverging("red_green", n = 5) +
  create_legend(
    title = "Förändring (%)",
    position = "bottom",
    direction = "horizontal",
    title_position = "top"
  )
```

#### Exakt position (koordinater)

```r
ggplot(karta, aes(fill = inkomst)) +
  geom_sf() +
  scale_fill_gbg_sequential("green", n = 5) +
  create_legend(
    title = "Medianinkomst",
    position = c(0.85, 0.25),  # x=85%, y=25% från nedre vänstra hörnet
    direction = "vertical"
  )
```

#### Stor legend med anpassade storlekar

```r
ggplot(karta, aes(fill = kategori)) +
  geom_sf() +
  scale_fill_gbg_categorical() +
  create_legend(
    title = "Stadsdelskategori",
    title_size = 14,
    text_size = 11,
    key_size = 1.2,
    spacing = unit(0.5, "cm")
  )
```

#### Ingen legend

```r
ggplot(karta, aes(fill = varde)) +
  geom_sf() +
  scale_fill_gbg_sequential("blue") +
  create_legend(position = "none")
```

### Vanliga positioner

```r
# Standardpositioner
position = "right"    # Höger (default)
position = "left"     # Vänster
position = "top"      # Topp
position = "bottom"   # Botten
position = "none"     # Ingen legend

# Exakta koordinater (x, y) där 0,0 = nedre vänstra hörnet
position = c(0.85, 0.75)  # Höger övre hörn
position = c(0.15, 0.85)  # Vänster övre hörn
position = c(0.50, 0.05)  # Centrerad botten
```

### Best Practices

#### 1. Vertikal legend för sekventiella skalor

```r
# BRA: Vertikal legend för gradient (lättare att läsa)
create_legend(
  title = "Befolkning",
  position = "right",
  direction = "vertical"
)
```

#### 2. Horisontell legend för kategorier

```r
# BRA: Horisontell legend för få kategorier (sparar plats)
create_legend(
  title = "Stadsdelskategori",
  position = "bottom",
  direction = "horizontal",
  nrow = 1
)
```

#### 3. Anpassa till kartans orientering

```r
# Bred karta → legend till höger eller botten
# Hög karta → legend till höger eller topp

# Bred karta
ggplot(karta_bred, aes(fill = varde)) +
  geom_sf() +
  create_legend(position = "bottom", direction = "horizontal")

# Hög karta
ggplot(karta_hog, aes(fill = varde)) +
  geom_sf() +
  create_legend(position = "right", direction = "vertical")
```

#### 4. Placera utanför kartan vid exakta koordinater

```r
# FEL: Legend täcker kartan
create_legend(position = c(0.5, 0.5))  # Mitt på kartan!

# BRA: Legend i hörn eller kant
create_legend(position = c(0.85, 0.85))  # Övre högra hörn
create_legend(position = c(0.12, 0.12))  # Nedre vänstra hörn
```

### Kombinera med theme_gothenburg_map()

```r
ggplot(karta, aes(fill = befolkning)) +
  geom_sf() +
  scale_fill_gbg_sequential("blue", n = 5) +
  create_legend(
    title = "Befolkning",
    position = "right",
    title_size = 12,
    text_size = 10
  ) +
  theme_gothenburg_map(
    legend_position = "right"  # Matcha med create_legend()
  )
```

---

## legend_position() {#legend_position}

### Beskrivning

Hjälpfunktion för att beräkna exakta legend-positioner baserat på fördefinierade lägen (t.ex. "top-right", "bottom-left").

### Syntax

```r
legend_position(position = "top-right", offset_x = 0.02, offset_y = 0.02)
```

### Parametrar

- **position** (character): Fördefinierat läge - "top-right", "top-left", "bottom-right", "bottom-left", "center"
- **offset_x** (numeric): X-offset från kanten (0-1, default: 0.02)
- **offset_y** (numeric): Y-offset från kanten (0-1, default: 0.02)

### Returnerar

Numeric vector c(x, y) med koordinater

### Exempel

```r
# Övre högra hörnet med 2% margin
pos <- legend_position("top-right")
# → c(0.98, 0.98)

ggplot(karta, aes(fill = varde)) +
  geom_sf() +
  create_legend(position = pos)

# Anpassad margin
pos <- legend_position("bottom-left", offset_x = 0.05, offset_y = 0.05)
# → c(0.05, 0.05)
```

### Fördefinierade lägen

```r
legend_position("top-right")     # c(0.98, 0.98)
legend_position("top-left")      # c(0.02, 0.98)
legend_position("bottom-right")  # c(0.98, 0.02)
legend_position("bottom-left")   # c(0.02, 0.02)
legend_position("center")        # c(0.50, 0.50)
```

---

## format_legend_labels() {#format_legend_labels}

### Beskrivning

Formaterar legend-etiketter med enheter, decimaler och prefix/suffix.

### Syntax

```r
format_legend_labels(
  labels,
  unit = "",
  decimals = 0,
  separator = " ",
  prefix = "",
  suffix = "",
  thousands_sep = " "
)
```

### Parametrar

- **labels** (character): Etiketter att formattera
- **unit** (character): Enhet (t.ex. "kr", "%", "km²")
- **decimals** (numeric): Antal decimaler (default: 0)
- **separator** (character): Separator mellan värde och enhet (default: " ")
- **prefix** (character): Prefix före värde (default: "")
- **suffix** (character): Suffix efter enhet (default: "")
- **thousands_sep** (character): Tusentalsavgränsare (default: " ")

### Returnerar

Character vector med formaterade etiketter

### Exempel

```r
# Enkla tal
labels <- c("1000", "2000", "3000")
format_legend_labels(labels, unit = "kr")
# → "1 000 kr", "2 000 kr", "3 000 kr"

# Procent med decimaler
labels <- c("10.5", "20.8", "31.2")
format_legend_labels(labels, unit = "%", decimals = 1)
# → "10.5 %", "20.8 %", "31.2 %"

# Prefix (t.ex. "<" för intervall)
labels <- c("0", "1000", "2000")
format_legend_labels(labels, prefix = "<", unit = "kr")
# → "< 0 kr", "< 1 000 kr", "< 2 000 kr"

# Kombinera med create_legend()
breaks <- c(0, 1000, 2000, 3000)
labels <- format_legend_labels(
  as.character(breaks),
  unit = "kr",
  thousands_sep = " "
)

ggplot(karta, aes(fill = inkomst_klass)) +
  geom_sf() +
  scale_fill_manual(values = goteborg_sequential_blue(4), labels = labels) +
  create_legend(title = "Medianinkomst")
```

---

## remove_legend() {#remove_legend}

### Beskrivning

Snabbfunktion för att ta bort legend från ggplot. Ekvivalent med `create_legend(position = "none")`.

### Syntax

```r
remove_legend()
```

### Returnerar

theme-element som tar bort legenden

### Exempel

```r
# Ta bort legend
ggplot(karta, aes(fill = varde)) +
  geom_sf() +
  scale_fill_gbg_sequential("blue") +
  remove_legend()

# Samma som
ggplot(karta, aes(fill = varde)) +
  geom_sf() +
  scale_fill_gbg_sequential("blue") +
  create_legend(position = "none")

# Eller
ggplot(karta, aes(fill = varde)) +
  geom_sf() +
  scale_fill_gbg_sequential("blue") +
  theme(legend.position = "none")
```

### När ta bort legend?

- **Enkel explorativ karta** (bara för dig själv)
- **Karta med titel som förklarar allt** (t.ex. "Göteborg har flest invånare")
- **Flera små kartor** (facets) med gemensam legend
- **Interaktiv karta** med tooltips (legenden kan vara överflödig)

```r
# Exempel: Small multiples utan individuella legender
ggplot(karta, aes(fill = varde)) +
  geom_sf() +
  facet_wrap(~ar) +
  scale_fill_gbg_sequential("blue") +
  remove_legend() +  # Gemensam legend istället
  labs(title = "Befolkningsutveckling över tid")
```

---

## Snabbreferens

### Vanliga legend-konfigurationer

```r
# Standard: Höger, vertikal
create_legend(title = "Titel", position = "right")

# Botten: Horisontell
create_legend(
  title = "Titel",
  position = "bottom",
  direction = "horizontal"
)

# Exakt position: Övre högra hörn
create_legend(
  title = "Titel",
  position = c(0.85, 0.85)
)

# Ingen legend
create_legend(position = "none")
# eller
remove_legend()
```

### Formattera etiketter

```r
# Pengar
format_legend_labels(labels, unit = "kr", thousands_sep = " ")

# Procent
format_legend_labels(labels, unit = "%", decimals = 1)

# Område
format_legend_labels(labels, unit = "km²", decimals = 2)

# Prefix
format_legend_labels(labels, prefix = "<", unit = "kr")
```

### Positionering

```r
# Hjälpfunktion
legend_position("top-right")
legend_position("bottom-left", offset_x = 0.05, offset_y = 0.05)

# Manuellt
position = c(0.85, 0.85)  # x=85%, y=85%
```

---

**Version:** 1.0  
**Uppdaterad:** 2025-12-16  
**För:** legend.R
