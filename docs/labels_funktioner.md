# 📖 Labels - Skapa och formattera kartetiketter

Funktioner för att skapa etiketter för klassificerade värden, med flexibla format för intervall, punktvärden och custom labels.

---

## Innehållsförteckning

1. [create_labels()](#create_labels) - Huvudfunktion för att skapa etiketter
2. [format_range()](#format_range) - Formattera intervall (t.ex. "100-200")
3. [format_ruler()](#format_ruler) - Formattera linjal-stil (t.ex. "< 100")
4. [format_exact()](#format_exact) - Formattera exakta värden
5. [add_units()](#add_units) - Lägg till enheter till etiketter

---

## create_labels() {#create_labels}

### Beskrivning

Huvudfunktion för att skapa etiketter från klassindelningsbreak-points. Stöder olika format: intervall ("100-200"), linjal ("< 100"), exakta värden och custom labels.

### Syntax

```r
create_labels(
  breaks,
  style = "range",
  decimals = 0,
  unit = "",
  separator = "-",
  prefix = "",
  suffix = "",
  include_max = TRUE,
  custom_labels = NULL
)
```

### Parametrar

#### Grundläggande
- **breaks** (numeric): Break-points från klassindelning
- **style** (character): Etikettsstil - "range", "ruler", "exact", "custom" (default: "range")

#### Formattering
- **decimals** (numeric): Antal decimaler (default: 0)
- **unit** (character): Enhet (t.ex. "kr", "%", "km²") (default: "")
- **separator** (character): Separator för intervall (default: "-")
- **prefix** (character): Prefix före värde (default: "")
- **suffix** (character): Suffix efter enhet (default: "")

#### Beteende
- **include_max** (logical): Inkludera max-värde som sista etikett? (default: TRUE)
- **custom_labels** (character): Egna etiketter (endast om style = "custom")

### Returnerar

Character vector med etiketter (längd = length(breaks) - 1 eller length(custom_labels))

### Exempel

#### Style: "range" (intervall)

```r
breaks <- c(0, 1000, 2000, 3000, 4000)

# Enkelt intervall
labels <- create_labels(breaks, style = "range")
# → "0-1000", "1000-2000", "2000-3000", "3000-4000"

# Med enhet
labels <- create_labels(breaks, style = "range", unit = "kr")
# → "0-1000 kr", "1000-2000 kr", "2000-3000 kr", "3000-4000 kr"

# Med decimaler och annan separator
labels <- create_labels(
  breaks,
  style = "range",
  unit = "kr",
  decimals = 0,
  separator = " till "
)
# → "0 till 1000 kr", "1000 till 2000 kr", ...

# På karta
karta$klass <- apply_classification(karta$inkomst, breaks, labels)

ggplot(karta, aes(fill = klass)) +
  geom_sf() +
  scale_fill_gbg_sequential("blue", n = 4) +
  labs(fill = "Medianinkomst")
```

#### Style: "ruler" (linjal med </>)

```r
breaks <- c(0, 1000, 2000, 3000, 4000)

# Linjal-stil
labels <- create_labels(breaks, style = "ruler", unit = "kr")
# → "< 1000 kr", "< 2000 kr", "< 3000 kr", "< 4000 kr"

# På karta
karta$klass <- apply_classification(karta$inkomst, breaks, labels)

ggplot(karta, aes(fill = klass)) +
  geom_sf() +
  scale_fill_gbg_sequential("green", n = 4) +
  labs(fill = "Upp till...")
```

#### Style: "exact" (exakta värden)

```r
breaks <- c(0, 1000, 2000, 3000, 4000)

# Exakta break-points
labels <- create_labels(breaks, style = "exact", unit = "kr")
# → "0 kr", "1000 kr", "2000 kr", "3000 kr", "4000 kr"

# Användbart för continuous scales
ggplot(karta, aes(fill = inkomst)) +
  geom_sf() +
  scale_fill_gradient(
    low = "#d1e5f0",
    high = "#0076bc",
    breaks = breaks,
    labels = labels
  )
```

#### Style: "custom" (egna etiketter)

```r
breaks <- c(0, 20, 40, 60, 80, 100)

# Egna beskrivande etiketter
custom <- c("Mycket låg", "Låg", "Medel", "Hög", "Mycket hög")

labels <- create_labels(breaks, style = "custom", custom_labels = custom)
# → "Mycket låg", "Låg", "Medel", "Hög", "Mycket hög"

# På karta
karta$klass <- apply_classification(karta$andel, breaks, labels)

ggplot(karta, aes(fill = klass)) +
  geom_sf() +
  scale_fill_gbg_sequential("blue", n = 5) +
  labs(fill = "Utbildningsnivå")
```

### Användningsområden per stil

| Stil | Användning | Exempel |
|------|-----------|---------|
| **range** | Standard för klassificering | "0-1000", "1000-2000" |
| **ruler** | Tydligt för "upp till X" | "< 1000", "< 2000" |
| **exact** | Continuous scales, exakta gränser | "1000", "2000", "3000" |
| **custom** | Beskrivande kategorier | "Låg", "Medel", "Hög" |

### Best Practices

#### 1. Välj stil efter datatyp

```r
# Pengar, invånare → range
labels <- create_labels(breaks, style = "range", unit = "kr")

# Procent, andelar → range eller ruler
labels <- create_labels(breaks, style = "ruler", unit = "%")

# Kategorier → custom
labels <- create_labels(breaks, style = "custom", custom_labels = c("Låg", "Medel", "Hög"))
```

#### 2. Matcha decimaler till data

```r
# Heltal (invånare, antal) → 0 decimaler
labels <- create_labels(breaks, decimals = 0, unit = "invånare")

# Procent → 1 decimal
labels <- create_labels(breaks, decimals = 1, unit = "%")

# Pengar → 0 decimaler för stora belopp
labels <- create_labels(breaks, decimals = 0, unit = "kr")
```

#### 3. Använd tydliga enheter

```r
# BRA
labels <- create_labels(breaks, unit = "kr")
labels <- create_labels(breaks, unit = "%")
labels <- create_labels(breaks, unit = "km²")

# UNDVIK vaga enheter
labels <- create_labels(breaks, unit = "enheter")  # Vad är en enhet?
```

---

## format_range() {#format_range}

### Beskrivning

Formaterar två värden som intervall (t.ex. "100-200").

### Syntax

```r
format_range(
  min_val,
  max_val,
  decimals = 0,
  unit = "",
  separator = "-",
  thousands_sep = " "
)
```

### Parametrar

- **min_val** (numeric): Minimum-värde
- **max_val** (numeric): Maximum-värde
- **decimals** (numeric): Antal decimaler (default: 0)
- **unit** (character): Enhet (default: "")
- **separator** (character): Separator (default: "-")
- **thousands_sep** (character): Tusentalsavgränsare (default: " ")

### Returnerar

Character string med formaterat intervall

### Exempel

```r
# Enkelt intervall
format_range(0, 1000)
# → "0-1000"

# Med enhet
format_range(0, 1000, unit = "kr")
# → "0-1000 kr"

# Med tusentalsavgränsare
format_range(10000, 20000, unit = "kr", thousands_sep = " ")
# → "10 000-20 000 kr"

# Med decimaler
format_range(0.5, 1.5, decimals = 1, unit = "%")
# → "0.5-1.5 %"

# Annan separator
format_range(0, 1000, separator = " till ", unit = "kr")
# → "0 till 1000 kr"

# Använd i loop för flera intervall
breaks <- c(0, 1000, 2000, 3000)
labels <- sapply(seq_along(breaks)[-length(breaks)], function(i) {
  format_range(breaks[i], breaks[i+1], unit = "kr")
})
# → "0-1000 kr", "1000-2000 kr", "2000-3000 kr"
```

---

## format_ruler() {#format_ruler}

### Beskrivning

Formaterar värde med "mindre än"-prefix (t.ex. "< 100").

### Syntax

```r
format_ruler(
  value,
  decimals = 0,
  unit = "",
  prefix = "<",
  thousands_sep = " "
)
```

### Parametrar

- **value** (numeric): Värde
- **decimals** (numeric): Antal decimaler (default: 0)
- **unit** (character): Enhet (default: "")
- **prefix** (character): Prefix (default: "<")
- **thousands_sep** (character): Tusentalsavgränsare (default: " ")

### Returnerar

Character string med formaterat värde

### Exempel

```r
# Enkelt
format_ruler(1000)
# → "< 1000"

# Med enhet
format_ruler(1000, unit = "kr")
# → "< 1000 kr"

# Med tusentalsavgränsare
format_ruler(10000, unit = "kr", thousands_sep = " ")
# → "< 10 000 kr"

# Annan prefix
format_ruler(100, prefix = "upp till ", unit = "%")
# → "upp till 100 %"

# För flera värden
breaks <- c(0, 1000, 2000, 3000)
labels <- sapply(breaks[-1], function(x) {
  format_ruler(x, unit = "kr")
})
# → "< 1000 kr", "< 2000 kr", "< 3000 kr"
```

---

## format_exact() {#format_exact}

### Beskrivning

Formaterar exakt värde (t.ex. "1000 kr").

### Syntax

```r
format_exact(
  value,
  decimals = 0,
  unit = "",
  thousands_sep = " "
)
```

### Parametrar

- **value** (numeric): Värde
- **decimals** (numeric): Antal decimaler (default: 0)
- **unit** (character): Enhet (default: "")
- **thousands_sep** (character): Tusentalsavgränsare (default: " ")

### Returnerar

Character string med formaterat värde

### Exempel

```r
# Enkelt
format_exact(1000)
# → "1000"

# Med enhet
format_exact(1000, unit = "kr")
# → "1000 kr"

# Med tusentalsavgränsare
format_exact(10000, unit = "kr", thousands_sep = " ")
# → "10 000 kr"

# Med decimaler
format_exact(15.5, decimals = 1, unit = "%")
# → "15.5 %"

# För alla break-points
breaks <- c(0, 1000, 2000, 3000)
labels <- sapply(breaks, function(x) {
  format_exact(x, unit = "kr")
})
# → "0 kr", "1000 kr", "2000 kr", "3000 kr"
```

---

## add_units() {#add_units}

### Beskrivning

Lägger till enhet till befintliga etiketter.

### Syntax

```r
add_units(labels, unit = "", separator = " ")
```

### Parametrar

- **labels** (character): Befintliga etiketter
- **unit** (character): Enhet att lägga till
- **separator** (character): Separator mellan etikett och enhet (default: " ")

### Returnerar

Character vector med etiketter med enheter

### Exempel

```r
# Lägg till enhet
labels <- c("0-1000", "1000-2000", "2000-3000")
add_units(labels, unit = "kr")
# → "0-1000 kr", "1000-2000 kr", "2000-3000 kr"

# Lägg till procent
labels <- c("0-20", "20-40", "40-60")
add_units(labels, unit = "%")
# → "0-20 %", "20-40 %", "40-60 %"

# Annan separator
labels <- c("Låg", "Medel", "Hög")
add_units(labels, unit = "inkomst", separator = " - ")
# → "Låg - inkomst", "Medel - inkomst", "Hög - inkomst"

# Användning i pipeline
labels <- create_labels(breaks, style = "range") |>
  add_units(unit = "kr")
```

---

## Komplett exempel: Från breaks till karta

```r
# 1. Skapa klassindelning
breaks <- create_breaks(karta$inkomst, "fisher", n_classes = 5)

# 2. Skapa etiketter
labels <- create_labels(
  breaks,
  style = "range",
  unit = "kr",
  decimals = 0,
  separator = "-"
)

# 3. Applicera klassificering
karta$klass <- apply_classification(karta$inkomst, breaks, labels)

# 4. Visualisera
ggplot(karta, aes(fill = klass)) +
  geom_sf(color = "white", linewidth = 0.1) +
  scale_fill_gbg_sequential("blue", n = 5) +
  labs(
    title = "Medianinkomst per DeSO-område",
    fill = "Inkomst"
  ) +
  theme_gothenburg_map()
```

---

## Snabbreferens

### Vanliga etikett-format

```r
# Intervall (mest vanligt)
create_labels(breaks, style = "range", unit = "kr")
# → "0-1000 kr", "1000-2000 kr"

# Linjal ("upp till")
create_labels(breaks, style = "ruler", unit = "%")
# → "< 20 %", "< 40 %"

# Exakta värden
create_labels(breaks, style = "exact", unit = "km²")
# → "0 km²", "100 km²"

# Egna etiketter
create_labels(breaks, style = "custom", custom_labels = c("Låg", "Medel", "Hög"))
# → "Låg", "Medel", "Hög"
```

### Formatering per datatyp

```r
# Pengar (heltal)
create_labels(breaks, style = "range", unit = "kr", decimals = 0)

# Procent (1 decimal)
create_labels(breaks, style = "range", unit = "%", decimals = 1)

# Invånare (heltal, tusentalsavgränsare hanteras automatiskt)
create_labels(breaks, style = "range", unit = "invånare", decimals = 0)

# Yta (2 decimaler)
create_labels(breaks, style = "range", unit = "km²", decimals = 2)
```

---

**Version:** 1.0  
**Uppdaterad:** 2025-12-16  
**För:** labels.R
