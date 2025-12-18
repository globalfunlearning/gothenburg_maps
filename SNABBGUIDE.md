# Snabbstart - Gothenburg Maps

I det här dokumentet finns ett antal konkreta scenarier för att komma igång snabbt.

## Förberedelse

```r
# Ladda funktioner och färger
source("R/00_setup.R")
source("path/to/gothenburg_colors/colors.R")

# Ladda exempeldata
befolkning <- readRDS("examples/data/gbg_primaromrade.rds")
```

---

## 1. Enkel tematisk karta

Standard choropleth-karta med 5 klasser.

```r
# Ladda och koppla data
primaromraden <- load_prepared_map("goteborg/primaromraden")
karta <- join_stat_to_map(primaromraden, befolkning, by = "omrade_kod")

# Klassindela
breaks <- create_breaks(karta$oktober, "fisher", 5)
labels <- create_labels(breaks, "range", decimals = 0)
karta$klass <- apply_classification(karta$oktober, breaks, labels)

# Ta fram en karta
ggplot(karta, aes(fill = klass)) +
  geom_sf(color = "white", linewidth = 0.1) +
  scale_fill_gbg_sequential("blue", n = 5) +
  labs(title = "Befolkning i oktober per primärområde", fill = "Invånare") +
  theme_gothenburg_map()

# Spara
ggsave("output/maps/befolkning.png", width = 12, height = 8, dpi = 300)
```

---

## 2. Divergerande karta (förändring)

Visar positiv/negativ förändring.

```r
# Ladda och koppla
primaromraden <- load_prepared_map("goteborg/primaromraden")
karta <- join_stat_to_map(primaromraden, befolkning, by = "omrade_kod")

# Klassindela med quantile (bra för skev data)
breaks <- create_breaks(karta$forandring, "quantile", 5)
labels <- create_labels(breaks, "range", decimals = 0)
karta$klass <- apply_classification(karta$forandring, breaks, labels)

# Röd-grön färgpalett
ggplot(karta, aes(fill = klass)) +
  geom_sf(color = "black", linewidth = 0.1) +
  scale_fill_gbg_diverging("red_green", n = 5) +
  labs(
    title = "Befolkningsförändring 2024",
    fill = "Förändring",
    caption = "Källa: SCB"
  ) +
  theme_gothenburg_map()
```

---

## 3. Karta med extra lager (vatten + gränser)

Lägg till vattenlager och stadsområdesgränser.

```r
# Ladda alla lager
primaromraden <- load_prepared_map("goteborg/primaromraden")
stadsomraden <- load_prepared_map("goteborg/stadsomraden")
alv <- load_water_layer("goteborg/alv_goteborg")

# Koppla data
karta <- join_stat_to_map(primaromraden, befolkning, by = "omrade_kod")

# Klassindela
breaks <- create_breaks(karta$forandring_procent, "fisher", 6)
labels <- create_labels(breaks, "range", unit = "%", decimals = 1)
karta$klass <- apply_classification(karta$forandring_procent, breaks, labels)

# Skapa karta med flera lager
ggplot() +
  geom_sf(data = karta, aes(fill = klass), color = "white", linewidth = 0.1) +
  geom_sf(data = alv, fill = "#b3d9ff", color = NA) +
  geom_sf(data = stadsomraden, fill = NA, color = "black", linewidth = 0.3) +
  scale_fill_gbg_sequential("blue", n = 6) +
  labs(title = "Procentuell befolkningsförändring", fill = "Förändring (%)") +
  theme_gothenburg_map()
```

---

## 4. Snabbkarta med create_static_map()

Allt-i-ett funktion för snabbanalys.

```r
karta <- create_static_map(
  stat_data = befolkning,
  geo_layer = "goteborg/primaromraden",
  value_col = "forandring",
  by = "omrade_kod",
  classify_method = "fisher",
  n_classes = 6,
  palette_type = "diverging",
  palette_name = "red_green",
  title = "Befolkningsförändring",
  legend_title = "Förändring",
  caption = "Källa: SCB"
)

print(karta)
```

---

## 5. Interaktiv webbkarta

Karta med tooltips för webb eller Quarto-rapport.

```r
karta_webb <- create_interactive_map(
  stat_data = befolkning,
  geo_layer = "goteborg/primaromraden",
  value_col = "oktober",
  by = "omrade_kod",
  classify_method = "fisher",
  n_classes = 5,
  palette_type = "sequential",
  palette_name = "blue",
  label_decimals = 0,
  title = "Befolkning per område",
  tooltip_cols = c("omrade_namn", "oktober", "forandring"),
  tooltip_alias = c("Område", "Befolkning", "Befolkningsförändring")
)

# Visa i RStudio
print(karta_webb)

# Spara som HTML
htmlwidgets::saveWidget(karta_webb, "output/maps/befolkning.html")
```

---

## 6. Anpassa tema och färger

Kontrollera legend och färger.

```r
# Ladda och förbered
primaromraden <- load_prepared_map("goteborg/primaromraden")
karta <- join_stat_to_map(primaromraden, befolkning, by = "omrade_kod")

# Klassindela
breaks <- create_breaks(karta$oktober, "pretty", 4)
labels <- create_labels(breaks, "range", decimals = 0)
karta$klass <- apply_classification(karta$oktober, breaks, labels)

# Anpassat tema
ggplot(karta, aes(fill = klass)) +
  geom_sf(color = "black", linewidth = 0.1) +
  scale_fill_gbg_sequential("green", n = 4, direction = 1) +
  labs(title = "", fill = "Invånare") +
  theme_gothenburg_map(
    legend_direction = "vertical",
    legend_position = c(0.12, 0.95),  # Uppe till vänster
    panel_background = "#f5f5f5"
  )
```

---

## 7. Sverigekarta - alla kommuner

Nationell nivå istället för Göteborg.

```r
# Ladda Sveriges kommuner
kommuner <- load_prepared_map("sverige/kommuner")

# Exempeldata (ersätt med din data)
kommun_data <- readRDS("examples/data/kommuner_lan.rds")

# Koppla data
karta <- join_stat_to_map(kommuner, kommun_data, by = c("kommun_kod" = "kommun_namn")

# Klassindela med fisher (dpih ger ofta för många klasser för kartor)
breaks <- create_breaks(karta$forandring, "fisher", 6)
labels <- create_labels(breaks, "range", decimals = 0)
karta$klass <- apply_classification(karta$forandring, breaks, labels)

# Skapa karta
ggplot(karta, aes(fill = klass)) +
  geom_sf(color = "white", linewidth = 0.05) +
  scale_fill_gbg_diverging("red_green", n = 6) +
  labs(
    title = "Befolkningsförändring i Sveriges kommuner",
    fill = "Förändring",
    caption = "Källa: SCB"
  ) +
  theme_gothenburg_map()
```

---

## 8. DeSO-karta med SCB-data

Karta över demografiska statistikområden.
```r
# Ladda förberedda DeSO-områden (anpassade för havet)
deso <- load_prepared_map("goteborg/deso_goteborg")

# Eller hämta direkt från SCB (originalgränser med vatten)
deso <- load_deso_from_scb(municipality_codes = "1480", year = 2025)

# Koppla din statistik (exempel)
stat_data <- readRDS("min_deso_statistik.rds")
karta <- join_stat_to_map(deso, stat_data, by = "deso")

# Klassindela och visualisera
breaks <- create_breaks(karta$min_variabel, "fisher", 5)
labels <- create_labels(breaks, "range", decimals = 1)
karta$klass <- apply_classification(karta$min_variabel, breaks, labels)

ggplot(karta, aes(fill = klass)) +
  geom_sf(color = "white", linewidth = 0.05) +
  scale_fill_gbg_sequential("blue", n = 5) +
  labs(title = "Din statistik per DeSO") +
  theme_gothenburg_map()
```

**Tips:**
- Förberedda lager har anpassade havsgränser för snyggare kartor
- Originalgränser från SCB innehåller vatten - använd om du behöver exakta gränser


---

## 9. Testa olika klassindelningar

Jämför metoder för att hitta bästa klassindelningen.

```r
# Ladda data
primaromraden <- load_prepared_map("goteborg/primaromraden")
karta <- join_stat_to_map(primaromraden, befolkning, by = "omrade_kod")

# 1. Analysera värdena
summarize_for_breaks(karta$forandring, n_classes = 5)

# 2. Jämför metoder visuellt
p <- compare_methods(
  karta$forandring,
  methods = c("fisher", "quantile", "equal"),
  n_classes = 5
)
print(p)
ggsave("output/maps/jamfor_metoder.png", p, width = 10, height = 12)

# 3. Välj metod och skapa karta
breaks <- create_breaks(karta$forandring, "fisher", 5)
labels <- create_labels(breaks, "range", decimals = 0)
karta$klass <- apply_classification(karta$forandring, breaks, labels)

ggplot(karta, aes(fill = klass)) +
  geom_sf(color = "black", linewidth = 0.1) +
  scale_fill_gbg_diverging("red_green", n = 5) +
  labs(title = "Befolkningsförändring") +
  theme_gothenburg_map()
```

---

## Tips

**Klassindelning:**

- Börja med `fisher` (bästa natural breaks)

- Använd `quantile` för skev data

- Testa `dpih` eller `headtails` om osäker på antal klasser **men**:

  - dpih kan ge MÅNGA klasser (100+) för stora dataset
  
  - Bättre för histogram än kartor
  
  - För kartor: använd fisher/quantile med 5-7 klasser
  
- Använd `compare_methods()` för att jämföra

**Färgpaletter:**

- Sequential: Värden från låg till hög

- Diverging: Avvikelser från neutral punkt (0, medel)

- Categorical: Kategorier utan inbördes ordning

**Antal klasser:**

- 5-6 är standard för choropleth

- Max 7 för categorical

- dpih/headtails/box väljer själva (kan bli många!)

- För kartor: håll dig till 5-7 klasser för läsbarhet

**Extra lager:**

- Rita vatten FÖRE kartdata (så det hamnar under)

- Rita gränser EFTER kartdata (så de syns ovanpå)

- Använd `fill = NA` för transparenta gränser

---

**Version:** 1.0  
**Uppdaterad:** 2025-12-11