# 📊 Klassindelningsfunktioner

Funktioner för att klassificera kontinuerliga värden till diskreta klasser för kartvisualisering.

## create_breaks()

Skapar brytpunkter för klassindelning.

### Syntax
```r
create_breaks(values, method = "fisher", n = 5)
```

### Metoder
- **fisher** - Naturliga brytpunkter (standard)
- **quantile** - Lika många per klass
- **equal** - Lika stora intervall  
- **dpih** - Automatiskt antal (VARNING: kan ge 100+ klasser!)

### Exempel
```r
breaks <- create_breaks(karta$befolkning, method = "fisher", n = 5)
```

## apply_classification()

Applicerar klassindelning på värden.

### Syntax
```r
apply_classification(values, breaks, labels)
```

### Exempel
```r
karta$klass <- apply_classification(karta$befolkning, breaks, labels)
```

## Komplett workflow

```r
# 1. Skapa breaks
breaks <- create_breaks(karta$varde, method = "fisher", n = 5)

# 2. Skapa labels
labels <- create_labels(breaks, style = "range", decimals = 0)

# 3. Klassificera
karta$klass <- apply_classification(karta$varde, breaks, labels)

# 4. Visualisera
ggplot(karta, aes(fill = klass)) +
  geom_sf() +
  scale_fill_gbg_sequential("blue", n = 5)
```

## Metodval

- **Fisher**: Standard, naturliga grupper
- **Quantile**: Skev fördelning
- **Equal**: Jämn fördelning, intuitivt
- **DPIH**: Explorativ analys (kräver validering!)

**Skapad:** 2025-12-18
