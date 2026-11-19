# pack.figures

Package R regroupant des fonctions pour les **graphiques** (ggplot2) et la **cartographie** (Leaflet) : graphiques préconfigurés, mise en forme et export de figures, palettes de couleurs, etc.

![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)

## Installation

```r
# install.packages("remotes")
remotes::install_github("Amaury974/pack.figures")
```

Le package dépend de [`pack.divers`](https://github.com/Amaury974/pack.divers) (déclaré dans `Remotes`), installé automatiquement.

### Packages optionnels

Certaines fonctions nécessitent des packages supplémentaires, vérifiés à l'appel :

| Package | Utilisé par |
|---|---|
| `cowplot` | `fig_grid()` |
| `agricolae` | `boxplot2()` |
| `colorspace` | `mega_Palette()` |
| `sf`, `sp`, `maps` | `quel_dep()` |
| `leaflet` | `leaf_colored_markers()` |
| `randomForest` | `rf_fig()` |
| `magick` | `add_background_image()`, `save_plot_bg()` |

```r
install.packages(c("cowplot", "agricolae", "colorspace", "sf", "sp", "maps",
                   "leaflet", "randomForest", "magick"))
```

## ⚠️ Options ggplot2 modifiées au chargement

Au chargement (`library(pack.figures)`), le package modifie le thème global de ggplot2 :

- thème `theme_minimal()` par défaut ;
- légende en bas, disposée horizontalement ;
- titre aligné à gauche, sous-titre et légende (`caption`) en gris ;
- palette continue `viridis` pour `colour` et `fill`.

Il règle aussi `dplyr.summarise.inform = FALSE` (messages de `summarise()` masqués).

Pour désactiver ces réglages, **avant** de charger le package :

```r
options(ggplot.change.default.options = FALSE,
        dplyr.change.default.options  = FALSE)
library(pack.figures)
```

## Fonctions

### Graphiques préconfigurés

| Fonction | Description |
|---|---|
| `graph_perf()` | Graphique observé / simulé avec indicateurs de performance (RMSE, RRMSE, biais, efficience, ou MAPE si effet développement) |
| `graph_perf_ROC()` | Graphique observé / simulé en quadrants selon des seuils, avec sensibilité, spécificité, taux de bien prédits et indice de Youden |
| `boxplot2()` | Boxplot par catégorie avec test de Tukey (groupes de significativité), classé par médiane |
| `rf_fig()` | Figure d'analyse d'une forêt aléatoire : %IncMSE et IncNodePurity |

### Mise en page et export

| Fonction | Description |
|---|---|
| `fig_grid()` | Assemble plusieurs graphiques en grille (titre, noms de lignes/colonnes, légende commune) et l'enregistre |
| `sauv_graph()` | Enregistre une figure en png et/ou svg |
| `add_background_image()` | Ajoute une image en arrière-plan (non déformée) sous le quadrillage d'un ggplot |
| `save_plot_bg()` | Enregistre un ggplot avec image de fond (png) |
| `dim_aire()` | Dimensions d'export optimales pour un ratio et une aire donnés |
| `quelles_marges()` | Marges fixes (X, Y) d'un ggplot |

### Couleurs

| Fonction | Description |
|---|---|
| `mega_Palette()` | Palette jusqu'à ~50 couleurs à peu près discernables (non adaptée aux daltoniens) |

### Cartographie

| Fonction | Description |
|---|---|
| `quel_dep()` | Identifie le département à partir de coordonnées (colonnes longitude/latitude) ; renvoie les données enrichies et les centroïdes des départements |
| `leaf_colored_markers()` | Crée une liste de marqueurs colorés pour Leaflet (`leaflet::iconList()`) |

La documentation détaillée de chaque fonction est accessible via `?nom_de_la_fonction`.

## Exemples

### Performance observé / simulé

```r
library(pack.figures)

df <- data.frame(mesure = rnorm(100, 10, 2))
df$modele <- df$mesure + rnorm(100, 0, 0.8)

graph_perf(df, obs = "mesure", sim = "modele", .variable = "rendement")
```

### Boxplot avec test de Tukey

```r
boxplot2(iris, x = "Species", y = "Sepal.Length")
```

### Palette étendue

```r
N_col <- 20
barplot(rep(1, N_col), col = mega_Palette(N_col))
```

### Marqueurs Leaflet colorés

```r
list_col <- list(rouge  = "#FF0000",
                 vert   = "#C5FF00",
                 bleu   = "#00FFFF",
                 violet = "#9100FF")

my_markers <- leaf_colored_markers(list_col)

leaflet::leaflet() |>
  leaflet::addTiles() |>
  leaflet::addMarkers(lat = c(0, 0, 1, 1),
                      lng = c(0, 1, 0, 1),
                      icon = my_markers[names(list_col)])
```

### Image de fond et export

```r
p <- ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
  geom_point() +
  labs(caption = "Source : mtcars")

save_plot_bg(p, "plot.png", image_path = "logo.png", width = 6, height = 4)

# Dimensions et marges
quelles_marges(p)
dim_aire(ratio = 4/3, aire = 30)
```

### Export png / svg

```r
sauv_graph(p, "ma_figure", width = 6, height = 4)  # crée .png et .svg
```

## Licence

Distribué sous licence [GNU GPL (≥ 3)](LICENSE.md).

## Auteur

Amaury Jorant ([@Amaury974](https://github.com/Amaury974)) — [issues](https://github.com/Amaury974/pack.figures/issues)
