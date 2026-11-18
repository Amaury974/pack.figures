
.onLoad <- function(libname, pkgname) {
  if (getOption("ggplot.change.default.options", TRUE)) {
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      ggplot2::theme_set(ggplot2::theme_minimal())
      ggplot2::update_theme(
        legend.box.spacing = unit(0, "pt"),
        legend.position = 'bottom',
        legend.box = "horizontal",
        plot.title = element_text(hjust = 0, vjust = 1),
        plot.caption = element_text(size = 7, color = '#4D4D4DFF'),
        plot.caption.position = 'plot',
        plot.subtitle = element_text(color = '#4D4D4DFF'),
        legend.key.spacing.y = unit(0, "pt"),
      )
      options(ggplot2.continuous.colour="viridis", ggplot2.continuous.fill = "viridis")
    }
  }

  if (getOption("dplyr.change.default.options", TRUE)) {
    if (requireNamespace("dplyr", quietly = TRUE)) {
      options(dplyr.summarise.inform = FALSE)
    }
  }


}
