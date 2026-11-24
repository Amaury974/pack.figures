
.onLoad <- function(libname, pkgname) {

  Sys.unsetenv(c("PROJ_LIB", "PROJ_DATA"))
  message("les directions 'PROJ_LIB' et 'PROJ_DATA' ont été réinitialisées")

  if (getOption("ggplot.change.default.options", TRUE)) {
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      ggplot2::theme_set(theme_basique())
      options(ggplot2.continuous.colour="viridis", ggplot2.continuous.fill = "viridis")
      message('le paramètres ggplot ont été modifié pour theme_ajorant() et couleurs viridis')

    }
  }

  if (getOption("dplyr.change.default.options", TRUE)) {
    if (requireNamespace("dplyr", quietly = TRUE)) {
      options(dplyr.summarise.inform = FALSE)
    }
  }

}
