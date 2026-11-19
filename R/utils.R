
# --- Element de theme personnalise : dessine un grob quelconque comme
#     panel.background (donc AVANT panel.grid) --------------------------------


#' @noRd
#' @export
#' @method element_grob element_custom_bg
element_grob.element_custom_bg <- function(element, ...) element$grob


#' @noRd
element_custom_bg <- function(grob) {
  structure(
    list(grob = grob),
    # doit heriter de "element_rect" pour passer la validation de theme()
    # de ggplot2 (panel.background attend un element_rect)
    class = c("element_custom_bg", "element_rect", "element")
  )
}




#' @noRd
fct_breakpoints <- function(x1, y1,
                            x2, y2,
                            range_X, range_Y){

  # on considère X1, Y1 l'origine de notre référentiel
  x <- x2-x1
  y <- y2-y1

  # l'espacement est considéré relativement à la longueur de l'axe uniquement
  x <- x/range_X
  y <- y/range_Y


  # la longueur d'un tiret = 2 x la longueur entre les tiret = 3% de la place dispo
  l <- 0.03
  l2 <- 2*l

  # longueur du segment
  L <- sqrt(x^2+y^2)


  # point au milieu du segment
  x_centre <- 0.5*x
  y_centre <- 0.5*y

  # intersection gauche (thales)
  ratio_longeurs <- l/(0.5*L)
  x_G <- x_centre + (x-x_centre) * ratio_longeurs
  y_G <- y_centre + (y-y_centre) * ratio_longeurs

  # intersection droite
  x_D <- x_centre - (x-x_centre) * ratio_longeurs
  y_D <- y_centre - (y-y_centre) * ratio_longeurs

  # angle de la courbe (en radians)
  angle <- atan(y/x)

  # décalage des tirets. En considérant qu'ils doivent former un angle droit avec la courbe
  x_ajout <- l2 * sin(angle)
  y_ajout <- l2 * cos(angle)


  # coordonnées des sommets des tirets
  x_Gh <- x_G + x_ajout
  y_Gh <- y_G - y_ajout

  x_Gb <- x_G - x_ajout
  y_Gb <- y_G + y_ajout

  x_Dh <- x_D + x_ajout
  y_Dh <- y_D - y_ajout

  x_Db <- x_D - x_ajout
  y_Db <- y_D + y_ajout

  # on retourne au référentiel d'origine
  x_Gh <- x1 + x_Gh * range_X
  y_Gh <- y1 + y_Gh * range_Y

  x_Gb <- x1 + x_Gb * range_X
  y_Gb <- y1 + y_Gb * range_Y

  x_Dh <- x1 + x_Dh * range_X
  y_Dh <- y1 + y_Dh * range_Y

  x_Db <- x1 + x_Db * range_X
  y_Db <- y1 + y_Db * range_Y


  data.frame(x1=c(x_Gh,x_Dh),
             x2=c(x_Gb,x_Db),
             y1=c(y_Gh,y_Dh),
             y2=c(y_Gb,y_Db))

}

