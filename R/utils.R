
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




