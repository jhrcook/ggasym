#' Generate an asymmetric matrix with different fill values for top-left
#'     and bottom-right triangles
#'
#' TODO: add description
#'
#' @param .data data table
#' @param x,y x and y-axes column names
#' @param fill_tl,fill_br column names for topleft and bottom-right fill
#'
#' @return a ggplot object of an asymmetrically-colored \code{x} x \code{y}
#'     matix with \code{fill_tl} data coloring the top-left triangle and
#'     \code{fill_br} coloring the bottom-left triangle.
#'
#' @export geom_asymat
geom_asymat <- function(.data, x, y, fill_tl, fill_br) {

    tib <- tibble::as_tibble(.data)
    tib <- prepare_asymmetric_tibble(tib, !!x, !!y)
}
