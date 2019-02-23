#' Asymmetrically filled symmetric matrix (using ggplot)
#'
#' Generate an asymmetric matrix with different fill values for top-left
#'     and bottom-right triangles as a \code{ggplot} object
#'
#' @param .data data table
#' @param x,y x and y-axes column names
#' @param fill_tl,fill_br column names for top-left and bottom-right fill
#' @param color_tl,color_br colors for the lines; use \code{NA} for no color
#' @param lab_tl,lab_br labels for the scales; use \code{NULL} (default) to use
#'     the column name (ie. \code{fill_tl} or \code{fill_br}) or empty strings
#'     (\code{""}) for no label
#'
#' @return a ggplot object of an asymmetrically-colored \code{x} x \code{y}
#'     matrix with \code{fill_tl} data coloring the top-left triangle and
#'     \code{fill_br} coloring the bottom-left triangle.
#'
#' @importFrom rlang !! := enquo
#' @import ggplot2
#' @export geom_asymmat
geom_asymmat <- function(.data, x, y, fill_tl, fill_br,
                         color_tl = NA, color_br = NA,
                         lab_tl = NULL, lab_br = NULL) {
    x <- enquo(x)
    y <- enquo(y)
    fill_tl <- enquo(fill_tl)
    fill_br <- enquo(fill_br)
    tib <- tibble::as_tibble(.data)
    tib <- prepare_asymmetric_tibble(tib, !!x, !!y, !!fill_tl, !!fill_br)
    ggplot(tib, aes(x = !!x, y = !!y)) +
        geom_tile(aes(fill = !!fill_tl), color = color_tl) +
        scale_fill_gradient2(low = "dodgerblue", high = "tomato",
                             na.value = NA) +
        labs(fill = ifelse(is.null(lab_tl), rlang::quo_text(fill_tl), lab_tl)) +
        ggnewscale::new_scale_fill() +
        geom_tile(aes(fill = !!fill_br), color = color_br) +
        scale_fill_gradient(low = "#F7FCF5", high = "#238B45",
                            na.value = NA) +
        labs(fill = ifelse(is.null(lab_br), rlang::quo_text(fill_br), lab_br))
}
