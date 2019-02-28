#' Gradient colour scales for top-left triangle in geom_asymmat
#'
#' @description This dictates a gradient color scheme for the top-left
#'     (\code{tl}) of a \code{geom_asymmat} ggplot geom.
#'     \code{scale_*_tl_gradient} creates a two colour gradient (low-high),
#'     \code{scale_*_tl_gradient2} creates a diverging colour gradient
#'     (low-mid-high), \code{scale_*_tl_gradientn} creates a n-colour
#'     gradient.
#' @param ... arguments passed on to \code{continuous_scale_asym}
#' @param low,high the colors to represent low and high values
#' @param space colour space in which to calculate gradient. Must be "Lab" -
#'     other values are deprecated.
#' @param na.value color of missing (\code{NA}) values
#' @param guide Type of legend. Use "colourbar" for continuous colour bar, or
#'     "legend" for discrete colour legend.
#' @param aesthetics Character string or vector of character strings listing
#'     the name(s) of the aesthetic(s) that this scale works with. For now,
#'     leave the default alone, though I plan to reinstate the standard ggplot
#'     system here, eventually.
#'
#' @examples
#' library(tibble)
#' library(ggplot2)
#' tib <- tibble(g1 = c("A", "A", "B"),
#'               g2 = c("B", "C", "C"),
#'               val_1 = c(1, 2, 3),
#'               val_2 = c(-1, 0, 1))
#'
#' tib
#'
#' ggplot(tib) +
#' geom_asymmat(aes(x = g1, y = g2, fill_tl = val_1, fill_br = val_2)) +
#'     scale_fill_tl_gradient(low = "lightpink", high = "tomato") +
#'     labs(fill_tl = "top-left fill")
#'
#' @export scale_fill_tl_gradient
scale_fill_tl_gradient <- function(..., low = "#132B43", high = "#56B1F7",
                                   space = "Lab",
                                   na.value = "grey50",
                                   guide = "colourbar",
                                   aesthetics = "fill_tl") {
    continuous_scale_asym(aesthetics = aesthetics,
                          scale_name = "gradient",
                          palette = scales::seq_gradient_pal(low, high, space),
                          na.value = na.value,
                          guide = guide, ...)
}


#' Gradient colour scales for bottom-right triangle in geom_asymmat
#'
#' @description This dictates a gradient color scheme for the bottom-right
#'     (\code{br}) of a \code{geom_asymmat} ggplot geom.
#'     \code{scale_*_br_gradient} creates a two colour gradient (low-high),
#'     \code{scale_*_br_gradient2} creates a diverging colour gradient
#'     (low-mid-high), \code{scale_*_br_gradientn} creates a n-colour
#'     gradient.
#'
#' @inheritParams scale_fill_tl_gradient
#'
#' @examples
#' library(tibble)
#' library(ggplot2)
#' tib <- tibble(g1 = c("A", "A", "B"),
#'               g2 = c("B", "C", "C"),
#'               val_1 = c(1, 2, 3),
#'               val_2 = c(-1, 0, 1))
#'
#' tib
#'
#' ggplot(tib) +
#' geom_asymmat(aes(x = g1, y = g2, fill_tl = val_1, fill_br = val_2)) +
#'     scale_fill_br_gradient(low = "lightblue1", high = "dodgerblue") +
#'     labs(fill_br = "bottom-right fill")
#'
#' @export scale_fill_br_gradient
scale_fill_br_gradient <- function(..., low = "#132B43", high = "#56B1F7",
                                   space = "Lab",
                                   na.value = "grey50",
                                   guide = "colourbar",
                                   aesthetics = "fill_br") {
    continuous_scale_asym(aesthetics = aesthetics,
                          scale_name = "gradient",
                          palette = scales::seq_gradient_pal(low, high, space),
                          na.value = na.value,
                          guide = guide, ...)
}
