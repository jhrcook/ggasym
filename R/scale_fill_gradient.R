#' Gradient colour scales for top-left or bottom-right triangle in geom_asymmat
#'
#' @description This dictates a gradient colour scheme for the top-left
#'     (\code{tl}) or  bottom_right (\code{br}) of a \code{geom_asymmat} ggplot geom.
#'     \code{scale_*_tl/br_gradient} creates a two colour gradient (low-high),
#'     \code{scale_*_tl/br_gradient2} creates a diverging colour gradient
#'     (low-mid-high), \code{scale_*_tl/br_gradientn} creates a n-colour
#'     gradient.
#' @param ... arguments passed on to \code{continuous_scale_asym}
#' @param low,high the colors to represent low and high values
#' @param mid color for mid point (see \code{?scales::div_gradient_pal} for
#'     more documentation of how colors are calculated)
#' @param midpoint The midpoint (in data value) of the diverging scale.
#'     Defaults to 0.
#' @param colours,colors Vector of colours to use for n-colour gradient.
#' @param values if colours should not be evenly positioned along the gradient
#'     this vector gives the position (between 0 and 1) for each colour in the
#'     colours vector. See \code{rescale()} for a convenience function to map an
#'     arbitrary range to between 0 and 1.
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
#' set.seed(0)
#' tib <- tibble(g1 = c("A", "A", "A", "B", "B", "C"),
#'               g2 = c("B", "C", "D", "C", "D", "D"),
#'               val_1 = c(1:6),
#'               val_2 = sample(-10:10, 6))
#' tib <- asymmetrise(tib, g1, g2)
#' g <- ggplot(tib) +
#'     geom_asymmat(aes(x = g1, y = g2, fill_tl = val_1, fill_br = val_2))
#'
#' g + scale_fill_tl_gradient(low = "lightpink", high = "tomato") +
#'     scale_fill_br_gradient(low = "lightblue", high = "dodgerblue")
#'
#' g + scale_fill_tl_gradient2(low = "dodgerblue",
#'                             mid = "white",
#'                             high = "tomato") +
#'     scale_fill_br_gradient2(low = "seagreen4",
#'                             mid = "white",
#'                             high = "orange")
#'
#' g + scale_fill_tl_gradientn(colours = terrain.colors(10)) +
#'     scale_fill_br_gradientn(colours = heat.colors(10))
#'
#' @name scale_gradient
NULL


#' @rdname scale_gradient
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


#' @rdname scale_gradient
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


#' @rdname scale_gradient
#' @export scale_fill_tl_gradient2
scale_fill_tl_gradient2 <- function(...,
                                    low = scales::muted("red"),
                                    mid = "white",
                                    high = scales::muted("blue"),
                                    midpoint = 0,
                                    space = "Lab",
                                    na.value = "grey50",
                                    guide = "colourbar",
                                    aesthetics = "fill_tl") {
    continuous_scale_asym(aesthetics = aesthetics,
                          scale_name = "gradient2",
                          palette = scales::div_gradient_pal(low, mid, high, space),
                          na.value = na.value,
                          guide = guide, ...,
                          rescaler = mid_rescaler(mid = midpoint))
}


#' @rdname scale_gradient
#' @export scale_fill_br_gradient2
scale_fill_br_gradient2 <- function(...,
                                    low = scales::muted("red"),
                                    mid = "white",
                                    high = scales::muted("blue"),
                                    midpoint = 0,
                                    space = "Lab",
                                    na.value = "grey50",
                                    guide = "colourbar",
                                    aesthetics = "fill_br") {
    continuous_scale_asym(aesthetics = aesthetics,
                          scale_name = "gradient2",
                          palette = scales::div_gradient_pal(low, mid, high, space),
                          na.value = na.value,
                          guide = guide, ...,
                          rescaler = mid_rescaler(mid = midpoint))
}

# not exported from ggplot2
mid_rescaler <- function(mid) {
    function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
        scales::rescale_mid(x, to, from, mid)
    }
}


#' @rdname scale_gradient
#' @export scale_fill_tl_gradientn
scale_fill_tl_gradientn <- function(..., colours,
                                    values = NULL,
                                    space = "Lab",
                                    na.value = "grey50",
                                    guide = "colourbar",
                                    aesthetics = "fill_tl",
                                    colors) {
    colours <- if (missing(colours)) colors else colours
    continuous_scale_asym(aesthetics, "gradientn",
                          scales::gradient_n_pal(colours, values, space),
                          na.value = na.value,
                          guide = guide,
                          ...)
}


#' @rdname scale_gradient
#' @export scale_fill_br_gradientn
scale_fill_br_gradientn <- function(..., colours,
                                    values = NULL,
                                    space = "Lab",
                                    na.value = "grey50",
                                    guide = "colourbar",
                                    aesthetics = "fill_br",
                                    colors) {
    colours <- if (missing(colours)) colors else colours
    continuous_scale_asym(aesthetics, "gradientn",
                          scales::gradient_n_pal(colours, values, space),
                          na.value = na.value,
                          guide = guide,
                          ...)
}
