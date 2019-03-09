#' Gradient colour scales geom_asymmat
#'
#' @description This dictates a gradient colour scheme for the top-left
#'     (\code{tl}),  bottom_right (\code{br}), or diagonal (\code{diag})
#'     of a \code{geom_asymmat} ggplot geom. \code{scale_*_tl/br_gradient}
#'     creates a two colour gradient (low-high), \code{scale_*_tl/br_gradient2}
#'     creates a diverging colour gradient (low-mid-high),
#'     \code{scale_*_tl/br_gradientn} creates a n-colour
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
#'
#' tib <- tibble(g1 = c("A", "A", "A", "B", "B", "C", "A", "B", "C", "D"),
#'               g2 = c("B", "C", "D", "C", "D", "D", "A", "B", "C", "D"),
#'               val_1 = c(1:10),
#'               val_2 = sample(-10:10, 10),
#'               val_3 = c(rep(NA, 6), 1, 2, 3, 4))
#' tib <- asymmetrise(tib, g1, g2)
#' g <- ggplot(tib, aes(x = g1, y = g2)) +
#'     geom_asymmat(aes(fill_tl = val_1, fill_br = val_2, fill_diag = val_3))
#'
#' g + scale_fill_tl_gradient(low = "lightpink", high = "tomato") +
#'     scale_fill_br_gradient(low = "lightblue", high = "dodgerblue") +
#'     scale_fill_diag_gradient(low = "yellow", high = "orange3")
#'
#' g + scale_fill_tl_gradient2(low = "dodgerblue",
#'                             mid = "white", midpoint = 5,
#'                             high = "tomato") +
#'     scale_fill_br_gradient2(low = "seagreen4",
#'                             mid = "white", midpoint = 0,
#'                             high = "orange") +
#'     scale_fill_diag_gradient2(low = "magenta",
#'                               mid = "cornflowerblue", midpoint = 2.5,
#'                               high = "chartreuse")
#'
#' g + scale_fill_tl_gradientn(colours = terrain.colors(200)) +
#'     scale_fill_br_gradientn(colours = heat.colors(200)) +
#'     scale_fill_diag_gradientn(colours = rainbow(200))
#'
#' g + scale_fill_tl_distiller(type  = "seq", palette = "Greens") +
#'     scale_fill_br_distiller(type  = "div", palette = "PuOr") +
#'     scale_fill_diag_distiller(type  = "seq", palette = "Blues")
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
#' @export scale_fill_diag_gradient
scale_fill_diag_gradient <- function(..., low = "#132B43", high = "#56B1F7",
                                     space = "Lab",
                                     na.value = "grey50",
                                     guide = "colourbar",
                                     aesthetics = "fill_diag") {
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

#' @rdname scale_gradient
#' @export scale_fill_diag_gradient2
scale_fill_diag_gradient2 <- function(...,
                                      low = scales::muted("red"),
                                      mid = "white",
                                      high = scales::muted("blue"),
                                      midpoint = 0,
                                      space = "Lab",
                                      na.value = "grey50",
                                      guide = "colourbar",
                                      aesthetics = "fill_diag") {
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


#' @rdname scale_gradient
#' @export scale_fill_diag_gradientn
scale_fill_diag_gradientn <- function(..., colours,
                                      values = NULL,
                                      space = "Lab",
                                      na.value = "grey50",
                                      guide = "colourbar",
                                      aesthetics = "fill_diag",
                                      colors) {
    colours <- if (missing(colours)) colors else colours
    continuous_scale_asym(aesthetics, "gradientn",
                          scales::gradient_n_pal(colours, values, space),
                          na.value = na.value,
                          guide = guide,
                          ...)
}


#' @param type One of \code{"seq"} (sequential), \code{"div"} (diverging) or
#'     \code{"qual"} (qualitative)
#' @param palette If a string, will use that named palette. If a number, will
#'     index into the list of palettes of appropriate type
#' @param direction Sets the order of colours in the scale. If 1, the default,
#'     colours are as output by \code{RColorBrewer::brewer.pal()}. If -1, the
#'     order of colours is reversed.
#' @rdname scale_gradient
#' @export scale_fill_tl_distiller
scale_fill_tl_distiller <- function(...,
                                    type = "seq",
                                    palette = 1,
                                    direction = -1,
                                    values = NULL,
                                    space = "Lab",
                                    na.value = "grey50",
                                    guide = "colourbar",
                                    aesthetics = "fill_tl") {
    type <- match.arg(type, c("seq", "div", "qual"))
    if (type == "qual") {
        warning("Using a discrete colour palette in a continuous scale.\n
                Consider using type = \"seq\" or type = \"div\" instead",
                call. = FALSE)
    }
    pal <- scales::brewer_pal(type, palette, direction)(7)
    continuous_scale_asym(aesthetics, "distiller",
                          scales::gradient_n_pal(pal, values, space),
                          na.value = na.value,
                          guide = guide,
                          ...)
}


#' @rdname scale_gradient
#' @export scale_fill_br_distiller
scale_fill_br_distiller <- function(...,
                                    type = "seq",
                                    palette = 1,
                                    direction = -1,
                                    values = NULL,
                                    space = "Lab",
                                    na.value = "grey50",
                                    guide = "colourbar",
                                    aesthetics = "fill_br") {
    type <- match.arg(type, c("seq", "div", "qual"))
    if (type == "qual") {
        warning("Using a discrete colour palette in a continuous scale.\n
                Consider using type = \"seq\" or type = \"div\" instead",
                call. = FALSE)
    }
    pal <- scales::brewer_pal(type, palette, direction)(7)
    continuous_scale_asym(aesthetics, "distiller",
                          scales::gradient_n_pal(pal, values, space),
                          na.value = na.value,
                          guide = guide,
                          ...)
}


#' @rdname scale_gradient
#' @export scale_fill_diag_distiller
scale_fill_diag_distiller <- function(...,
                                    type = "seq",
                                    palette = 1,
                                    direction = -1,
                                    values = NULL,
                                    space = "Lab",
                                    na.value = "grey50",
                                    guide = "colourbar",
                                    aesthetics = "fill_diag") {
    type <- match.arg(type, c("seq", "div", "qual"))
    if (type == "qual") {
        warning("Using a discrete colour palette in a continuous scale.\n
                Consider using type = \"seq\" or type = \"div\" instead",
                call. = FALSE)
    }
    pal <- scales::brewer_pal(type, palette, direction)(7)
    continuous_scale_asym(aesthetics, "distiller",
                          scales::gradient_n_pal(pal, values, space),
                          na.value = na.value,
                          guide = guide,
                          ...)
}
