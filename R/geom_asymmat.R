#' Asymmetrically filled symmetric matrix (using 'ggplot2')
#'
#' Generate an asymmetric matrix with different fill values for top-left
#'     and bottom-right triangles and along the diagonal as a
#'     \code{ggplot()} object
#'
#' @param mapping Set of aesthetic mappings created by \code{aes()} or
#'     \code{aes_()}. If specified and \code{inherit.aes = TRUE} (the default),
#'     it is combined with the default mapping at the top level of the plot.
#'     You must supply mapping if there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three options:
#'     If \code{NULL} (the default) the data is inherited from the plot data as
#'     specified in the call to \code{ggplot()}. A data frame, or other object,
#'     will override the plot data. All objects will be fortified to produce
#'     a data frame. See \code{fortify()} for which variables will be created.
#'     A function will be called with a single argument, the plot data. The
#'     return value must be a data frame, and will be used as the layer data.
#' @param stat The statistical transformation to use on the data for this
#'     layer, as a string.
#' @param position Position adjustment, either as a string, or the result of a
#'     call to a position adjustment function.
#' @param ... Other arguments passed on to \code{layer()}. These are often
#'     aesthetics, used to set an aesthetic to a fixed value, like
#'     \code{colour = "red"} or \code{size = 3}. They may also be parameters
#'     to the paired geom/stat.
#' @param na.rm If \code{FALSE}, the default, missing values are removed with a
#'     warning. If \code{TRUE}, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends?
#'     \code{NA} (the default) includes if any aesthetics are mapped.
#'     \code{FALSE} never includes, and \code{TRUE} always includes. It can
#'     also be a named logical vector to finely select the aesthetics to
#'     display.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#'     than combining with them. This is most useful for helper functions that
#'     define both data and aesthetics and should not inherit behaviour from the
#'     default plot specification, e.g. \code{borders()}.
#'
#' @return A \code{ggplot()} object of an asymmetrically-colored
#'     \eqn{x \times y} matrix with \code{fill_tl} data coloring the top-left
#'     triangle, \code{fill_br} coloring the bottom-left triangle, and
#'     \code{fill_diag} coloring along the diagonal.
#'
#' @examples
#' library(tibble)
#' library(ggplot2)
#' suppressMessages(library(dplyr))
#' tib <- tibble(g1 = c("A", "A", "B"),
#'               g2 = c("B", "C", "C"),
#'               val_1 = c(1, 2, 3),
#'               val_2 = c(-1, 0, 1))
#'
#' tib
#'
#' tib <- asymmetrise(tib, g1, g2)
#' tib$val_3 <- NA
#' tib$val_3[tib$g1 == tib$g2] <- c(1, 2, 3)
#' ggplot(tib, aes(x = g1, y = g2)) +
#'     geom_asymmat(aes(fill_tl = val_1, fill_br = val_2, fill_diag = val_3)) +
#'     scale_fill_br_gradient(low = "lightblue1", high = "dodgerblue") +
#'     scale_fill_tl_gradient(low = "lightpink", high = "tomato") +
#'     scale_fill_diag_gradient(low = "aquamarine", high = "forestgreen") +
#'     labs(fill_tl =  "top-left fill", fill_br = "bottom-right fill")
#'
#' @importFrom stringr str_detect
#' @import ggplot2
#' @export geom_asymmat
geom_asymmat <- function(mapping = NULL, data = NULL,
                         stat = "identity", position = "identity",
                         ...,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
    # open mapping and remove tw of the three aesthetics
    mapping_1 <- mapping[!str_detect(names(mapping), "fill_br|fill_diag")]
    mapping_2 <- mapping[!str_detect(names(mapping), "fill_tl|fill_diag")]
    mapping_3 <- mapping[!str_detect(names(mapping), "fill_tl|fill_br")]

    new_layer1 <- layer(
        data = data,
        mapping = mapping_1,
        stat = stat,
        geom = GeomAsymmat,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            na.rm = na.rm,
            which_triangle = "tl",
            ...
        )
    )
    new_layer2 <- layer(
        data = data,
        mapping = mapping_2,
        stat = stat,
        geom = GeomAsymmat,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            na.rm = na.rm,
            which_triangle = "br",
            ...
        )
    )
    new_layer3 <- layer(
        data = data,
        mapping = mapping_3,
        stat = stat,
        geom = GeomAsymmat,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            na.rm = na.rm,
            which_triangle = "diag",
            ...
        )
    )
    return(list(new_layer1, new_layer2, new_layer3))
}

#' GeomAsymmat
#'
#' A 'ggproto' object for the 'ggasym' package and used by \code{geom_asymmat}
#'
#' @section Warning:
#' \code{GeomAsymmat} is subject to change in future versions.
#'     Use at your own risk. If dependent on \code{GeomAsymmat},
#'     it is advisable to include tests with a cached version to test for
#'     equivalence.
#'
#' @importFrom rlang %||%
#' @import ggplot2
#' @export GeomAsymmat
GeomAsymmat <- ggproto(
    "GeomAsymmat",
    GeomRect,
    extra_params = c("na.rm", "which_triangle"),

    setup_data = function(data, params) {
        # rearrange x and y for ggasym
        check_all_combinations(data)
        data <- organize_xy(data, params)
        data$width <- data$width %||% params$width %||% resolution(data$x, FALSE)
        data$height <- data$height %||% params$height %||% resolution(data$y, FALSE)
        transform(data,
                  xmin = x - width / 2,  xmax = x + width / 2,  width = NULL,
                  ymin = y - height / 2, ymax = y + height / 2, height = NULL
        )
    },

    draw_panel = function(self, data, panel_params, coord) {
        # coordinate which fill_* gets used for fill
        if (all(is.character(data$fill_tl))) {
            data$fill <- data$fill_tl
        } else if (all(is.character(data$fill_br))) {
            data$fill <- data$fill_br
        } else if (all(is.character(data$fill_diag))) {
            data$fill <- data$fill_diag
        } else {
            data$fill <- NA
        }

        if (!coord$is_linear()) {
            aesthetics <- setdiff(
                names(data), c("x", "y", "xmin", "xmax", "ymin", "ymax")
            )

            polys <- lapply(split(data, seq_len(nrow(data))), function(row) {
                poly <- rect_to_poly(row$xmin, row$xmax, row$ymin, row$ymax)
                aes <- new_data_frame(row[aesthetics])[rep(1,5), ]

                GeomPolygon$draw_panel(cbind(poly, aes), panel_params, coord)
            })

            ggname("bar", do.call("grobTree", polys))
        } else {
            coords <- coord$transform(data, panel_params)
            ggname("geom_asymmat", grid::rectGrob(
                coords$xmin, coords$ymax,
                width = coords$xmax - coords$xmin,
                height = coords$ymax - coords$ymin,
                default.units = "native",
                just = c("left", "top"),
                gp = grid::gpar(
                    col = coords$colour,
                    fill = alpha(coords$fill, coords$alpha),
                    lwd = coords$size * .pt,
                    lty = coords$linetype,
                    lineend = "butt"
                )
            ))
        }
    },

    default_aes = aes(fill_tl = NA, fill_br = NA, fill_diag = NA,
                      colour = NA, size = 0.1, linetype = 1,
                      alpha = NA, width = NA, height = NA),

    required_aes = c("x", "y"),

    draw_key = draw_key_polygon
)


# built-in to ggplot2, but not exported
ggname <- function(prefix, grob) {
    grob$name <- grid::grobName(grob, prefix)
    grob
}


# not exported from ggplot2
rect_to_poly <- function(xmin, xmax, ymin, ymax) {
    data.frame(
        y = c(ymax, ymax, ymin, ymin, ymax),
        x = c(xmin, xmax, xmax, xmin, xmin)
    )
}


# swap x and y if necessary for top-left and bottom-right
organize_xy <- function(data, params) {
    if (!any(names(params) == "which_triangle")) return(data)
    if (params$which_triangle == "tl") {
        data <- data %>% dplyr::filter(data$x < data$y)
    } else if (params$which_triangle == "br") {
        data <- data %>% dplyr::filter(data$x > data$y)
    } else if (params$which_triangle == "diag") {
        data <- data %>% dplyr::filter(data$x == data$y)
    }
    return(data)
}


# check that data is symmetric
check_all_combinations <- function(data) {
    if(!identical(data, add_missing_combinations(data, x, y))) {
        stop(paste("All combinations not present in data.\n",
                   "Use \"asymmetrize(data, x, y)\" to fix."))
    }
    invisible(TRUE)
}


# for "check_all_combinations"
utils::globalVariables(c("x", "y"), add = TRUE)
