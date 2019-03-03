#' Asymmetrically filled symmetric matrix (using ggplot)
#'
#' Generate an asymmetric matrix with different fill values for top-left
#'     and bottom-right triangles as a \code{ggplot} object
#'
#' @param mapping Set of aesthetic mappings created by aes() or aes_(). If
#'     specified and \code{inherit.aes = TRUE} (the default), it is combined with
#'     the default mapping at the top level of the plot. You must supply
#'     mapping if there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three options:
#'     If NULL, the default, the data is inherited from the plot data as
#'     specified in the call to \code{ggplot()}. A data.frame, or other object,
#'     will override the plot data. All objects will be fortified to produce
#'     a data frame. See \code{fortify()} for which variables will be created.
#'     A function will be called with a single argument, the plot data. The
#'     return value must be a data.frame, and will be used as the layer data.
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
#'     \code{NA}, the default, includes if any aesthetics are mapped.
#'     \code{FALSE} never includes, and \code{TRUE} always includes. It can
#'     also be a named logical vector to finely select the aesthetics to
#'     display.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#'     than combining with them. This is most useful for helper functions that
#'     define both data and aesthetics and shouldn't inherit behaviour from the
#'     default plot specification, e.g. \code{borders()}.
#' @param rearrange_xy This parameter controls whether \code{geom_asymmat} can
#'     rearrange \code{x} and \code{y} such that the lower level is along
#'     \code{x} for top-left and along \code{y} for bottom-right. This behavior
#'     is demonstrated in the vignette (\code{vignette("ggasym")})
#'
#' @return A ggplot object of an asymmetrically-colored \eqn{x \times y}
#'     matrix with \code{fill_tl} data coloring the top-left triangle and
#'     \code{fill_br} coloring the bottom-left triangle.
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
#'     scale_fill_tl_gradient(low = "lightpink", high = "tomato") +
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
    # open mapping and pass fill_tl as fill to tl layer and fill_br to br layer
    # browser()
    mapping_1 <- mapping[!str_detect(names(mapping), "fill_br")]
    mapping_2 <- mapping[!str_detect(names(mapping), "fill_tl")]
    # names(mapping_2)[[1]] <- "y"
    # names(mapping_2)[[2]] <- "x"
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
    return(list(new_layer1, new_layer2))
}

#' GeomAsymmat
#'
#' A ggproto object for the ggasym package and used by \code{geom_asymmat}
#'
#' @section Warning:
#' This \code{GeomAsymmat} is very much still in development and warrant to change
#'     without notice. Use at your own risk. If dependent on \code{GeomAsymmat}
#'     it is advisable to include tests with a cached version to test for
#'     equivalence.
#'
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
        # what I add to add the scaled fill_tl/br to `fill`
        # TODO: I have removed the error by defaulting to `NA`, but still need
        #   to figure out how the default blue is called
        # TODO: this needs to be a better check that they are colors
        #   make the check it's own function
        if (all(is.character(data$fill_tl))) {
            data$fill <- data$fill_tl
        } else if (all(is.character(data$fill_br))) {
            data$fill <- data$fill_br
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

    default_aes = aes(fill_tl = NA, fill_br = NA,
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
        data <- data %>% dplyr::filter(data$x <= data$y)
        # .new_x <- ifelse(data$x <= data$y, data$x, data$y)
        # .new_y <- ifelse(data$x <= data$y, data$y, data$x)
        # data$x <- .new_x
        # data$y <- .new_y
    } else if (params$which_triangle == "br") {
        data <- data %>% dplyr::filter(data$x >= data$y)
        # .new_x <- ifelse(data$x >= data$y, data$x, data$y)
        # .new_y <- ifelse(data$x >= data$y, data$y, data$x)
        # data$x <- .new_x
        # data$y <- .new_y
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
