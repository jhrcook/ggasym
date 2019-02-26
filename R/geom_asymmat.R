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
#' @importFrom stringr str_detect
#' @import ggplot2
#' @export geom_asymmat
geom_asymmat <- function(mapping = NULL, data = NULL,
                         stat = "identity", position = "identity",
                         ...,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
    mapping_1 <- mapping[!str_detect(names(mapping), "fill_br")]
    # names(mapping_1)[str_detect(names(mapping_1), "fill_tl")] <- "fill"
    mapping_2 <- mapping[!str_detect(names(mapping), "fill_tl")]
    # names(mapping_2)[str_detect(names(mapping_2), "fill_br")] <- "fill"
    names(mapping_2)[[1]] <- "y"
    names(mapping_2)[[2]] <- "x"
    # browser()
    # open mapping and pass fill_tl as fill to tl layer and fill_br to br layer
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
    # browser()
    return(list(new_layer1, new_layer2))
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export GeomAsymmat
#' @ import ggplot2
#' @include geom-rect.r
GeomAsymmat <- ggproto("GeomAsymmat", GeomRect,
                    extra_params = c("na.rm", "which_triangle"),

                    setup_data = function(data, params) {
                        # browser()
                        data$width <- data$width %||% params$width %||% resolution(data$x, FALSE)
                        data$height <- data$height %||% params$height %||% resolution(data$y, FALSE)
                        # if (params$which_triangle == "tl") data$fill <- data$fill_tl
                        # else if (params$which_triangle == "br") data$fill <- data$fill_br

                        transform(data,
                                  xmin = x - width / 2,  xmax = x + width / 2,  width = NULL,
                                  ymin = y - height / 2, ymax = y + height / 2, height = NULL
                        )
                    },

                    draw_panel = function(self, data, panel_params, coord) {
                        if (!all(is.na(data$fill_tl))) {
                            data$fill <- data$fill_tl
                        } else if (!all(is.na(data$fill_br))) {
                            data$fill <- data$fill_br
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

ggname <- function(prefix, grob) {
    grob$name <- grid::grobName(grob, prefix)
    grob
}



scale_fill_tl_gradient <- function(..., low = "#132B43", high = "#56B1F7",
                                   space = "Lab",
                                   na.value = "grey50",
                                   guide = "colourbar",
                                   aesthetics = "fill_tl") {
    cs <- continuous_scale(aesthetics,
                     "gradient",
                     seq_gradient_pal(low, high, space),
                     na.value = na.value,
                     guide = guide, ...)
    cs$guide <- match.fun(paste0("guide_", cs$guide))()
    cs$guide$available_aes[stringr::str_which(cs$guide$available_aes, "fill")] <- "fill_tl"
    return(cs)
}

scale_fill_br_gradient <- function(..., low = "#132B43", high = "#56B1F7",
                                   space = "Lab",
                                   na.value = "grey50",
                                   guide = "colourbar",
                                   aesthetics = "fill_br") {
    cs <- continuous_scale(aesthetics,
                     "gradient",
                     seq_gradient_pal(low, high, space),
                     na.value = na.value,
                     guide = guide, ...)
    cs$guide <- match.fun(paste0("guide_", cs$guide))()
    cs$guide$available_aes[stringr::str_which(cs$guide$available_aes, "fill")] <- "fill_br"
    return(cs)
}




# FOR REFERENCE
# geom_tile <- function(mapping = NULL, data = NULL,
#                       stat = "identity", position = "identity",
#                       ...,
#                       na.rm = FALSE,
#                       show.legend = NA,
#                       inherit.aes = TRUE) {
#     layer(
#         data = data,
#         mapping = mapping,
#         stat = stat,
#         geom = GeomTile,
#         position = position,
#         show.legend = show.legend,
#         inherit.aes = inherit.aes,
#         params = list(
#             na.rm = na.rm,
#             ...
#         )
#     )
# }
