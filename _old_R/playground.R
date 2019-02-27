# library(tidyverse)
#
#
# draw_panel_fxn <- function(data, panel_params, coord) {
#     coords <- coord$transform(data, panel_params)
#     grid::pointsGrob(coords$x,
#                      coords$y,
#                      pch = coords$shape,
#                      gp = grid::gpar(col = coords$color))
# }
#
# GeomSimplePoint <- ggproto("GeomSimplePoint", Geom,
#                            required_aes = c("x", "y"),
#                            default_aes = aes(shape = 19, colour = "black"),
#                            draw_key = draw_key_point,
#                            draw_panel = draw_panel_fxn)
#
# geom_simple_point <- function(mapping = NULL, data = NULL,
#                               stat = "identity", position = "identity",
#                               na.rm = FALSE,
#                               show.legend = NA,
#                               inherit.aes = TRUE,
#                               ...) {
#     layer(
#         geom = GeomSimplePoint,
#         mapping = mapping,
#         data = data,
#         stat = stat,
#         position = position,
#         show.legend = show.legend,
#         inherit.aes = inherit.aes,
#         params = list(na.rm = na.rm, ...)
#     )
# }
#
# ggplot(mpg, aes(displ, hwy)) + geom_simple_point()
#
#
#
#
#
#
#
#
# #### ggnewscale example
#
# library(ggplot2)
# library(ggnewscale)
# # Equivalent to melt(volcano)
# topography <- expand.grid(x = 1:nrow(volcano),
#                           y = 1:ncol(volcano))
# topography$z <- c(volcano)
#
# # point measurements of something at a few locations
# set.seed(42)
# measurements <- data.frame(x = runif(30, 1, 80),
#                            y = runif(30, 1, 60),
#                            thing = rnorm(30))
#
# g1 <- ggplot(mapping = aes(x, y)) +
#     geom_contour(data = topography, aes(z = z, color = stat(level))) +
#     scale_color_viridis_c(option = "D")
#
# g2 <- ggplot(mapping = aes(x, y)) +
#     geom_contour(data = topography, aes(z = z, color = stat(level))) +
#     scale_color_viridis_c(option = "D") +
#     new_scale_color()
#
# g3 <- ggplot(mapping = aes(x, y)) +
#     geom_contour(data = topography, aes(z = z, color = stat(level))) +
#     scale_fill_gradient(low = "blue", high = "red") +
#     new_scale_color() +
#     geom_point(data = measurements, size = 3, aes(color = thing)) +
#     scale_color_viridis_c(option = "A")
#
#
# g4 <- ggplot(mapping = aes(x, y)) +
#     geom_contour(data = topography, aes(z = z, color = stat(level))) +
#     geom_point(data = measurements, size = 3, aes(color = thing)) +
#     scale_color_viridis_c(option = "A")
#
#
# bump_aes_scale <- function(scale, new_aes) {
#     # browser()
#     old_aes <- scale$aesthetics[remove_new(scale$aesthetics) %in% new_aes]
#     if (length(old_aes) != 0) {
#         new_aes <- paste0(old_aes, "_new")
#
#         scale$aesthetics[scale$aesthetics %in% old_aes] <- new_aes
#
#         if (is.character(scale$guide)) {
#             scale$guide <- match.fun(paste("guide_", scale$guide, sep = ""))()
#         }
#         scale$guide$available_aes[scale$guide$available_aes %in% old_aes] <- new_aes
#     }
#     scale
# }
#
#
# #### geom_tile browsing
#
# my_geom_tile <- function(mapping = NULL, data = NULL,
#                       stat = "identity", position = "identity",
#                       ...,
#                       na.rm = FALSE,
#                       show.legend = NA,
#                       inherit.aes = TRUE) {
#     layer(
#         data = data,
#         mapping = mapping,
#         stat = stat,
#         geom = MyGeomTile,
#         position = position,
#         show.legend = show.legend,
#         inherit.aes = inherit.aes,
#         params = list(
#             na.rm = na.rm,
#             ...
#         )
#     )
# }
#
#
# MyGeomTile <- ggproto("MyGeomTile", GeomRect,
#                     extra_params = c("na.rm"),
#
#                     setup_data = function(data, params) {
#                         data$width <- data$width %||% params$width %||% resolution(data$x, FALSE)
#                         data$height <- data$height %||% params$height %||% resolution(data$y, FALSE)
#
#                         transform(data,
#                                   xmin = x - width / 2,  xmax = x + width / 2,  width = NULL,
#                                   ymin = y - height / 2, ymax = y + height / 2, height = NULL
#                         )
#                     },
#
#                     draw_panel = function(data, panel_params, coord) {
#                         coords <- coord$transform(data, panel_params)
#                         grid::pointsGrob(
#                             coords$x, coords$y,
#                             pch = coords$shape,
#                             gp = grid::gpar(col = coords$colour)
#                         )
#
#
#                     default_aes = aes(fill = "grey20", colour = NA, size = 0.1, linetype = 1,
#                                       alpha = NA, width = NA, height = NA),
#
#                     required_aes = c("x", "y", "fill_tl", "fill_br"),
#
#                     draw_key = draw_key_polygon
# )
#
# scale_fill_gradient_tl <- function(..., low = "#132B43", high = "#56B1F7", space = "Lab",
#                                 na.value = "grey50", guide = FALSE, aesthetics = "fill_tl") {
#     continuous_scale(aesthetics, "gradient", scales::seq_gradient_pal(low, high, space),
#                      na.value = na.value, guide = guide, ...)
# }
#
# ggplot(mpg) +
#     my_geom_tile(aes(x = manufacturer, y = class, fill_tl = displ, fill_br = model)) +
#     scale_fill_gradient_tl(low = "blue", high = "red")
#
#
# ggplot_add.fill_tl <- function(object, plot, object_name) {
#     new_data <-
# }
#
#
#
#
#
#
#
# #### Example of adding another layer of geom_point
#
#
# geom_highlight_point <- function(expr) {
#     structure(list(expr = rlang::enquo(expr)), class = "highlight")
# }
#
# ggplot_add.highlight <- function(object, plot, object_name) {
#     browser()
#     new_data <- dplyr::filter(plot$data, !! object$expr)
#     new_layer <- geom_point(data = new_data,
#                             mapping = plot$mapping,
#                             colour = alpha("red", 0.5),
#                             size = 5)
#     plot$layers <- append(plot$layers, new_layer)
#     plot
# }
#
# d <- data.frame(foo = 11:30,
#                 bar = 11:30,
#                 baz = rep(c("A", "B", "C"),length.out = 20),
#                 col = runif(20),
#                 stringsAsFactors = FALSE)
#
# ggplot(d, aes(foo, bar, color = col)) +
#     geom_point(aes(color = col)) +  # for comparison
#     geom_highlight_point(bar > 20 & baz == "A") +
#     scale_color_gradient(low = "blue", high = "red")
