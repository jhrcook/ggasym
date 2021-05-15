#' Discrete scale constructor for ggasym
#'
#' @description This is a this wrapper around \code{discrete_scale()} from
#'     the ggplot2 package. It is generally best to call this function
#'     implicitly using one of the wrappers that have the general naming
#'     scheme of \code{scale_*_tl/br_*} (such as \code{scale_fill_tl_distiller}
#'     ).
#'
#' @param aesthetics The names of the aesthetics that this scale works with
#' @param scale_name The name of the scale
#' @param palette A palette function that when called with a numeric vector
#'     with values between 0 and 1 returns the corresponding values in the
#'     range the scale maps to.
#' @param na.value Missing values will be replaced with this value.
#' @param guide A function used to create a guide or its name. See
#'     \code{guides()} for more info.
#' @param ... other input is passed on to \code{ggplot2::discrete_scale()};
#'     see \code{?ggplot2::continuous_scale} for complete documentation
#'
#' @examples
#' library(tibble)
#' library(ggplot2)
#' tib <- tibble(
#'   g1 = c("A", "A", "B"),
#'   g2 = c("B", "C", "C"),
#'   val_1 = c(1, 2, 3),
#'   val_2 = c(-1, 0, 1)
#' )
#'
#' tib
#'
#' tib <- asymmetrise(tib, g1, g2)
#' ggplot(tib) +
#'   geom_asymmat(aes(x = g1, y = g2, fill_tl = val_1, fill_br = val_2)) +
#'   scale_fill_tl_brewer()
#' scale_fill_br_gradient(low = "lightpink", high = "tomato") +
#'   labs(fill_tl = "top-left fill", fill_br = "bottom-right fill")
#' @import ggplot2
#' @export discrete_scale_asym
discrete_scale_asym <- function(aesthetics,
                                scale_name,
                                palette,
                                na.value = NA,
                                guide = "legend",
                                ...) {
  cs <- discrete_scale(
    aesthetics = aesthetics,
    scale_name = scale_name,
    palette = palette,
    na.value = na.value,
    guide = guide,
    ...
  )
  cs <- add_extras_to_colorscale(cs, aesthetics)
  return(cs)
}

#' Discrete colour scales geom_asymmat
#'
#' @description TODO
#'
#' @param ... Other arguments passed on to \code{discrete_scale()}
#'     to control name, limits, breaks, labels and so forth.
#' @param type One of seq (sequential), div (diverging) or qual (qualitative).
#' @param pallete If a string, will use that named palette. If a number, will
#'     index into the list of palettes of appropriate \code{type}.
#' @param direction Sets the order of colours in the scale. If 1, the default,
#'     colours are as output by \code{RColorBrewer::brewer.pal()}. If -1,
#'     the order of colours is reversed.
#' @param aesthetics Character string or vector of character strings listing
#'     the name(s) of the aesthetic(s) that this scale works with. This can be
#'     useful, for example, to apply colour settings to the \code{fill_tl} and
#'     \code{fill_br} aesthetics at the same time, via
#'     \code{aesthetics = c("fill_tl", "fill_br")}.
#' @examples
#' # TODO
#' @export scale_fill_tl_brewer
scale_fill_tl_brewer <- function(...,
                                 type = "seq",
                                 palette = 1,
                                 direction = 1,
                                 aesthetics = "fill_tl") {
  discrete_scale_asym(
    aesthetics,
    "brewer",
    scales::brewer_pal(type, palette, direction),
    ...
  )
}
