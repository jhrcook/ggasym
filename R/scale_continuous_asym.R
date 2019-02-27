#' Continuous scale constructor for ggasym
#'
#' @description This is a this wrapper around \code{continuous_scale()} from
#'     the ggplot2 package.
#'
#' @param aesthetics The names of the aesthetics that this scale works with
#' @param scale_name The name of the scale
#' @param palette A palette function that when called with a numeric vector
#'     with values between 0 and 1 returns the corresponding values in the
#'     range the scale maps to.
#' @param na.value Missing values will be replaced with this value.
#' @param guide A function used to create a guide or its name. See guides()
#'     for more info.
#' @param ... other input is passed on to \code{ggplot2::continuous_scale()};
#'     see \code{?ggplot2::continuous_scale} for complete documentation
#'
#' @return a scales object \code{FINISH THIS}
#'
#' @import ggplot2
#' @export continuous_scale_asym
continuous_scale_asym <- function(aesthetics,
                                  scale_name,
                                  palette,
                                  na.value,
                                  guide, ...) {
    cs <- continuous_scale(aesthetics = aesthetics,
                           scale_name = scale_name,
                           palette = palette,
                           na.value = na.value,
                           guide = guide, ...)
    cs <- add_extras_to_colorscale(cs, aesthetics)

    return(cs)
}

# adds on the extras required for geom_asymmat
add_extras_to_colorscale <- function(cs, aesthetics) {
    core_aes <- get_core_aes(aesthetics)
    if (is.character(cs$guide)) {
        cs$guide <- match.fun(paste0("guide_",cs$guide))()
    }
    available_aes_idx <- stringr::str_which(cs$guide$available_aes, core_aes)
    cs$guide$available_aes[available_aes_idx] <- aesthetics
    return(cs)
}

# gets the original aesthetic (colour, color, or fill)
get_core_aes <- function(aesthetics) {
    stringr::str_extract(aesthetics, "fill|color|colour")
}
