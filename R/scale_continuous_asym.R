#' Continuous scale constructor for 'ggasym'
#'
#' @description This is a this wrapper around \code{continuous_scale()} from
#'     the 'ggplot2' package. It is generally best to call this function
#'     implicitly using one of the wrappers that have the general naming
#'     scheme of \code{scale_*_tl/br_*()} (such as
#'     \code{scale_fill_tl_gradient()}).
#'
#' @param aesthetics The names of the aesthetics that this scale works with
#' @param scale_name The name of the scale
#' @param palette A palette function that when called with a numeric vector
#'     with values between 0 and 1 returns the corresponding values in the
#'     range the scale maps to.
#' @param na.value Missing values will be replaced with this value.
#' @param guide A function used to create a guide or its name. See
#'     \code{guides()} for more info.
#' @param ... other input is passed on to \code{ggplot2::continuous_scale()};
#'     see \code{?ggplot2::continuous_scale} for complete documentation
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
#' tib <- asymmetrise(tib, g1, g2)
#' ggplot(tib) +
#' geom_asymmat(aes(x = g1, y = g2, fill_tl = val_1, fill_br = val_2)) +
#'     scale_fill_tl_gradient(low = "lightpink", high = "tomato") +
#'     scale_fill_br_gradient(low = "lightblue1", high = "dodgerblue") +
#'     labs(fill_tl =  "top-left fill", fill_br = "bottom-right fill")
#'
#' @import ggplot2
#' @export continuous_scale_asym
continuous_scale_asym <- function(aesthetics,
                                  scale_name,
                                  palette,
                                  na.value,
                                  guide,
                                  ...) {
    cs <- continuous_scale(aesthetics = aesthetics,
                           scale_name = scale_name,
                           palette = palette,
                           na.value = na.value,
                           guide = guide,
                           ...)
    cs <- add_extras_to_colorscale(cs, aesthetics)
    return(cs)
}


# adds on the extras required for geom_asymmat
add_extras_to_colorscale <- function(cs, aesthetics) {
    core_aes <- get_core_aes(aesthetics)
    if (is.character(cs$guide)) {
        cs$guide <- match.fun(paste0("guide_", cs$guide))()
    }
    available_aes_idx <- index_aesthetic(cs, core_aes)
    cs$guide$available_aes[available_aes_idx] <- aesthetics
    return(cs)
}

# return the location for the new aesthetic to replace
index_aesthetic <- function(cs, aesthetic) {
    available_aes <- cs$guide$available_aes
    if (identical(available_aes, "any")) {
        return(1)
    } else {
        return(stringr::str_which(available_aes, aesthetic))
    }
}

# gets the original aesthetic (colour, color, or fill)
get_core_aes <- function(aesthetics) {
    a <- stringr::str_extract(aesthetics, "fill|color|colour")
    if (is.na(a)) {
        warning(paste("core aesthetic not found; input:", aesthetics))
    }
    return(a)
}
