#' Prepare an asymmetric data table from a statistical test
#'
#' @description This function prepares the results of a statistical test for
#'     plotting using `geom_asymmat` from the ggasym package. For more
#'     information, see \code{vignette(ggasym-stats)}
#'
#' @param .data either the results of a statistical test or the tidy tibble
#'     from using the \code{broom::tidy()} function
#' @param .comparison the column name of the comparison made for the row
#' @param sep the separation used in the \code{.comparison} column; it is
#'     usually a hyphen (set as default here); since it is passed as the
#'     \code{pattern} parameter to \code{stringr::str_split_fixed()}, this can
#'     be any regular expression that will reliably split \code{.comparison}
#'
#' @return a tibble object that can be used as direct input for ggplot2 for
#'     use  with the \code{geom_asymmat} geom
#'
#' @importFrom rlang enquo !! :=
#' @importFrom magrittr %>%
#' @export asymmetrise_stats
asymmetrise_stats <- function(.data, .comparison, sep = "-") {
    .data <- prepare_data(.data)
    .comparison = enquo(.comparison)
    new_data <- .data %>%
        mutate(x = stringr::str_split_fixed(!!.comparison, sep, 2)[, 1],
               y = stringr::str_split_fixed(!!.comparison, sep, 2)[, 2])
    new_data <- dplyr::bind_rows(new_data, swap_cols(new_data, x, y))
    return(asymmetrise(new_data, x, y))
}


# prepare the input data into asymmetrise_stats
prepare_data <- function(.data) {
    if (is.data.frame(.data) | is_tibble(.data)) {
        return(.data)
    } else {
        new_data <- try(broom::tidy(.data))
        if ("try-error" %in% class(new_data)) {
            stop("Could not handle input data; try turning into a tibble using the broom package")
        } else if (is_tibble(new_data)) {
            return(new_data)
        } else {
            stop("Unable to parse data")
        }
    }
}
