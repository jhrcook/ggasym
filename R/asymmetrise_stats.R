#' Prepare an asymmetric data table from a statistical test
#'
#' @description This function prepares the results of a statistical test for
#'     plotting using `geom_asymmat` from the ggasym package. For more
#'     information, see \code{vignette(ggasym-stats)}
#'
#' @param .data either the results of a statistical test or the tidy tibble
#'     from using the \code{broom::tidy()} function
#' @param comparison_sep the separation used between the names being compared;
#'     it is usually a hyphen (set as default here); since it is passed as the
#'     \code{pattern} parameter to \code{stringr::str_split_fixed()}, this can
#'     be any regular expression that will reliably split \code{.comparison}
#'
#' @return a tibble object that can be used as direct input for ggplot2 for
#'     use  with the \code{geom_asymmat} geom
#'
#' @importFrom rlang enquo !! :=
#' @importFrom magrittr %>%
#' @export asymmetrise_stats
asymmetrise_stats <- function(.data, comparison_sep = "-") {
    .data <- prepare_data(.data)
    new_data <- .data %>%
        dplyr::mutate(x = stringr::str_split_fixed(comparison,
                                                   comparison_sep, 2)[, 1],
                      y = stringr::str_split_fixed(comparison,
                                                   comparison_sep, 2)[, 2])
    new_data <- dplyr::bind_rows(new_data, swap_cols(new_data, x, y))
    return(asymmetrise(new_data, x, y))
}


#' @rdname asymmetrise_stats
#' @export asymmetrize_stats
asymmetrize_stats <- asymmetrise_stats


#' Prepares the input data into asymmetrise_stats
#'
#' @description Tries to make the data ready for use in the
#'     \code{asymmetrise_stats} function using \code{broom::tidy}
#'
#' @param .data input data of either a tibble, data.frame, or results from a
#'     statistical test
#'
#' @return a tibble data table
#'
#' @section Warning:
#'     If you repeatedly get errors, try preparing the data before hand using
#'     \code{broom::tidy(.data)}
#'
#' @examples
#' a <- rnorm(10, mean = 1, sd = 1)
#' b <- rnorm(10, mean = 1.5, sd = 1)
#' prepare_data(t.test(a, b))
#'
#' @export prepare_data
prepare_data <- function(.data) {
    if (is.data.frame(.data) | tibble::is_tibble(.data)) {
        return(.data)
    } else {
        new_data <- try(broom::tidy(.data))
        if ("try-error" %in% class(new_data)) {
            stop("Could not handle input data; try turning into a tibble using the broom package")
        } else if (tibble::is_tibble(new_data)) {
            return(new_data)
        } else {
            stop("Unable to parse data")
        }
    }
}


# for "asymmetrise_stats"
utils::globalVariables(c("comparison", "x", "y"), add = TRUE)
