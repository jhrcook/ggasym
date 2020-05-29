#' Prepare an asymmetric data table from a statistical test
#'
#' @description This function prepares the results of a statistical test for
#'     plotting using `geom_asymmat` from the ggasym package. For more
#'     information, see \code{vignette(ggasym-stats)}
#'
#' @param df either the results of a statistical test or the tidy tibble
#'     from using the \code{broom::tidy()} function
#' @param comparison_sep the separation used between the names being compared;
#'     it is usually a hyphen (set as default here); since it is passed as the
#'     \code{pattern} parameter to \code{stringr::str_split_fixed()}, this can
#'     be any regular expression that will reliably split \code{.comparison}
#'
#' @return a tibble object that can be used as direct input for 'ggplot2' for
#'     use  with the \code{geom_asymmat} geom
#'
#' @importFrom rlang enquo !! :=
#' @importFrom magrittr %>%
#' @export asymmetrise_stats
asymmetrise_stats <- function(df, contrast_sep = "-") {
    df <- prepare_data(df)
    new_df <- df %>%
        dplyr::mutate(x = stringr::str_split_fixed(contrast,
                                                   contrast_sep, 2)[, 1],
                      y = stringr::str_split_fixed(contrast,
                                                   contrast_sep, 2)[, 2])
    new_df <- dplyr::bind_rows(new_df, swap_cols(new_df, x, y))
    return(asymmetrise(new_df, x, y))
}


#' @rdname asymmetrise_stats
#' @export asymmetrize_stats
asymmetrize_stats <- asymmetrise_stats


#' Prepares the input data into asymmetrise_stats
#'
#' @description Tries to make the data ready for use in the
#'     \code{asymmetrise_stats()} function using \code{broom::tidy()}
#'
#' @param df input data of either a \code{tibble}, \code{data.frame}, or results from a
#'     statistical test
#'
#' @return a \code{tibble} data table
#'
#' @section Warning:
#'     If you repeatedly get errors, try preparing the data before-hand using
#'     \code{broom::tidy(df)}
#'
#' @examples
#' a <- rnorm(10, mean = 1, sd = 1)
#' b <- rnorm(10, mean = 1.5, sd = 1)
#' prepare_data(t.test(a, b))
#'
#' @export prepare_data
prepare_data <- function(df) {
    if (is.data.frame(df) | tibble::is_tibble(df)) {
        return(df)
    } else {
        new_df <- tryCatch(
            broom::tidy(df),
            error = function(x) {
                stop("Could not handle input data; try turning into a tibble using the broom package")
            }
        )

        if (tibble::is_tibble(new_df)) {
            return(new_df)
        } else {
            stop("Unable to parse data")
        }
    }
}


# for "asymmetrise_stats"
utils::globalVariables(c("comparison", "x", "y"), add = TRUE)
