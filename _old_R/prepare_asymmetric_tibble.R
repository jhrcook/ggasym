#' Copy a tibble, but flip .x and .y
#'
#' @param .data A tibble
#' @param .x,.y Column names to swap
#' @return The same tibble with the values in columns \code{.x} and \code{.y} swapped
#' @examples
#' suppressMessages(library(dplyr))
#' suppressMessages(library(magrittr))
#'
#' tib <- tibble(col_a = c(1:5),
#'               col_b = LETTERS[1:5],
#'               not_changed = runif(5))
#'
#' tib
#'
#' switched_ab <- tib %>%
#'     col_swap(col_a, col_b)
#'
#' switched_ab
#'
#' @importFrom rlang := !! enquo eval_tidy
#' @importFrom magrittr %>%
#' @export col_swap
col_swap <- function(.data, .x, .y) {
    .x <- enquo(.x)
    .y <- enquo(.y)
    y_new <- eval_tidy(.x, .data)
    x_new <- eval_tidy(.y, .data)
    dplyr::mutate(.data,
           !!.x := x_new,
           !!.y := y_new)
}

#' Complete the \eqn{n \times n} matrix and make asymmetric
#'
#' This function is designed to make the "other half" of the \eqn{n \times n}
#' matrix. It copies the matrix, reversing \code{.x} and \code{.y}, followed by
#' only keeping the top-left (\code{.tl}) and bottom-right (\code{br}) values
#' in the correct rows.
#'
#' @param .data a tibble with at least four columns for the x and y-axes
#'     (\code{.x} and \code{.y}) and the top-left and bottom-right values
#'     (\code{.tl} and \code{.br})
#' @param .x,.y x and y-axes column names
#' @param .tl,.br top-left and bottom right column names
#'
#' @examples
#' library(tibble)
#'
#' tib <- tibble(a = LETTERS[1:5],
#'               b = LETTERS[10:14],
#'               val_1 = c(0.70, 0.05, 0.14, 0.60, 0.83),
#'               val_2 = c(0.78, 0.25, 0.74, 0.26, 0.16))
#'
#' tib
#'
#' prepare_asymmetric_tibble(tib, a, b, val_1, val_2)
#'
#' @importFrom rlang := !! enquo eval_tidy
#' @importFrom magrittr %>%
#' @export prepare_asymmetric_tibble
prepare_asymmetric_tibble <- function(.data, .x, .y, .tl, .br) {
    .x <- enquo(.x)
    .y <- enquo(.y)
    .tl <- enquo(.tl)
    .br <- enquo(.br)
    x_vals <- eval_tidy(.x, .data)
    y_vals <- eval_tidy(.y, .data)
    all_levels <- sort(unique(c(x_vals, y_vals)))
    mod_tib <- dplyr::bind_rows(.data,
                         col_swap(.data, !!.x, !!.y)) %>%
        dplyr::arrange(!!.x, !!.y) %>%
        dplyr::mutate(!!.x := factor(!!.x, levels = all_levels),
                      !!.y := factor(!!.y, levels = all_levels),
                      !!.tl := ifelse(factor_is_greater(!!.y, !!.x), !!.tl, NA),
                      !!.br := ifelse(factor_is_greater(!!.x, !!.y), !!.br, NA))
    return(mod_tib)
}
