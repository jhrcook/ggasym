#' Copy a tibble, but flip .x and .y
#'
#' @param .data A tibble
#' @param .x,.y Column names to swap
#' @return The same tibble with the values in columns \code{.x} and \code{.y} swapped
#' @examples
#' tib <- tibble(col_a = c(1:5),
#'               col_b = LETTERS[1:5],
#'               not_changed = runif(5))
#'
#' tib
#'
#' #> # A tibble: 5 x 3
#' #>   col_a col_b not_changed
#' #>   <int> <chr>       <dbl>
#' #> 1     1 A           0.904
#' #> 2     2 B           0.603
#' #> 3     3 C           0.632
#' #> 4     4 D           0.937
#' #> 5     5 E           0.850
#'
#' switched_ab <- tib %>%
#'     col_swap(col_a, col_b)
#'
#' switched_ab
#'
#' #> # A tibble: 5 x 3
#' #>   col_a col_b not_changed
#' #>   <chr> <int>       <dbl>
#' #> 1 A         1       0.904
#' #> 2 B         2       0.603
#' #> 3 C         3       0.632
#' #> 4 D         4       0.937
#' #> 5 E         5       0.850
#'
#' @export

col_swap <- function(.data, .x, .y) {
    .x <- rlang::enquo(.x)
    .y <- rlang::enquo(.y)
    y_new <- rlang::eval_tidy(.x, .data)
    x_new <- rlang::eval_tidy(.y, .data)
    dplyr::mutate(.data,
           !!.x := x_new,
           !!.y := y_new)
}


#' Returns a tibble of the diagonal of a hypothetical symmetrix matrix
#'
#' Returns a tibble where everything is the same except that the two selected
#' columns are set equal. It is used to set diagonal values
#'
#' @param .data a tibble
#' @param .x,.y the \emph{x} and \emph{y} column names of the hypothetical
#'     matrix
#' @param ... column names to group by
#' @return a tibble where everything is the same except that the two selected
#'     columns are set equal
#' @examples
#' "NOT USED"
# add_diag_rows <- function(.data, .x, .y, ...) {
#     all_vals <- unique(c(tib$g1, tib$g2))
#     diag_tib <- tib %>%
#         slice(1:length(all_vals)) %>%
#         mutate(g1 = all_vals,
#                g2 = all_vals)
#     return(diag_tib)
# }


#' Complete the \math{n x n} matrix and make asymmetric
#'
#' @export
prepare_asymmetic_tibble <- function(.data, .x, .y, .tl, .br) {
    .x <- rlang::enquo(.x)
    .y <- rlang::enquo(.y)
    x_vals <- rlang::eval_tidy(.x, .data)
    y_vals <- rlang::eval_tidy(.y, .data)
    all_levels <- sort(unique(c(x_vals, y_vals)))
    mod_tib <- bind_rows(.data,
                         col_swap(.data, !!.x, !!.y)) %>%
        arrange(!!.x, !!.y) %>%
        mutate(!!.x := factor(!!.x, levels = all_levels),
               !!.y := factor(!!.y, levels = all_levels),
               !!.tl := ifelse(factor_is_greater(!!.x, !!.y), !!.tl, NA),
               !!.br := ifelse(factor_is_greater(!!.x, !!.y), !!.br, NA),
               .diag_val = ifelse(!!.x == !!.y, 1, NA))
    return(mod_tib)
}
