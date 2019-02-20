
# return is factor a is greater than factor b (a and b are same-length vectors)
factor_is_greater <- function(a, b) {
    # TODO: take an expression instead of only computing greater than
    ai <- which_level(a)
    bi <- which_level(b)
    return(ai > bi)
}



# level (as int) of a factor (x is a vector)
which_level <- function(x) {
    map_int(x, ~ str_which(levels(x), as.character(.x)))
}



# duplicates .data but swtiching the values in .x and .y
col_swap <- function(.data, .x, .y) {
    .x <- enquo(.x)
    .y <- enquo(.y)
    y_new <- eval_tidy(.x, .data)
    x_new <- eval_tidy(.y, .data)
    mutate(.data,
           !!.x := x_new,
           !!.y := y_new)
}


# returns a tibbles with g1 <=> g2
add_diag_rows <- function(.data, ..., .grp = NA) {
    all_vals <- unique(c(tib$g1, tib$g2))
    diag_tib <- tib %>%
        slice(1:length(all_vals)) %>%
        mutate(g1 = all_vals,
               g2 = all_vals)
    return(diag_tib)
}
