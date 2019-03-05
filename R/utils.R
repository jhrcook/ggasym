#' Determine the level of a value in a vector of type \code{factor}
#'
#' @param x vectors of type \code{factor}
#'
#' @return a vector holding the corresponding level of the input factors
#'
#' @examples
#' first <- factor(c("J", "O", "S", "H"), LETTERS)
#' which_level(first)
#'
#' @export which_level
which_level <- function(x) {
    purrr::map_int(x, ~ stringr::str_which(levels(x), as.character(.x)))
}


#' Determines if the level of a is greater than that of b
#'
#' @param a,b Two same-length, same-leveled vectors of type \code{factor}
#'
#' @return a single boolean vector of type \code{logical}
#'
#' @examples
#' first <- c("J", "O", "S", "H")
#' last <- c("C", "O", "O", "K")
#' first <- factor(first, LETTERS)
#' last <- factor(last, LETTERS)
#' factor_is_greater(first, last)
#'
#'@export factor_is_greater
factor_is_greater <- function(a, b) {
    stopifnot(class(a) == "factor" & class(b) == "factor")
    ai <- which_level(a)
    bi <- which_level(b)
    return(ai > bi)
}
