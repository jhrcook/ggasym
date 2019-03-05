#' Add all missing comparisons between two columns
#'
#' @description This function prepares input data for \code{geom_asymmat} by
#'     adding in any missing comparisons to be plotted. Note that this function
#'     observes groups created with the \code{dplyr::group_by} function. For
#'     the ggasym package, this is useful for when you want to facet the plot:
#'     before "asymmetrizing" the data table, use \code{dplyr::group_by},
#'     passing the column name you wish to later facet by. This functionality
#'     is demonstrated in the second example, below.
#'
#' @param .data a tidy \code{data.frame} or \code{tibble}
#' @param .x,.y the data to add all comparisons between (ie. will be the
#'     x and y-axes for \code{geom_asymmat}
#'
#' @return a data table with new rows for the added comparisons
#'
#' @examples
#'
#' df <- data.frame(a = c("A", "B", "C"),
#'                  b = c("C", "A", "B"),
#'                  untouched = c(1, 2, 3),
#'                  grouping_value = c("group1", "group1", "group2"),
#'                  stringsAsFactors = FALSE)
#' df
#'
#' asymmetrise(df, a, b)
#'
#' grouped_df <- dplyr::group_by(df, grouping_value)
#' asymmetrise(grouped_df, a, b)
#'
#' @section Warning:
#' This function does it's best when \code{x} or \code{y} are factors. If they
#'     have the same levels, then they are maintained. If the levels partially
#'     overlap, they are merged. Otherwise, the values are turned into
#'     characters and all levels dropped. If you are using factors, save
#'     yourself the headache and make both columns factors with the desired
#'     levels.
#'
#' @importFrom rlang enquo eval_tidy !! :=
#' @importFrom magrittr %>%
#' @export asymmetrise
asymmetrise <- function(.data, .x, .y) {
    .x <- enquo(.x)
    .y <- enquo(.y)
    .x_data <- eval_tidy(.x, .data)
    .y_data <- eval_tidy(.y, .data)

    if (class(.x_data) == "factor" | class(.y_data) == "factor") {
        data_levels <- organize_levels(.x_data, .y_data)
        .data <- .data %>%
            dplyr::mutate(!!.x := as.character(!!.x),
                          !!.y := as.character(!!.y))
    } else {
        data_levels <- NULL
    }
    new_data <- dplyr::bind_rows(.data, swap_cols(.data, !!.x, !!.y)) %>%
        add_missing_combinations(!!.x, !!.y)

    if (!is.null(data_levels)) {
        new_data <- new_data %>%
            dplyr::mutate(!!.x := factor(!!.x, levels = data_levels),
                          !!.y := factor(!!.y, levels = data_levels))
    }

    return(new_data)
}


#' @rdname asymmetrise
#' @export asymmetrize
asymmetrize <- asymmetrise


#' Swap columns in a data.frame
#'
#' @description Swap columns \code{.x} and \code{.y} in \code{.data}.
#'
#' @param .data a data.frame (or tibble) object
#' @param .x,.y column names to switch
#'
#' @return a data.frame (or tibble) object with \code{.x} and \code{.y} swapped
#'
#' @examples
#' df <- data.frame(a = c("A", "B"),
#'                  b = c("C", "D"),
#'                  untouched = c(1, 2))
#' df
#'
#' swap_cols(df, a, b)
#'
#' @importFrom rlang enquo eval_tidy !! :=
#' @importFrom magrittr %>%
#' @export swap_cols
swap_cols <- function(.data, .x, .y) {
    groups <- dplyr::groups(.data)
    .data <- dplyr::ungroup(.data)
    .x <- enquo(.x)
    .y <- enquo(.y)
    .x_data <- eval_tidy(.x, .data)
    .y_data <- eval_tidy(.y, .data)
    new_data <- .data %>%
        dplyr::mutate(!!.x := .y_data,
                      !!.y := .x_data)
    return(dplyr::group_by(new_data, !!!groups))
}


#' Add missing combinations of x and y to a data.frame
#'
#' @description Add rows to \code{.data} to complete all combinations of
#'     columns \code{.x} and \code{.y}. Importantly, this function observes and
#'     maintains any groups created by \code{dplyr::group_by}.
#'
#' @param .data a data.frame (or tibble) object
#' @param .x,.y column names to make combinations of
#'
#' @return a data.frame (or tibble) with additional columns
#'
#' @examples
#' df <- data.frame(a = c("A", "B"),
#'                  b = c("C", "D"),
#'                  untouched = c(1, 2))
#' df
#'
#' add_missing_combinations(df, a, b)
#'
#' @importFrom rlang enquo eval_tidy !! :=
#' @importFrom magrittr %>%
#' @export add_missing_combinations
add_missing_combinations <- function(.data, .x, .y) {
    .x <- enquo(.x)
    .y <- enquo(.y)
    .x_data <- eval_tidy(.x, .data)
    .y_data <- eval_tidy(.y, .data)

    # handle levels if x or y is a factor
    if (class(.x_data) == "factor" | class(.y_data) == "factor") {
        data_levels <- organize_levels(.x_data, .y_data)
        .data <- .data %>%
            dplyr::mutate(!!.x := as.character(!!.x),
                          !!.y := as.character(!!.y))
        .x_data <- as.character(.x_data)
        .y_data <- as.character(.y_data)
    } else {
        data_levels <- NULL
    }

    if (is_grouped(.data)) {
        # call function over all groups
        # purrr::nest() %>% mutate(data = my_function(data)
        new_data <- .data %>%
            tidyr::nest(.key = ".grp_nest") %>%
            dplyr::mutate(.grp_nest = purrr::map(.grp_nest,
                    function(df) {
                        new_df <- bind_missing_combs(df, !!.x, !!.y)
                    })) %>%
            tidyr::unnest()
    } else {
        new_data <- bind_missing_combs(.data, !!.x, !!.y)
    }

    # if necessary, reinstate levels
    if (!is.null(data_levels)) {
        new_data <- new_data %>%
            dplyr::mutate(!!.x := factor(!!.x, levels = data_levels),
                          !!.y := factor(!!.y, levels = data_levels))
    }

    return(new_data)
}

#' Add the missing combinations of x and y
#'
#' @description Adds rows to the input data table to include any combinations
#'     of \code{.x} and \code{.y} that are not already present. All other
#'     columns (if any) are set to \code{NA}
#'
#' @param .data input data table
#' @param .x,.y names of the columns for which to add missing comparisons
#'
#' @return a data table with the new rows
#'
#' @examples
#' df <- data.frame(a = c("A", "B"),
#'                  b = c("C", "A"),
#'                  untouched = c(1, 2),
#'                  stringsAsFactors = FALSE)
#' df
#'
#' bind_missing_combs(df, a, b)
#'
#' @export bind_missing_combs
bind_missing_combs <- function(.data, .x, .y)  {
    .x <- enquo(.x)
    .y <- enquo(.y)
    others_combs <- get_other_combs(eval_tidy(.x, .data),
                                    eval_tidy(.y, .data))
    if (nrow(others_combs) > 0) {
        data_cp <- make_fill_df(.data, n_rows = nrow(others_combs)) %>%
            dplyr::mutate(!!.x := others_combs$Var1,
                          !!.y := others_combs$Var2)

        new_data <- dplyr::bind_rows(.data, data_cp)
    } else {
        new_data <- .data
    }
    return(new_data)
}

#' Get all combinations of values between two vectors
#'
#' @description Get all combinations of the values in vectors x and y that
#'     aren't already there.
#'
#' @param x,y two vectors
#'
#' @return data.frame of other possible combinations stored in \code{Var1} and
#'     \code{Var2} for \code{x} and \code{y}, respectively
#'
#' @examples
#' get_other_combs(LETTERS[1:2], LETTERS[1:2])
#'
#' @export get_other_combs
get_other_combs <- function(x, y) {
    current_combs <- paste(x, y, sep = "_")
    all_vals <- unique(c(x, y))
    all_combs <- expand.grid(all_vals, all_vals, stringsAsFactors = FALSE) %>%
        dplyr::mutate(comb = paste(Var1, Var2, sep = "_")) %>%
        dplyr::filter(!(comb %in% current_combs)) %>%
        dplyr::select(-comb)
    return(all_combs)
}

#' Make a data.frame of all a single value
#'
#' @description Makes a data.frame with the same columns of \code{df} and
#'     \code{n_rows} number of rows and all values \code{fill_val}
#'
#' @param df a data.frame (or tibble) object
#' @param n_rows number of rows for the final data.frame
#' @param fill_val value to fill all cells of the data.frame
#'
#' @return a data.frame (or tibble) with the desired number of rows filled
#'     with \code{fill_val}
#'
#' @examples
#' df <- data.frame(col_a = c("A", "B"),
#'                  col_b = c("C", "D"))
#' df
#'
#' make_fill_df(df, 5)
#'
#' @importFrom magrittr %>%
#' @export make_fill_df
make_fill_df <- function(df, n_rows = 1, fill_val = NA) {
    if (ncol(df) < 1) stop("df must have at least 1 column")
    if (n_rows < 1) stop("must request at least one row")
    na_df <- df %>% dplyr::slice(1) %>% dplyr::mutate_all(function(i) fill_val)
    na_df <- dplyr::bind_rows(purrr::map(seq_len(n_rows), ~ na_df))
    return(na_df)
}


#' Decides on the levels of factors x and y
#'
#' @description Organizes the levels to use for the two inputs. This is useful
#'     for when one wants to merge two vectors that are factors. Ideally, they
#'     have the same levels, in which case those are returned. If they have
#'     overlapping levels, then the levels are merged and sorted (using
#'     \code{sort}). Otherwise, the levels are dropped (returning \code{NULL})
#'
#' @param x,y Two factor vectors
#' @param ... passed to \code{sort}; see \code{?sort} for options
#'
#' @return vector of levels or \code{NULL} for no levels
#'
#' @examples
#' set.seed(0)
#' a <- factor(sample(LETTERS, 5), levels = LETTERS)
#' b <- factor(sample(LETTERS, 5), levels = LETTERS)
#'
#' a
#'
#' b
#'
#' organize_levels(a, b)
#'
#'@export organize_levels
organize_levels <- function(x, y, ...) {
    x_levels <- levels(x)
    y_levels <- levels(y)
    if (is.null(x_levels) | is.null(y_levels)) {
        # at leat x or y has no levels
        message("x and/or y have no levels, both coerced to char")
        return(NULL)
    } else if (identical(x_levels, y_levels)) {
        # identical levels
        return(x_levels)
    } else if (identical(intersect(x_levels, y_levels), character(0))) {
        # no overlap in levels
        message("completely different levels for x and y, coerced to char")
        return(NULL)
    } else if (length(intersect(x_levels, y_levels)) >= 1) {
        # partial overlap in levels
        message("partial overlap of levels, merging levels of x and y")
        return(sort(unique(c(x_levels, y_levels)), ...))
    } else {
        stop("Unforeseen condition in organizing levels --> open an Issue")
    }
}


#' Is a data table grouped?
#'
#' @description Determines if the input data.frame or tibble is grouped
#'     (using \code{dplyr::group_by}
#'
#' @param .data input \code{data.frame} or \code{tibble}
#'
#' @return boolean
#'
#' @examples
#' df <- data.frame(x = c(1:5), g = c(1,1,2,2,2))
#' is_grouped(df)
#'
#' is_grouped(dplyr::group_by(df, g))
#'
#' @export is_grouped
is_grouped <- function(.data) {
    !is.null(dplyr::groups(.data))
}
