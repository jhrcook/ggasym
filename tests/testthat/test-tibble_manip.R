context("Tibble manipulations")
library(dplyr)

test_that("columns are swapped", {
    start_tib <- tibble(col_a = c(1:5),
                        col_b = LETTERS[1:5],
                        not_changed = c(11:15))
    final_tib <- tibble(col_a = LETTERS[1:5],
                        col_b = c(1:5),
                        not_changed = c(11:15))

    expect_equal(col_swap(start_tib, col_a, col_b), final_tib)
    expect_equal(dplyr::slice(start_tib, 0), dplyr::slice(start_tib, 0))
})
