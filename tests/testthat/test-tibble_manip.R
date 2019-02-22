context("Tibble manipulations")
library(dplyr)
library(tibble)

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

test_that("tibble is made asymmetric", {
    start_tib <- tibble(a = LETTERS[1:5],
                        b = LETTERS[10:14],
                        val_1 = c(0.70, 0.05, 0.14, 0.60, 0.83),
                        val_2 = c(0.78, 0.25, 0.74, 0.26, 0.16))
    asym_tib <- tibble(a = factor(c(LETTERS[1:5], LETTERS[10:14])),
                       b = factor(c(LETTERS[10:14], LETTERS[1:5]),
                                  levels = c(LETTERS[1:5], LETTERS[10:14])),
                       val_1  = c(0.70, 0.05, 0.14, 0.60, 0.83, rep(NA, 5)),
                       val_2 = c(rep(NA, 5), 0.78, 0.25, 0.74, 0.26, 0.16))

    expect_equal(prepare_asymmetric_tibble(start_tib, a, b, val_1, val_2),
                 asym_tib)
})
