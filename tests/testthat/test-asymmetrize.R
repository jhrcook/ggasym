context("test-asymmetrize")

test_that("create a single fill data frame", {
    expect_error(make_fill_df(data.frame(), 2),
                 regexp = "df must have at least 1 column")
    expect_error(make_fill_df(data.frame(x = 1), 0),
                 regexp = "must request at least one row")
    df <- data.frame(x = 1:10, y = LETTERS[1:10])
    expect_equal(make_fill_df(df),
                 data.frame(x = NA, y = NA))
    expect_equal(make_fill_df(df, 5),
                 data.frame(x = rep(NA, 5), y = rep(NA, 5)))
    expect_equal(make_fill_df(df, 5, "Z"),
                 data.frame(x = c("Z", "Z", "Z", "Z", "Z"),
                            y = c("Z", "Z", "Z", "Z", "Z"),
                            stringsAsFactors = FALSE))
})


test_that("adding all combinations", {
    complete_df <- data.frame(x = c("A", "A", "B", "B"),
                              y = c("A", "B", "A", "B"),
                              stringsAsFactors = FALSE)
    partial_df_1 <- data.frame(x = c("A", "B"),
                               y = c("A", "B"),
                               val = c(1, 2),
                               stringsAsFactors = FALSE)
    partial_df_2 <- data.frame(x = c("A", "B", "B", "A"),
                               y = c("A", "B", "A", "B"),
                               val = c(1, 2, NA, NA),
                               stringsAsFactors = FALSE)
    expect_equal(add_missing_combinations(complete_df, x, y), complete_df)
    expect_equal(add_missing_combinations(partial_df_1, x, y), partial_df_2)
})


test_that("columns are swapped",  {
    df <- data.frame(x = c("A", "B", "B", "A"),
                     y = c("A", "B", "A", "B"),
                     val = c(1, 2, NA, NA))
    df_swapped <- data.frame(x = c("A", "B", "A", "B"),
                             y = c("A", "B", "B", "A"),
                             val = c(1, 2, NA, NA))
    expect_equal(swap_cols(df, x, y), df_swapped)
})


test_that("data frame is asymmeterized", {
    df <- data.frame(a = c("A", "B"),
                     b = c("C", "D"),
                     stringsAsFactors = FALSE)
    a_df <- data.frame(a = c("A", "B", "C", "D", "A", "B", "D", "A", "B", "C",
                             "B", "C", "D", "A", "C", "D"),
                       b = c("C", "D", "A", "B", "A", "A", "A", "B", "B", "B",
                             "C", "C", "C", "D", "D", "D"),
                       stringsAsFactors = FALSE)
    expect_equal(asymmetrise(df, a, b), a_df)
    expect_equal(asymmetrize(df, a, b), a_df)
})
