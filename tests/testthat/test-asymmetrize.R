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


test_that("factor levels are handeled correctly", {
    fa <- factor(c("J", "O", "S", "H"), levels = LETTERS)
    fb <- factor(c("C", "O", "O", "K"), levels = LETTERS)
    fc <- factor(LETTERS[1:4])
    fd <- factor(LETTERS[2:6])
    char1 <- LETTERS[1:5]
    char2 <- LETTERS[10:20]
    expect_equal(organize_levels(fa, fb), LETTERS)
    expect_null(organize_levels(c(), c()))
    expect_null(organize_levels(char1, char2))
    expect_equal(organize_levels(fc, fd), LETTERS[1:6])
})


test_that("adding all combinations", {
    complete_df <- data.frame(x = c("A", "A", "B", "B"),
                              y = c("A", "B", "A", "B"))
    partial_df_1 <- data.frame(x = c("A", "B"),
                               y = c("A", "B"),
                               val = c(1, 2))
    partial_df_2 <- data.frame(x = c("A", "B", "B", "A"),
                               y = c("A", "B", "A", "B"),
                               val = c(1, 2, NA, NA))
    expect_equal(add_missing_combinations(complete_df, x, y), complete_df)
    expect_equal(add_missing_combinations(partial_df_1, x, y), partial_df_2)
})


test_that("columns are swapped",  {
    df <- data.frame(x = c(LETTERS[1:4]),
                     y = c(letters[10:13]),
                     val = c(1, 2, NA, NA),
                     grp = c(1, 1, 2, 2))
    grouped_df <- dplyr::group_by(df, grp)
    df_swapped <- data.frame(x = c(letters[10:13]),
                             y = c(LETTERS[1:4]),
                             val = c(1, 2, NA, NA),
                             grp = c(1, 1, 2, 2))
    expect_equal(swap_cols(df, x, y), df_swapped)
    expect_equal(swap_cols(grouped_df, x, y),
                 dplyr::group_by(df_swapped, grp))
})


test_that("data frame is asymmeterized", {
    df_factors <- data.frame(a = c("A", "B"),
                             b = c("C", "D"))
    df_char <- data.frame(a = c("A", "B"),
                          b = c("C", "D"),
                          stringsAsFactors = FALSE)
    a_df <- data.frame(a = c("A", "B", "C", "D", "A", "B", "D", "A", "B", "C",
                             "B", "C", "D", "A", "C", "D"),
                       b = c("C", "D", "A", "B", "A", "A", "A", "B", "B", "B",
                             "C", "C", "C", "D", "D", "D"),
                       stringsAsFactors = FALSE)
    expect_equal(asymmetrise(df_factors, a, b), a_df)
    expect_equal(asymmetrize(df_factors, a, b), a_df)

    expect_equal(asymmetrise(df_char, a, b), a_df)
    expect_equal(asymmetrize(df_char, a, b), a_df)
})


# TESTS TO ADD:
#  asymmetrise/ze obeys groups
#  `get_other_combs`
