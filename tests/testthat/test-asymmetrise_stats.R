context("test-asymmetrise_stats")
library(broom)
library(dplyr)

test_that("stats asymmetrization works", {
    grps <- c("A", "B", "C")
    n_reps <- 5
    tib <- tibble::tibble(grp = rep(grps, n_reps),
                          val = rnorm(n_reps * length(grps)))
    tukey <- TukeyHSD(aov(val ~ grp, data = tib))

    if (utils::packageVersion("broom") > 0.7) {
        expect_error(prepare_data(grps),
                     "Could not handle input data; try turning into a tibble using the broom package")
    } else {
        expect_warning(prepare_data(grps))
    }

    expect_true(tibble::is_tibble(prepare_data(c())))
    expect_equal(prepare_data(tib), tib)
    expect_s3_class(prepare_data(tukey), "tbl_df")
    expect_true(tibble::is_tibble(prepare_data(tukey)))

    expect_equal(nrow(asymmetrise_stats(tukey)), 15)
    expect_equal(nrow(asymmetrize_stats(tukey)), 15)
})
