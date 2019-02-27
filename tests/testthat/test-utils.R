context("test-utils")

test_that("if (a is not NULL) then a, else b", {
    expect_equal(NULL %||% 1, 1)
    expect_equal(NULL %||% -1, -1)
    expect_equal(1 %||% NULL, 1)
    expect_equal(-1 %||% NULL, -1)
    expect_equal(NULL %||% NULL, NULL)
    expect_equal(NULL %||% NULL %||% 1, 1)
    expect_equal("a" %||% NULL, "a")
    expect_equal(NULL %||% "a", "a")
    expect_equal(NA %||% 1, NA)
    expect_equal(FALSE %||% "a", FALSE)
    expect_equal("" %||% "a", "")
})

