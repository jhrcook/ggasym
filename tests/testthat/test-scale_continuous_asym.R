context("test-scale_continuous_asym")

test_that("get core aesthetic name", {
    expect_equal(get_core_aes("color"), "color")
    expect_equal(get_core_aes("colour"), "colour")
    expect_equal(get_core_aes("fill_tl"), "fill")
    expect_equal(get_core_aes("fill_brl"), "fill")
    expect_equal(get_core_aes("color_tl"), "color")
    expect_equal(get_core_aes("color_br"), "color")
    expect_equal(get_core_aes("colour_tl"), "colour")
    expect_equal(get_core_aes("colour_br"), "colour")
})
