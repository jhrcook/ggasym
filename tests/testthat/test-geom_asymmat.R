context("test-geom_asymmat")

test_that("geom_asymmat works", {
    tib <- data.frame(grp1 = c("A", "A", "B"),
                      grp2 = c("B", "C", "C"),
                      val_1 = c(1, 2, NA),
                      val_2 = c(-1, 0, 1))
    g_asymmat <- ggplot(tib) +
        geom_asymmat(aes(x = grp1, y = grp2, fill_tl = val_1, fill_br = val_2))
    g_asymmat_build <- ggplot_build(g_asymmat)
    g_tile <- ggplot(tib) +
        geom_tile(aes(x = grp1, y = grp2, fill = val_1))

    expect_equal(class(g_asymmat), class(g_tile))
    expect_equal(length(g_asymmat$layers), 2)
    expect_true(all(c("fill_tl", "fill_br") %in% names(g_asymmat$labels)))
    expect_true(all(c("val_1", "val_2") %in% unlist(g_asymmat$labels)))

    expect_equal(g_asymmat_build$data[[1]]$fill_tl, tib$val_1)
    expect_equal(g_asymmat_build$data[[1]]$fill_br, c(NA, NA, NA))
    expect_equal(g_asymmat_build$data[[2]]$fill_tl, c(NA, NA, NA))
    expect_equal(g_asymmat_build$data[[2]]$fill_br, tib$val_2)
})
