context("test-scale_continuous_asym")

test_that("get core aesthetic name", {
    expect_warning(get_core_aes("not_real"),
                   regexp = "core aesthetic not found")
    expect_true(is.na(suppressWarnings(get_core_aes("not_real"))))
    expect_equal(get_core_aes("color"), "color")
    expect_equal(get_core_aes("colour"), "colour")
    expect_equal(get_core_aes("fill_tl"), "fill")
    expect_equal(get_core_aes("fill_brl"), "fill")
    expect_equal(get_core_aes("color_tl"), "color")
    expect_equal(get_core_aes("color_br"), "color")
    expect_equal(get_core_aes("colour_tl"), "colour")
    expect_equal(get_core_aes("colour_br"), "colour")
})

test_that("scale_fill_tl/br_gradient values populate properly", {
    tib <- data.frame(grp1 = c("A", "A", "B"),
                      grp2 = c("B", "C", "C"),
                      val_1 = c(1, 2, NA),
                      val_2 = c(-1, 0, 1))
    tib <- asymmetrise(tib, grp1, grp2)
    g1 <- ggplot(tib) +
        geom_asymmat(aes(x = grp1, y = grp2, fill_tl = val_1, fill_br = val_2)) +
        scale_fill_tl_gradient(low = "lightpink", high = "red", na.value = "green")
    g2 <- g1 +
        scale_fill_br_gradient(low = "lightblue1", high = "dodgerblue", na.value = "orange")

    g1_build <- ggplot2::ggplot_build(g1)
    g2_build <- ggplot2::ggplot_build(g2)

    expect_equal(g1$scales$n(), 1)
    expect_true(g1$scales$has_scale("fill_tl"))
    expect_false(g1$scales$has_scale("fill_br"))
    expect_equal(g2$scales$n(), 2)
    expect_true(g2$scales$has_scale("fill_tl"))
    expect_true(g2$scales$has_scale("fill_br"))

    expect_equal(g1_build$data[[1]]$fill_tl, c("#FFB6C1", "#FF0000", "green"))
    expect_equal(g1_build$data[[1]]$fill_br, c(NA, NA, NA))
    expect_equal(g1_build$data[[2]]$fill_tl, c(NA, NA, NA))
    expect_equal(g1_build$data[[2]]$fill_br, c(-1, 0, 1))
    expect_equal(g2_build$data[[1]]$fill_tl, c("#FFB6C1", "#FF0000", "green"))
    expect_equal(g2_build$data[[1]]$fill_br, c(NA, NA, NA))
    expect_equal(g2_build$data[[2]]$fill_tl, c(NA, NA, NA))
    expect_equal(g2_build$data[[2]]$fill_br, c("#BFEFFF", "#86BEFF", "#1E90FF"))
})


test_that("scale_fill_tl/br_gradient2 values populate properly", {
    tib <- data.frame(grp1 = c("A", "A", "A", "B", "B", "C"),
                      grp2 = c("B", "C", "D", "C", "D", "D"),
                      val_1 = c(1, 2, NA, 0, 10, 5),
                      val_2 = c(-2, -1, 0, 1, 2, 3))
    atib <- asymmetrise(tib, grp1, grp2)
    g1 <- ggplot(atib) +
        geom_asymmat(aes(x = grp1, y = grp2, fill_tl = val_1, fill_br = val_2)) +
        scale_fill_tl_gradient2(low = "lightpink", mid = "white", high = "red",
                                na.value = "green")
    g2 <- g1 +
        scale_fill_br_gradient2(low = "lightblue1",
                                mid = "grey50",
                                high = "dodgerblue")

    g1_build <- ggplot2::ggplot_build(g1)
    g2_build <- ggplot2::ggplot_build(g2)

    expect_equal(g1$scales$n(), 1)
    expect_true(g1$scales$has_scale("fill_tl"))
    expect_false(g1$scales$has_scale("fill_br"))
    expect_equal(g2$scales$n(), 2)
    expect_true(g2$scales$has_scale("fill_tl"))
    expect_true(g2$scales$has_scale("fill_br"))

    expect_equal(g1_build$data[[1]]$fill_tl, c("#FFECE5", "#FFD9CB", "green", "#FFFFFF", "#FF0000", "#FF9E81"))
    expect_equal(g1_build$data[[1]]$fill_br, rep(NA, nrow(tib)))
    expect_equal(g1_build$data[[2]]$fill_tl, rep(NA, nrow(tib)))
    expect_equal(g1_build$data[[2]]$fill_br, tib$val_2)
    expect_equal(g2_build$data[[1]]$fill_tl,  c("#FFECE5", "#FFD9CB", "green", "#FFFFFF", "#FF0000", "#FF9E81"))
    expect_equal(g2_build$data[[1]]$fill_br, rep(NA, nrow(tib)))
    expect_equal(g2_build$data[[2]]$fill_tl, rep(NA, nrow(tib)))
    expect_equal(g2_build$data[[2]]$fill_br, c("#AAC8D3", "#95A3A8", "#7F7F7F", "#7584A9", "#5F8AD3", "#1E90FF"))
})


test_that("scale_fill_tl/br_gradientn values populate properly", {
    tib <- data.frame(grp1 = c("A", "A", "A", "B", "B", "C"),
                      grp2 = c("B", "C", "D", "C", "D", "D"),
                      val_1 = c(1, 2, NA, 0, 10, 5),
                      val_2 = c(-2, -1, 0, 1, 2, 3))
    atib <- asymmetrise(tib, grp1, grp2)
    g1 <- ggplot(atib) +
        geom_asymmat(aes(x = grp1, y = grp2, fill_tl = val_1, fill_br = val_2)) +
        scale_fill_tl_gradientn(colours = terrain.colors(10))
    g2 <- g1 +
        scale_fill_br_gradientn(colours = heat.colors(10))

    g1_build <- ggplot2::ggplot_build(g1)
    g2_build <- ggplot2::ggplot_build(g2)

    expect_equal(g1$scales$n(), 1)
    expect_true(g1$scales$has_scale("fill_tl"))
    expect_false(g1$scales$has_scale("fill_br"))
    expect_equal(g2$scales$n(), 2)
    expect_true(g2$scales$has_scale("fill_tl"))
    expect_true(g2$scales$has_scale("fill_br"))

    expect_equal(g1_build$data[[1]]$fill_tl,c("#2AB400", "#5AC300", "grey50", "#00A600", "#F2F2F2", "#E7D520"))
    expect_equal(g1_build$data[[1]]$fill_br, rep(NA, nrow(tib)))
    expect_equal(g1_build$data[[2]]$fill_tl, rep(NA, nrow(tib)))
    expect_equal(g1_build$data[[2]]$fill_br, tib$val_2)
    expect_equal(g2_build$data[[1]]$fill_tl, c("#2AB400", "#5AC300", "grey50", "#00A600", "#F2F2F2", "#E7D520"))
    expect_equal(g2_build$data[[1]]$fill_br, rep(NA, nrow(tib)))
    expect_equal(g2_build$data[[2]]$fill_tl, rep(NA, nrow(tib)))
    expect_equal(g2_build$data[[2]]$fill_br, c("#FF0000", "#FF4300", "#FF8400", "#FFC500", "#FFFF19", "#FFFFBF"))
})


test_that("ggplot's midrescaler is working", {
    num_scale <- mid_rescaler(mid = 5)
    v <- c(0:10)
    sv <- seq(0, 1, 0.1)

    expect_true(is.function(mid_rescaler(mid = 10)))
    expect_true(is.function(mid_rescaler(mid = NA)))
    expect_equal(num_scale(v), sv)
    expect_error(num_scale("A"), regexp = "applied to an object of class")
    expect_true(is.null(num_scale(c())))
})

