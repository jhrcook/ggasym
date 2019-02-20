
# Plots the  heatmaps of statistical signifance for "slape_figure_script.R"

# duplicates .data but swtiching the values in .x and .y
col_swap <- function(.data, .x, .y) {
    .x <- enquo(.x)
    .y <- enquo(.y)
    y_new <- eval_tidy(.x, .data)
    x_new <- eval_tidy(.y, .data)
    mutate(.data,
           !!.x := x_new,
           !!.y := y_new)
}

# level (as int) of a factor (x is a vector)
which_level <- function(x) {
    map_int(x, ~ str_which(levels(x), as.character(.x)))
}

# return is factor a is greater than factor b (a and b are same-length vectors)
factor_is_greater <- function(a, b) {
    # TODO: take an expression instead of only computing greater than
    ai <- which_level(a)
    bi <- which_level(b)
    return(ai > bi)
}

# returns a tibbles with g1 == g2
add_diag_rows <- function(.data, ..., .grp = NA) {
    all_vals <- unique(c(tib$g1, tib$g2))
    diag_tib <- tib %>%
        slice(1:length(all_vals)) %>%
        mutate(g1 = all_vals,
               g2 = all_vals)
    return(diag_tib)
}

# prepares the tibble needed for geom_tile
prepare_asymmetic_tibble <- function(tib) {
    all_levels <- sort(unique(c(tib$g1, tib$g2)))
    mod_tib <- bind_rows(tib,
                         col_swap(tib, g1, g2)) %>%
        arrange(g1, g2) %>%
        mutate(g1 = factor(g1, levels = all_levels),
               g2 = factor(g2, levels = all_levels),
               estimate = ifelse(factor_is_greater(g2, g1), estimate, NA),
               log10_pval = ifelse(factor_is_greater(g1, g2), log10_pval, NA),
               diag_val = ifelse(g1 == g2, 1, NA))
    return(mod_tib)
}

# adds asterisk and adjusts size according to number of values for the hallmark
set_labels_for_pvals <- function(tib) {
    mod_tib <- tib %>%
        mutate(label = map_chr(adj.p.value, map_pval_asterisk),
               label = ifelse(is.na(log10_pval), "", label)) %>%
        group_by(hallmark) %>%
        mutate(label_size = rescale(sqrt(dplyr::n()), from = c(7, 10), to = c(5, 3)),
               label_size = min(max(label_size, 3), 5)) %>%
        ungroup()
    return(mod_tib)
}

# constructs a asymmetric heatmap using two color aesthetics for
# top and bottom triangles of matrix
make_statsig_heatmaps_plotting <- function(tib) {
    g <- prepare_asymmetic_tibble(tib) %>%
        set_labels_for_pvals() %>%
        ggplot(aes(x = g1, y = g2)) +
        facet_wrap(~ hallmark, nrow = 2, scales = "free") +
        geom_tile(aes(fill = estimate), color = "black") +
        scale_fill_gradient2(low = "dodgerblue", high = "tomato",
                             na.value = NA) +
        labs(fill = "Estimate") +
        new_scale_fill() +
        geom_tile(aes(fill = log10_pval), color = "black") +
        scale_fill_gradient(low = "#F7FCF5", high = "#238B45",
                            na.value = NA) +
        geom_text(aes(label = label, size = label_size),
                  color = "black", vjust = 0.75) +
        theme(panel.background = element_rect(fill = "grey50"),
              panel.border = element_rect(fill = NA, color = "black"),
              axis.title = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1),
              panel.grid = element_blank()) +
        scale_x_discrete(expand = c(0, 0)) +
        scale_y_discrete(expand = c(0, 0)) +
        scale_size_continuous(range = c(3, 5), guide = FALSE) +
        labs(fill = "-log10( q-val )")
    return(g)
}

# prepare stats_tib for plotting as asymmetric matrix
make_statsig_heatmaps <- function(st_tib) {
    g <- st_tib %>%
        mutate(comparison = str_replace_all(comparison, "_", " "),
               g1 = str_split_fixed(comparison, "-", 2)[, 1],
               g2 = str_split_fixed(comparison, "-", 2)[, 2],
               adj.p.value = ifelse(adj.p.value == 0, 1E-10, adj.p.value),
               log10_pval = -log10(adj.p.value)) %>%
        make_statsig_heatmaps_plotting()
}


