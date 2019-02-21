




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


