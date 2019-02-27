
# returns the number of asterisks for a p-value
map_pval_asterisk <- function(pval) {
    if (pval < 0.001) return("***")
    else if (pval < 0.01) return("**")
    else if (pval < 0.05) return("*")
    else return("")
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

