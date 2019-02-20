
# Prepare the example data for the package from my own data set

library(magrittr)
library(tidyverse)

ori_tib <- readRDS("stats_tib.rds")  # load my real data

# get all KRAS alleles
all_kras_alleles <- ori_tib %>%
    pull(comparison) %>%
    str_split("-") %>%
    unlist() %>%
    unique()

# make a dict of allele => gene
allele_to_gene <- paste("gene", LETTERS[1:length(all_kras_alleles)])
names(allele_to_gene) <- all_kras_alleles

# replace all alleles with the gene
mod_tib <- ori_tib
for (i in 1:length(allele_to_gene)) {
    allele <- names(allele_to_gene)[[i]]
    gene <- allele_to_gene[[i]]
    mod_tib %<>% mutate(comparison = str_replace_all(comparison, allele, gene))
}

# select only relevant columns
mod_tib %<>%
    select(hallmark, comparison, estimate, adj.p.value)

# rename to a better variable name and save to data directory
enrichment_data <- mod_tib
save(list = c("enrichment_data"), file = "../data/enrichment_example.RData")
