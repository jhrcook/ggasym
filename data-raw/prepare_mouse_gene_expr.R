# Preparing mouse liver gene expression data set

library(dplyr)
library(magrittr)
library(readr)
library(stringr)

# raw data file (unzipped)
expr_file <- "data-raw/GDS6248.soft"

# filter low variance genes (q = quantile to cut at)
filter_low_var <- function(df, q) {
    v <- apply(df[, 2:ncol(df)], 1, var)
    var_quant <- quantile(v, q)
    idx <- v >= var_quant
    return(df[idx, ])
}

# read in expression data
# everything after "!dataset_table_begin" is gene expression data in the file
# except for the last line, which I explicitly remove
file_lines <- readLines(expr_file)
start_line <- str_which(file_lines, "!dataset_table_begin")
moexpr_data <- readr::read_tsv(expr_file, skip = start_line) %>%
    dplyr::rename(gene = IDENTIFIER) %>%
    dplyr::select(-ID_REF) %>%
    group_by(gene) %>%
    slice(1) %>%
    ungroup() %>%
    na.omit() %>%
    filter(str_detect(gene, "^[:upper:]{1}[:lower:]")) %>%
    filter_low_var(0.75)

# extract high or normal fat diet information for each mouse
start_line <- str_which(file_lines, "#GSM994790")  # line of first mouse
end_line <- str_which(file_lines, "#GSM994837")    # line of last mouse
mouse_data_lines <- file_lines[start_line:end_line]
mouse <- str_extract(mouse_data_lines, "(?<=#)GSM[:digit:]+")  # get mouse IDs
diet <- str_extract(mouse_data_lines, "High|Normal")           # diet
mouse_diet <- tibble(mouse_id = mouse, diet = diet)

moexpr_data <- moexpr_data[, colnames(moexpr_data) %in% c(mouse_diet$mouse_id, "gene")]

# save to "data"
save(moexpr_data, file = "data/moexpr_data.RData",
     compress = "xz", compression_level = 9)
save(mouse_diet, file = "data/mouse_diet.RData")
