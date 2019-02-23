#' Example results from a multiple-way comparison of the levels of enrichment
#'     of mutations in pathways associated with the Hallmarks of Cancer
#'
#' Biological signaling pathways were assessed for their mutational burden
#' across several cancer types (generically named \code{"tissue A-G"}). The
#' pathways were grouped into one of the 10 Hallmarks of Cancer (as defined by
#' the \href{https://cancer.sanger.ac.uk/cosmic}{Catalogue of Somatic Mutations in Cancer}
#'
#' @format A tibble with 166 rows and 4 variables:
#' \describe{
#'     \item{hallmark}{Hallmark of Cancer}
#'     \item{comparison}{tissue-to-tissue comparison}
#'     \item{estimate}{the \code{estimate} value returned by a statstical test
#'         - is the average of the first tissue minus that of the second}
#'     \item{adj.p.value}{Benjamini and Hochberg corrected q-values}
#' }
#'
#' #>
#' #>  # A tibble: 166 x 4
#' #>    hallmark                           comparison        estimate adj.p.value
#' #>    <chr>                              <chr>                <dbl>       <dbl>
#' #>  1 Activating Invasion and Metastasis tissue A-tissue B  -0.282     7.03e- 1
#' #>  2 Activating Invasion and Metastasis tissue C-tissue B  -0.504     1.31e- 1
#' #>  3 Activating Invasion and Metastasis tissue D-tissue B  -1.69      0.
#' #>  4 Activating Invasion and Metastasis tissue E-tissue B  -0.382     4.00e- 1
#' #>  5 Activating Invasion and Metastasis tissue F-tissue B  -1.37      5.49e-10
#' #>  6 Activating Invasion and Metastasis tissue G-tissue B  -1.91      4.80e-10
#' #>  7 Activating Invasion and Metastasis tissue C-tissue A  -0.222     9.01e- 1
#' #>  8 Activating Invasion and Metastasis tissue D-tissue A  -1.41      6.14e-11
#' #>  9 Activating Invasion and Metastasis tissue E-tissue A  -0.0992    9.98e- 1
#' #> 10 Activating Invasion and Metastasis tissue F-tissue A  -1.09      3.78e- 7
#' #>  # … with 156 more rows
"enrichment_data"
