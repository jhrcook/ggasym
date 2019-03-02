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
#'     \item{estimate}{the \code{estimate} value returned by a statistical test
#'         - is the average of the first tissue minus that of the second}
#'     \item{adj.p.value}{Benjamini and Hochberg corrected q-values}
#' }
#'
#' @examples
#' enrichment_data
"enrichment_data"

#' Diet of mice for gene expression data
#'
#' \code{mouse_diet} contains the mouse ID and diet information for the mice
#'     used in this study.
#'
#' \code{moexpr_data} contains the gene expression of each mouse (one mouse
#'     per column)
#'
#' This data was obtained from the Gene Expression Omnibus (GEO) under DataSet
#'     Record GDS6248.
#'
#' "Analysis of livers of C57BL/6J mice fed a high fat diet for up to 24 weeks.
#'     Significant body weight gain was observed after 4 weeks. Results provide
#'     insight into the effect of high fat diets on metabolism in the liver."
#'
#' @format
#' \code{moexpr_data} is a tibble (18,596 x 49)
#' \describe{
#'     \item{gene}{gene name for the row}
#'     \item{GSM994790 ... GSM994837}{expression data for each mouse}
#' }
#' \code{mouse_diet} is a tibble (48 x 2)
#' \describe{
#'     \item{mouse}{the ID of the mouse}
#'     \item{diet}{the level of fat in the diet}
#' }
#'
#' @examples
#' mouse_diet
#' moexpr_data
#'
#' @source \url{https://www.ncbi.nlm.nih.gov/sites/GDSbrowser/}
"moexpr_data"

#' @rdname moexpr_data
"mouse_diet"
