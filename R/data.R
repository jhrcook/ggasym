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
