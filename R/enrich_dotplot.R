#' @title My dotplot
#' @description dotplot for enrichment result
#'
#' @param result_object compareClusterResult object
#' @param showcategory A number or a list of terms. If it is a number, the first n terms will be displayed. If it is a list of terms, the selected terms will be displayed.
#' @param title_name figure title
#' @importFrom enrichplot dotplot
#' @importFrom ggplot2 theme_bw
#'
#' @returns plot
#' @export
#'
#' @examples
#' \dontrun{
#' results <- enrichRice(gene_ID = CO2_data,
#'                      geneset = "Top200",
#'                      ont = "ALL",
#'                      padjustMethod = "BH",
#'                      pvalueCut = 0.05,
#'                      qvalueCut = 0.2,
#'                      minSize = 5,
#'                      maxSize = 1000)
#'
#' result_shift <- transform_result_to_dotplot(result_data = results)
#'
#' enrich_dotplot(result_object=result_shift,
#'            showcategory = 20,
#'            title_name = "Custom Gene Set Enrichment Results")
#' }
enrich_dotplot <- function(result_object,
                       showcategory = 20,
                       title_name = "Custom Gene Set Enrichment Results"){
  p1 <- enrichplot::dotplot(result_object,showCategory=showcategory,title=title_name)
  p1 <- p1 + ggplot2::theme_bw()
  return(p1)
}
