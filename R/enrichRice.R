#' @title Custom Gene Set Enrichment Analysis
#'
#' @description 用于将提供的基因集与Top200,Top500或者Top1000的Gene set富集，并且返回富集结果.
#'
#'
#' @param gene_ID a vector of gene id
#' @param geneset one of "Top200", "Top500", "Top1000"
#' @param ont One of "MG", "TG", and "DG" subontologies, or "ALL" for all three.
#' @param padjustMethod one of "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
#' @param pvalueCut adjusted pvalue cutoff on enrichment tests to report
#' @param qvalueCut qvalueCutoff qvalue cutoff on enrichment tests to report as significant.
#' @param minSize minimal size of genes annotated for testing
#' @param maxSize maximal size of genes annotated for testing
#'
#' @returns A \code{enrichResult} instance
#' @export
#' @importFrom clusterProfiler enricher
#' @examples
#' \dontrun{
#' results<-enrichRice(gene_ID = CO2_data,
#'           geneset = "Top200",
#'           ont = "ALL",
#'           padjustMethod = "BH",
#'           pvalueCut = 0.05,
#'           qvalueCut = 0.2,
#'           minSize = 5,
#'           maxSize = 1000)
#'}
enrichRice <- function(gene_ID,
                      geneset="Top200",
                      ont="ALL",
                      padjustMethod = "BH",
                      pvalueCut = 0.05,
                      qvalueCut = 0.2,
                      minSize = 10,
                      maxSize = 500){

enrich_data <- .get_data()

if(geneset=="Top200") {
  if(ont == "ALL"){
  term2gene <- enrich_data$term2gene_top200
  term2name <- enrich_data$term2name_top200
  universe_genes <- enrich_data$universe_genes_top200
  }
  if(ont== "MG"){
  term2gene <- enrich_data$term2gene_top200_MG
  term2name <- enrich_data$term2name_top200_MG
  universe_genes <- enrich_data$universe_genes_top200_MG
  }
  if(ont == "TG"){
    term2gene <- enrich_data$term2gene_top200_TG
    term2name <- enrich_data$term2name_top200_TG
    universe_genes <- enrich_data$universe_genes_top200_TG
  }
  if(ont == "DG"){
    term2gene <- enrich_data$term2gene_top200_DG
    term2name <- enrich_data$term2name_top200_DG
    universe_genes <- enrich_data$universe_genes_top200_DG
  }
} else if(geneset=="Top500") {
  if(ont == "ALL"){
    term2gene <- enrich_data$term2gene_top500
    term2name <- enrich_data$term2name_top500
    universe_genes <- enrich_data$universe_genes_top500
  }
  if(ont== "MG"){
    term2gene <- enrich_data$term2gene_top500_MG
    term2name <- enrich_data$term2name_top500_MG
    universe_genes <- enrich_data$universe_genes_top500_MG
  }
  if(ont == "TG"){
    term2gene <- enrich_data$term2gene_top500_TG
    term2name <- enrich_data$term2name_top500_TG
    universe_genes <- enrich_data$universe_genes_top500_TG
  }
  if(ont == "DG"){
    term2gene <- enrich_data$term2gene_top500_DG
    term2name <- enrich_data$term2name_top500_DG
    universe_genes <- enrich_data$universe_genes_top500_DG
  }
} else if(geneset=="Top1000") {
  if(ont == "ALL"){
    term2gene <- enrich_data$term2gene_top1000
    term2name <- enrich_data$term2name_top1000
    universe_genes <- enrich_data$universe_genes_top1000
  }
  if(ont== "MG"){
    term2gene <- enrich_data$term2gene_top1000_MG
    term2name <- enrich_data$term2name_top1000_MG
    universe_genes <- enrich_data$universe_genes_top1000_MG
  }
  if(ont == "TG"){
    term2gene <- enrich_data$term2gene_top1000_TG
    term2name <- enrich_data$term2name_top1000_TG
    universe_genes <- enrich_data$universe_genes_top1000_TG
  }
  if(ont == "DG"){
    term2gene <- enrich_data$term2gene_top1000_DG
    term2name <- enrich_data$term2name_top1000_DG
    universe_genes <- enrich_data$universe_genes_top1000_DG
  }
} else{
  return("错误，请检查geneset输入是否为Top200，Top500，Top1000中的之一")
}
if (!ont %in%  c("ALL","MG","TG","DG")){
  return("错误，请检查ont输入是否为ALL,MG,TG,DG中的之一")
}

#将输入的差异基因统一转为大写
significant_genes <- toupper(gene_ID)
#检查差异基因列与背景基因的交集
submit_count <- length(significant_genes)
significant_genes_final <- intersect(significant_genes,universe_genes)
final_intersect_count <- length(significant_genes_final)

message("################开始进行富集分析###################\n")
message("从提交的基因集中提取到了",submit_count,"个基因.\n")
message("其中",final_intersect_count,"个基因存在于背景基因集中，将用于富集分析.\n")
message("背景基因共",length(universe_genes),"个.\n")

#执行富集分析
if (final_intersect_count > 0){
  enrich_results <- clusterProfiler::enricher(gene = significant_genes,
                                              pvalueCutoff = pvalueCut,
                                              pAdjustMethod = padjustMethod,
                                              universe = universe_genes,
                                              minGSSize = minSize,
                                              maxGSSize = maxSize,
                                              qvalueCutoff = qvalueCut,
                                              TERM2GENE = term2gene,
                                              TERM2NAME = term2name)
}else{
  enrich_results <- NULL
  message("\n错误，没有有效的差异基因用于富集分析。请再检查基因ID是否匹配。\n")
}
  return(enrich_results)

}
