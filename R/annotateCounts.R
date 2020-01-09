#' Title Annotate Counts
#'
#' @param counts dataframe of counts to be annotated
#' @param anno annotation dataframe
#'
#' @return annotated counts
#' @export
#'
#' @examples
annotateCounts <- function(counts, anno){

    counts$ensembl_gene_id <- rownames(counts)
    countsTmp <- counts %>% left_join(anno)

    countsTmp$external_gene_name[is.na(countsTmp$external_gene_name)] <- countsTmp$ensembl_gene_id[is.na(countsTmp$external_gene_name)]
    sum(duplicated(countsTmp$external_gene_name))

    ### remove ensembl_gene_id col
    removeCol <- which(colnames(countsTmp) == "ensembl_gene_id")
    countsTmp <- countsTmp[, -removeCol]

    ## collapse by gene symbols
    countsTmp <- countsTmp %>% group_by(external_gene_name) %>% summarise_all(sum) %>% data.frame()
    rownames(countsTmp) <- countsTmp[,1]
    counts<- countsTmp[, -1]
    return(counts)
}




