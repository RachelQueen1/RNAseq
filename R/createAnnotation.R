#' Create annotation dataframe
#'
#' @param geneNames vector to annotate
#' @param species dataset to use for the annotation
#'
#' @return annotation dataframe
#' @export
#'
#' @examples
#' @importFrom biomaRt useMart getBM
annotate = function(geneNames, species = 'hsapiens_gene_ensembl'){
    mart <- useMart(biomart = 'ensembl', dataset = species)

    annotation = getBM(attributes = c("ensembl_gene_id", "external_gene_name"),
                   filters = "ensembl_gene_id",
                   values = geneNames,
                   mart = mart)

#annotation = annotation[geneNames,]
return(annotation)
}
