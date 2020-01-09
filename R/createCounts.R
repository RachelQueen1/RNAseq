#' Title
#'
#' @param countDir path to count directory
#'
#' @return counts data frame
#' @export
#'
#' @examples


readCounts <- function(countDir){
    counts <- read.csv(file.path(countDir, "counts.csv"), sep = "\t", stringsAsFactors = FALSE, header = FALSE)
    rownames(counts) <- counts[, 1]
    counts <- counts[, -1]

    ### add colnames
    namesCounts <- read.csv(file.path(countDir, "colnames.txt") , sep = " ", header = FALSE, stringsAsFactors = FALSE) %>%
        .[1,] %>%
        as.character %>%
        gsub("counts__", "", .) %>%
        gsub(".txt", "", .)
    colnames(counts) <- namesCounts


    ### collapse duplicated genes
    counts$geneNames <- gsub("\\..*", "", rownames(counts))
    counts <- counts %>% group_by(geneNames) %>% summarise_all(sum) %>% data.frame()
    rownames(counts) <- counts[,1]
    counts<- counts[, -1]
    counts <- counts[grepl("ENS", rownames(counts)), ]

    return(counts)
}
