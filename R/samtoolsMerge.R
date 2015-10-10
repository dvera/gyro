#' Merge sorted bam files
#'
#' \code{samtools.merge} is a simple wrapper for samtools merge for merging sorted bam files.
#'
#' @param bamFiles A character vector of paths to bam files.
#' @param outname A vector of length one specifying the concatenated file name.
#' @param threads An integer specifying the number of BAM compression threads to use



samtoolsMerge <- function( bamFiles , outname , threads=getOption("threads",1L)  ){

  cmdString <- paste(
    "samtools merge -f",
    "-@" , threads ,
    outname ,
    paste(bamFiles, collapse=" ")
  )

  res <- rage.run(cmdString)

  return(outname)
}
