#' Concatenate bam files
#'
#' \code{bam.cat} is a simple wrapper for samtools cat for combining bam files.
#'
#' @param bamFiles A character vector of paths to bam files.
#' @param outname A vector of length one specifying the concatenated file name.



samtoolsCat <- function( bamFiles , outname ){

	cmdString <- paste("samtools cat -o" , outname , paste(bamFiles, collapse=" ") )

	res <- rage.run(cmdString)

	return(outname)

}
