#' Index bam files
#'
#' \code{bam.index} is a simple wrapper for \code{samtools index} for indexing bam files.
#'
#' @param bamFiles A character vector of paths to bam files.
#' @param threads A positive integer specifying how many bams to process simultaneously.

bam.index <-
function( bamFiles , threads=getOption("threads",1L) ){

	cmdString <- paste("samtools index",bamFiles)

	res <- rage.run(cmdString,threads)
	
}
