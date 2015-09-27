#' Count reads in bam files
#'
#' \code{bam.count} is a simple wrapper for \code{samtools view -c} for counting the number of reads in a bam file.
#'
#' @param bamFiles A character vector of paths to bam files.
#' @param q An integer >= 0 specifying the minimum quality of a read for it to be counted.
#' @param threads A positive integer specifying how many bams to process simultaneously.


bam.count <-
function ( bamfiles , q = 0 , threads=getOption("threads",1L) ){

	cmdString <- paste( "samtools view -c -q" , q , bamfiles )

	res <- as.numeric ( rage.run( cmdString, threads, intern=TRUE ) )

	return( res )
}
