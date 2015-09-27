#' Sort bams by position or name
#'
#' \code{bam.sort} sorts bam files either by position or read name using \code{samtools sort}.
#'
#' @param bamFiles A character vector of paths to bam files.
#' @param outputFormat String specifying output format: ('sam'/'bam'/'cram')
#' @param sortThreads A positive integer specifying the number of sorting and compression threads
#' @param threads A positive integer specifying how many bams to process simultaneously.
#' @param memory String specifying maximum memory per thread; suffix K/M/G recognized.
#' @param sortByName Boolean. If TRUE, reads are sorted by name. If FALSE, reads are sorted by chromosome/position

bam.sort <-
function( bamFiles, outputFormat="bam" , sortThreads=1 , threads=getOption("threads",1L), memory="768M", sortByName=FALSE ){

	if( !(outputFormat %in% c("sam","bam","cram") ) ){ stop("output format must be \"sam\" \"bam\" or \"cram\"")}

	if(sortByName){suffix="_nsort"} else{ suffix="_psort" }
	outnames <- paste0( basename(removeext(bamFiles)) , suffix , "." , outputFormat )

	# generate command strings
	cmdString <- paste(
		"samtools sort",
		if( !is.na(memory) ){ paste( "-m" , memory ) },
		if( !is.na(threads) ){ paste( "-@" , sortThreads ) },
		if(sortByName){"-n"},
		"-T",outnames,
		"-O",outputFormat,
		bamFiles,
		">",
		outnames
	)

	# print and execute command string
	res<-rage.run(cmdString,threads)

	return(outnames)

}
