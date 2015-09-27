#' Calculate coverage of a set of bed files over intervals in a single bed file.
#'
#' \code{bed.coverage} is a wrapper for \code{bedtools intersect -c}.
#'
#' @param bedFiles A character vector of paths to bed files.
#' @param windowfile A string specifying the path to the bed file within which to calculate coverage of beds.
#' @param windowsize Positive integer indicating size of windows used to resize overlapping windows to comply with bedGraph format restriction against overlapping intervals. This only needs to be specified if windows are overlapping and a bedGraph-compliant file is desired.
#' @param stepsize Positive integer indicating size of the steps used to resize overlapping windows to comply with bedGraph format restriction against overlapping intervals. This only needs to be specified if windows are overlapping and a bedGraph-compliant file is desired.
#' @param scalar A number to multiply the read counts by. If "rpm", the read counts will be multiplied by a 1000000/(number of reads) to yield reads-per-million (rpm).
#' @param threads A positive integer specifying how many samples to process simultaneously.


bed.intersect <-
function( a, b, extraargs="-u", header=TRUE, count=FALSE, threads=getOption("threads",1L) ){

	bed1name<-basename(removeext(a))
	bed2name<-basename(removeext(b))
	ext<-file_ext(a)

	outname<-paste0(bed1name,"_x_",bed2name,".",ext)

	if(count){
		cmdString<-paste("bedtools intersect",extraargs,if(header){"-header"},"-a",a,"-b",b,"| wc -l")
		res <- rage.run(cmdString, threads, intern=TRUE)
		return(res)
	} else{
		cmdString<-paste("bedtools intersect",extraargs,if(header){"-header"},"-a",a,"-b",b,">",outname)
		res <- rage.run(cmdString, threads)
		return(outname)
	}

}
