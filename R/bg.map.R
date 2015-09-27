#' Perform calculations on bedGraph scores over the inverals in a bed file.
#'
#' \code{bg.map} is a wrapper for \code{bedtools map} that "allows one to map overlapping features in a B file onto features in an A file and apply statistics and/or summary operations on those features."
#'
#' @param bgFiless A character vector of paths to bedGraph files.
#' @param windows A string specifying the path to the bed file within which to perform calculations on bedGraph files.
#' @param operation to perform on bedGraph intervals overlapping windows. Can be one of: sum,count,count_distinct,min,max,absmin,absmax,mean,median,antimode,collapse,distinct,concat. Default is mean. See bedtools docs for more info.
#' @param outnames A character vector of output file names. If NULL, output file names are automatically generated using the syntax bedGraphFileName_operation_windowFileName.bg
#' @param windowsize Positive integer indicating size of windows. This is used to resize overlapping windows to comply with bedGraph format restriction against overlapping intervals. This only needs to be specified if windows are overlapping and a bedGraph-compliant file is desired.
#' @param stepsize Positive integer indicating size of the steps. This is used to resize overlapping windows to comply with bedGraph format restriction against overlapping intervals. This only needs to be specified if windows are overlapping and a bedGraph-compliant file is desired.
#' @param filler Value to assign intervals with no overlapping bedGraph intervals.
#' @param printzero Boolean indicating of regions with a value of 0 should be printed. Default is TRUE.
#' @param threads A positive integer specifying how many samples to process simultaneously.


bg.map <-
function( bgFiles, windows , operation="mean" , outnames = NULL , windowsize=25, stepsize=windowsize, filler=0 , printzero=TRUE , threads=getOption("threads",1L) ){

	if(is.null(outnames)){ outnames<-paste0(basename(removeext(bgFiles)),"_",operation,"_",basename(removeext(windows)),".bg") }
	if(length(outnames) != length(bgFiles)){stop("length of outnames must match length of bgFiles")}
	if(length(windows)!=1){stop("must have only 1 window file")}
	overlapsize<-(windowsize-stepsize)/2

	lsub=floor(overlapsize)

	cmdString <- paste(
		"bedtools map -c 4 -o",operation,
		"-null",filler,
		"-a",windows,
		"-b",bgFiles,
		if(printzero==F){"| awk '$4!=0'"},
		if( stepsize<windowsize ){ paste( "| awk '{print $1,$2","+",lsub,",$2+",lsub,"+",stepsize,",$4}' OFS='\t'" ) } ,
		">" , outnames )

	res <- rage.run(cmdString,threads)
	return(outnames)
}
