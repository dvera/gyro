#' Calculate coverage of a set of bed files over intervals in a single bed file.
#'
#' \code{bedtoolsCoverage} is a wrapper for \code{bedtools intersect -c}.
#'
#' @param bedFiles A character vector of paths to bed files.
#' @param windowfile A string specifying the path to the bed file within which to calculate coverage of beds.
#' @param windowsize Positive integer indicating size of windows used to resize overlapping windows to comply with bedGraph format restriction against overlapping intervals. This only needs to be specified if windows are overlapping and a bedGraph-compliant file is desired.
#' @param stepsize Positive integer indicating size of the steps used to resize overlapping windows to comply with bedGraph format restriction against overlapping intervals. This only needs to be specified if windows are overlapping and a bedGraph-compliant file is desired.
#' @param scalar A number to multiply the read counts by. If "rpm", the read counts will be multiplied by a 1000000/(number of reads) to yield reads-per-million (rpm).
#' @param threads A positive integer specifying how many samples to process simultaneously.


bedtoolsCoverage <-
function( bedFiles , windowfile , windowsize=25 , stepsize=windowsize , scalar="rpm" , threads=getOption("threads",1L) ){

	#check if only 1 waindowfile
	if(length(windowfile) > 1){stop("bedtoolsCoverage can only take 1 window file")}

	#get base name
	bedname<-basename(removeext(bedFiles))
	winname<-basename(removeext(windowfile))
	outname<-paste0(basename(removeext(bedFiles)),"_",winname,".bg")
	overlapsize<-(windowsize-stepsize)/2
	lsub=floor(overlapsize)

	#calculate scaling factor
	if(scalar=="rpm"){
		scalar=1000000 / filelines( bedFiles, threads=threads )
	}

	#calculate coverage over windowfile
	cmdString <- (paste(
		"bedtools intersect -sorted -c",
		"-b",bedFiles,
		"-a",windowfile,
		"| awk '{print $1,$2",
		if(stepsize<windowsize){
			paste("+",lsub)
		},
		if(stepsize<windowsize){
			paste(",$2+",lsub,"+",stepsize)
		} else{",$3"},
		",$4*",scalar,
		"}' OFS='\\t' >",
		outname
	))

	res<-cmdRun(cmdString, threads)

	return(outname)
}
