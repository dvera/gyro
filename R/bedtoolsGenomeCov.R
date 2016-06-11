#' Calculate coverage of a set of bed files at each base of the genome.
#'
#' \code{bedtoolsGenomeCov} is a wrapper for \code{bedtools genomecov -bg}.
#'
#' @param bedFiles A character vector of paths to bed files.
#' @param windowfile A string specifying the path to the bed file within which to calculate coverage of beds.
#' @param windowsize Positive integer indicating size of windows used to resize overlapping windows to comply with bedGraph format restriction against overlapping intervals. This only needs to be specified if windows are overlapping and a bedGraph-compliant file is desired.
#' @param stepsize Positive integer indicating size of the steps used to resize overlapping windows to comply with bedGraph format restriction against overlapping intervals. This only needs to be specified if windows are overlapping and a bedGraph-compliant file is desired.
#' @param scalar A number to multiply the read counts by. If "rpm", the read counts will be multiplied by a 1000000/(number of reads) to yield reads-per-million (rpm).
#' @param threads A positive integer specifying how many samples to process simultaneously.

bedtoolsGenomeCov <-
function( bedFiles, chromsizes , covmode="-bg" , scalar="rpm", bam=FALSE, blocks=FALSE , bamInsertCov=FALSE , threads=getOption("threads",1L) ){

	if(missing(chromsizes)){
		chromsizes<-getOption("chromsizes",NULL)
		if(is.null(chromsizes)){stop("must define file contain chromosome sizes")}
	}
	if(!file.exists(chromsizes)){
		stop("chromsizes file does not exist")
	}

	options(scipen=9999)
	outnames<-paste0(basename(removeext(bedFiles)),".bg")

	if(scalar=="rpm" & bam == FALSE){scalar<-1000000/filelines(bedFiles)}
	if(scalar=="rpm" & bam == TRUE){scalar<-1000000/samtoolsView(bedFiles,count=T)}

	if(covmode=="-d"){pipes<-"| awk '{print $1,$2,$2+1,$3}' OFS='\t'"}
	if(covmode=="-dz"){pipes<-"| awk '{print $1,$2+1,$2+2,$3}' OFS='\t'"}
	if(covmode=="-bg"){pipes<-""}


	if(bam==TRUE){inputarg="-ibam"} else{inputarg="-i"}

	cmdString<-paste(
		"bedtools genomecov",
		covmode,
		"-g",chromsizes,
		"-scale",scalar,
		if(blocks){"-split"},
		if(bam & bamInsertSize){"-pc"},
		inputarg,
		bedFiles,
		pipes,
		">",outnames
	)
	cmdRun(cmdString,threads)
	return(outnames)
}
