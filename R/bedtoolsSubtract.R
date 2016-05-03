#' Remove regions from a bed file overlapping another bed file.
#'
#' \code{bedtoolsSubtract} is a wrapper for \code{bedtools subtract}.
#'
#' @param a A character vector of paths to bed files.
#' @param b A path to the bed file used to remove intervals in a.
#' @param extraargs A string passed as arguments to bedtools subtract
#' @param count If TRUE, only returns the number of intervals from the operation.
#' @param threads A positive integer specifying how many samples to process simultaneously.


bedtoolsSubtract <-
function( a, b, outname , extraargs="", header=TRUE, count=FALSE, threads=getOption("threads",1L) ){


	bed1name<-basename(removeext(a))
	bed2name<-basename(removeext(b))
	ext<-file_ext(a)


	if(missing(outname)){
    outname<-paste0(bed1name,"_minus_",bed2name,".",ext)
  } else{
    if(length(outname) != length(a)){stop("length of outnames must match length of bedfiles")}
  }




	if(count){
		cmdString<-paste("bedtools subtract",extraargs,if(header){"-header"},"-a",a,"-b",b,"| wc -l")
		res <- cmdRun(cmdString, threads, intern=TRUE)
		return(res)
	} else{
		cmdString<-paste("bedtools subtract",extraargs,if(header){"-header"},"-a",a,"-b",b,">",outname)
		res <- cmdRun(cmdString, threads)
		return(outname)
	}

}
