#' Merge overlapping interavls within an individual bed file.
#'
#' \code{bed.merge} is a wrapper for \code{bedtools merge} that merges overlapping intervals within individual bed files but NOT across bed files.
#'
#' @param bedFiles A character vector of paths to bed files.
#' @param flank "Maximum distance between features allowed for features to be merged. Default is 0. That is, overlapping and/or book-ended features are merged."
#' @param scorecol Positive integer specifying the column with values for performing the specified operation.
#' @param operation to perform on overlapping bed intervals. Can be one of: none,sum,mean,collapse,distinct,count,count_distinct. Default is none, meaning no operation is performed. See bedtools docs for more info.
#' @param threads A positive integer specifying how many samples to process simultaneously.

bed.merge <- function( bedFiles, flank=0, scorecol=5, operation="none", strand=FALSE, threads=getOption("threads",1L) ){

  ext<-file_ext(bedFiles)

  if(flank==0){suffix=""}
  else{suffix=flank}

  if(operation=="none"){moreargs=""} else{
  	suffix=paste0(suffix,"_",operation)
  	moreargs=paste("-c",scorecol,"-o",operation, collapse=" ")
  }

  outnames<-paste(basename(removeext(bedFiles)),"_merged",suffix,".",ext,sep="")

  cmdString <- paste("bedtools merge -d",flank,if(strand){"-s"},moreargs,"-i",bedFiles,">",outnames)
  res <- rage.run(cmdString,threads)
  return(outnames)
}
