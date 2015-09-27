stringtie <- function( bamFiles, gff=NULL, outnames=NULL, referenceOnly=TRUE, ballgown=TRUE, threads="max"){
  library(parallel)
	if(threads=="max"){threads<-detectCores()-1}
	numfiles=length(bamFiles)
	
  if(is.null(outnames)){
    outnames <- basename(removeext(bamFiles))
  } else{
    if(bamFiles != length(outnames)){stop("outnames and number of input files must be same length")}
  }

  cmdString <- paste(
		"stringtie",
    bamFiles,
    "-p",threads,
    if(ballgown){paste0("-b ",outnames,"_ballgown")},
    "-o",paste0(outnames,".gff"),
    if(!is.null(gff)){paste("-G",gff)},
    if(!is.null(gff) & referenceOnly){"-e"}
	)

	for(i in 1:numfiles){
		print(cmdString[i])
		system(cmdString[i])
	}

	return(outnames)
}
