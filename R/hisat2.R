hisat2 <-
function( read1files, index, read2files=NULL, outnames=NULL, strandedness=NULL, threads=getOption("threads",1L) ){

	library(parallel)
	if(threads=="max"){threads<-detectCores()-1}
	numfiles=length(read1files)


	if(!is.null(read2files)){
		paired<-TRUE
		if(numfiles!=length(read2files)){stop("must have the same number of mates")}
	} else{
		paired<-FALSE
	}

	if(is.null(outnames)){
		outnames <- paste0(basename(removeext(read1files)),".sam")
		lognames <- paste0(basename(removeext(read1files)),".sam.log")
	} else{
		if(numfiles != length(outnames)){stop("outnames and number of input files must be same length")}
	}


	cmdString <- paste(
		"hisat2",
		"-p",threads,
		"-x",index,
		if(!is.null(strandedness)){paste("--rna-strandness",strandedness)},
		if(paired){
			paste("-1",read1files,"-2",read2files)
		} else{
			paste("-U",read1files)
		},
		"-S",outnames,
		"2>",lognames
	)

	cmdRun(cmdString, threads=1)

	return(outnames)
}
