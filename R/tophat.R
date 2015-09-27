tophat <-
function( read1files, index, read2files=NULL, gtf=NULL, transcriptomeIndex=NULL, transcriptomeOnly=FALSE, outnames=NULL, librarytype="fr-unstranded", noCoverageSearch=TRUE, cores="max"){



	library(parallel)
	if(cores=="max"){cores<-detectCores()-1}
	numfiles=length(read1files)

	if(transcriptomeOnly & is.null(gtf) & is.null(transcriptomeIndex)){
		cat("warning: gtf or transcriptome not provided\n")
		transcriptomeOnly=FALSE
	}

	if(!is.null(read2files)){
		paired<-TRUE
		if(numfiles!=length(read2files)){stop("must have the same number of mates")}
	} else{
		paired<-FALSE
	}

	if(is.null(outnames)){
		outnames <- paste0(basename(removeext(read1files)),"_tophat")
	} else{
		if(numfiles != length(outnames)){stop("outnames and number of input files must be same length")}
	}


	cmdString <- paste(
		"tophat",
		"-p",cores,
		"-o",outnames,
		"--library-type",librarytype,
		if(!is.null(transcriptomeIndex)){paste("--transcriptome-index",transcriptomeIndex)},
		if(!is.null(gtf)){paste("-G",gtf)},
		if(transcriptomeOnly){"-T"},
		if(noCoverageSearch){"--no-coverage-search"},
		index,
		if(paired){
			paste(read1files,read2files)
		} else{
			read1files
		}

	)

	for(i in 1:numfiles){
		print(cmdString[i])
		system(cmdString[i])
	}

	return(outnames)
}
