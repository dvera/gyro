kentBedClip <-
function( datafiles, chromsizes, threads=getOption("threads",1L) ){

	if(missing(chromsizes)){
		chromsizes<-getOption("chromsizes",NULL)
		if(is.null(chromsizes)){stop("must define file contain chromosome sizes")}
	}
	if(!file.exists(chromsizes)){
		stop("chromsizes file does not exist")
	}


	ext<-file_ext(datafiles)
	outnames<-paste0(basename(removeext(datafiles)),"_clip.",ext)
	cmdString <- paste( "bedClip", datafiles, chromsizes , outnames )
	res <- cmdRun ( cmdString , threads )
	return(outnames)
}
