bedtoolsSlop <-
function( bedFiles, genomefile, expandleft=0, expandright=0, strand = FALSE, threads=getOption("threads",1L) ){

	if(missing(genomefile)){
		genomefile=getOption("genomefile")
	}
	if(is.null(genomefile)){
		stop("genomefile must exist")
	}

	bedname<-basename(removeext(bedFiles))

	ext<-file_ext(bedFiles)

	outnames<-paste0(basename(removeext(bedFiles)),"_sl",expandleft,"_sr",expandright,".",ext)

	cmdString <- paste(

		"bedtools slop",
		if(strand){"-s"} ,
		"-i",bedFiles,
		"-g",genomefile,
		"-l",expandleft,
		"-r",expandright,
		">",outnames
	)

	res <- cmdRun(cmdString,threads)

	return(outnames)

}
