bedtoolsSlop <-
function( bedFiles, chromsizes, expandleft=0, expandright=0, strand = FALSE, threads=getOption("threads",1L) ){


	if(missing(chromsizes)){
		chromsizes<-getOption("chromsizes",NULL)
		if(is.null(chromsizes)){stop("must define file contain chromosome sizes")}
	}
	if(!file.exists(chromsizes)){
		stop("chromsizes file does not exist")
	}

	bedname<-basename(removeext(bedFiles))

	ext<-file_ext(bedFiles)

	outnames<-paste0(basename(removeext(bedFiles)),"_sl",expandleft,"_sr",expandright,".",ext)

	cmdString <- paste(

		"bedtools slop",
		if(strand){"-s"} ,
		"-i",bedFiles,
		"-g",chromsizes,
		"-l",expandleft,
		"-r",expandright,
		">",outnames
	)

	res <- cmdRun(cmdString,threads)

	return(outnames)

}
