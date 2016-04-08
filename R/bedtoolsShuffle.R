bedtoolsShuffle <-
function( bedFiles, chromsizes , threads=getOption("threads",1L), include=NULL, exclude=NULL, samechrom=FALSE, chromfirst=FALSE, outnames=NULL, postsort=TRUE ){

	if(missing(chromsizes)){
		chromsizes<-getOption("chromsizes",NULL)
		if(is.null(chromsizes)){stop("must define file contain chromosome sizes")}
	}
	if(!file.exists(chromsizes)){
		stop("chromsizes file does not exist")
	}

	bednames<-basename(removeext(bedFiles))
	ext<-file_ext(bedFiles)
	#cat(bedname,": shuffling features\n")
	extraargs<-""
	if(samechrom==TRUE){extraargs<-"-chrom"}
	if(chromfirst==TRUE){extraargs<-paste(extraargs,"-chromFirst")}
	if(is.null(include) == FALSE){extraargs<-paste(extraargs,"-incl",include)}
	if(is.null(exclude) == FALSE){extraargs<-paste(extraargs,"-excl",exclude)}
	if(is.null(outnames)){outnames<-paste(bednames,"_shuffled.",ext,sep="")}
	#print(paste("bedtools shuffle",extraargs,"-i",bedFiles,"-g",chromsizes,">",outname))

	cmdString <- paste(
		"bedtools shuffle",
		extraargs,
		"-i",bedFiles,
		"-g",chromsizes,
		if(postsort){"| sort -k1,1 -k2,2n -k3,3n"},
		">",outnames
	)
	res <- cmdRun(cmdString,threads)
	return(outnames)
}
