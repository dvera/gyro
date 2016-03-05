bedtoolsShuffle <-
function( bedFiles, genomefile , include=NULL, exclude=NULL, samechrom=FALSE, chromfirst=FALSE, outnames=NULL ){

	bednames<-basename(removeext(bedFiles))
	ext<-file_ext(bedFiles)
	#cat(bedname,": shuffling features\n")
	extraargs<-""
	if(samechrom==TRUE){extraargs<-"-chrom"}
	if(chromfirst==TRUE){extraargs<-paste(extraargs,"-chromFirst")}
	if(is.null(include) == FALSE){extraargs<-paste(extraargs,"-incl",include)}
	if(is.null(exclude) == FALSE){extraargs<-paste(extraargs,"-excl",exclude)}
	if(is.null(outnames)){outnames<-paste(bednames,"_shuffled.",ext,sep="")}
	#print(paste("bedtools shuffle",extraargs,"-i",bedFiles,"-g",genomefile,">",outname))

	cmdString <- paste(
		"bedtools shuffle",
		extraargs,
		"-i",bedFiles,
		"-g",genomefile,
		">",outnames
	)
	res <- cmdRun(cmdString,threads)
	return(outnames)
}
