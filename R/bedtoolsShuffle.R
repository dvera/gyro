bedtoolsShuffle <-
function( bedfile, genomefile , include=NULL, exclude=NULL, samechrom=FALSE, chromfirst=FALSE, outname=NULL ){
	library(tools)
	bedname<-basename(removeext(bedfile))
	ext<-file_ext(bedfile)
	#cat(bedname,": shuffling features\n")
	extraargs<-""
	if(samechrom==TRUE){extraargs<-"-chrom"}
	if(chromfirst==TRUE){extraargs<-paste(extraargs,"-chromFirst")}
	if(is.null(include) == FALSE){extraargs<-paste(extraargs,"-incl",include)}
	if(is.null(exclude) == FALSE){extraargs<-paste(extraargs,"-excl",exclude)}
	if(is.null(outname)){outname<-paste(bedname,"_shuffled.",ext,sep="")}
	#print(paste("bedtools shuffle",extraargs,"-i",bedfile,"-g",genomefile,">",outname))

	system(paste("bedtools shuffle",extraargs,"-i",bedfile,"-g",genomefile,">",outname))
	return(outname)
}
