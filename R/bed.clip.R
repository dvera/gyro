bed.clip <-
function( datafiles, genomefile, threads=getOption("threads",1L) ){
	library(tools)
	ext<-file_ext(datafiles)
	outnames<-paste0(basename(removeext(datafiles)),"_clip.",ext)
	cmdString <- paste( "bedClip", datafiles, genomefile , outnames )
	res <- rage.run ( cmdString , threads )
	return(outnames)
}
