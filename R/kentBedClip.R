kentBedClip <-
function( datafiles, genomefile, threads=getOption("threads",1L) ){

	ext<-file_ext(datafiles)
	outnames<-paste0(basename(removeext(datafiles)),"_clip.",ext)
	cmdString <- paste( "bedClip", datafiles, genomefile , outnames )
	res <- cmdRun ( cmdString , threads )
	return(outnames)
}
