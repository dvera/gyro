bgzip <- function( filenames, clobber=FALSE, threads=getOption("threads",1L) ){
	outnames <- paste0(basename(filenames),".gz" )
	cmdString <- paste("bgzip -c", if(clobber){"-f"}, filenames , ">", outnames)
	res <- cmdRun(cmdString, threads=threads)
	return(outnames)
}
