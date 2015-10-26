kentLiftOver <- function( beds, chainfile, suffix , threads=getOption("threads",1L) ){

	numbeds <- length(beds)

	exts <- file_ext( beds )
	bednames <- basename(removeext(beds))
	outnames <- paste0( bednames, suffix, ".", exts)
	unmapped <- paste0( bednames, "_unmapped", ".", exts)

	cmdString <- paste( "liftOver", beds, chainfile, outnames, unmapped )

	cmdRun( cmdString , threads )

	return(outnames)

}
