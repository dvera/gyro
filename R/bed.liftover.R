bed.liftover <- function( beds, chainfile, suffix , threads=getOption("threads",1L) ){

	library(parallel)
	library(gtools)
	numbeds <- length(beds)

	exts <- file_ext( beds )
	bednames <- basename(removeext(beds))
	outnames <- paste0( bednames, suffix, ".", exts)
	unmapped <- paste0( bednames, "_unmapped", ".", exts)

	cmdString <- paste( "liftOver", beds, chainfile, outnames, unmapped )

	rage.run( cmdString , threads )

	return(outnames)

}
