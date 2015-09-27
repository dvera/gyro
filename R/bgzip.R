bgzip <- function( filename, clobber=FALSE ){
	outname <- paste0(basename(filename),".gz" )
	system(paste("bgzip -c", if(clobber){"-f"}, filename , ">", outname))
	return(outname)
}
