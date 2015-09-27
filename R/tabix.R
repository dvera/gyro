tabix <- function( filename , format=NULL ){
	library(tools)
	format <- file_ext(basename(removeext(filename)))
	print(format)
	outname <- paste0(filename,".tbi")
	if(!format %in% c("vcf","bed","sam","vcf","psltbl")){
		stop("unable to determine format, specify format argument")
	} else{
		system(paste("tabix -p",format,filename))
		return(outname)
	}


}
