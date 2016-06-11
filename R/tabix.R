tabix <- function( filenames , formats , threads=getOption("threads",1L) ){

	if(missing(formats)){
		formats <- file_ext(removeext(filenames))
	}

	outnames <- paste0(filenames,".tbi")

	if(any(is.na(match(formats,c("vcf","bed","sam","vcf","psltbl"))))) {
		stop("unable to determine format, specify format argument")
	}

	cmdString <- paste("tabix -p",formats,filenames)

	res <- cmdRun(cmdString,threads=threads)

	return(outnames)


}
