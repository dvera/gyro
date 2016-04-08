bedtoolsMakeWindows <- function( bedfiles, windowsize=25, stepsize=windowsize, mergewithin=NULL, outnames=NULL, genome=FALSE, threads=getOption("threads",1L) ){

	options(scipen=99999)

	# check input
	if(missing(bedfiles)){
		bedfiles<-getOption("chromsizes",NULL)
		genome=TRUE
		if(is.null(bedfiles)){stop("must define file")}
	} else{
		if(file_ext(bedfiles)=="sizes"){
			genome=TRUE
		}
		if(any(!file.exists(bedfiles))){
			stop("at least one input file does not exist")
		}
	}



	# define or check output file names
	if(is.null(outnames)){
		if(windowsize==stepsize){
			outnames<-paste0(basename(removeext(bedfiles)),"_w",windowsize,".bed")
		} else{
			outnames<-paste0(basename(removeext(bedfiles)),"_w",windowsize,"s",stepsize,".bed")
		}
	} else{
		if(length(outnames) != length(bedfiles)){
			stop("length of outnames should equal length of bedfiles")
		}
	}

	# merge bedfile if desired
	if(!is.null(mergewithin) & !genome){
		if(!is.numeric(mergewithin) | length(mergewithin)>1){
			stop("mergewithin must be an integer")
		}

		bedfiles<-bedtoolsMerge(bedfiles,flank=mergewithin)
	}

	if(windowsize > stepsize){
		extraargs<-"| awk 'x != $3; {x=$3}'"
	}
	else{
		extraargs<-""
	}


	if(genome){
		inputarg="-g"
	} else{
		inputarg="-b"
	}

	cmdString <- paste(
		"bedtools makewindows",
		"-w",windowsize,"-s",stepsize,
		inputarg,
		bedfiles,
		extraargs,
		">",outnames
	)

	res <- cmdRun(cmdString,threads=threads)

	return(outnames)
}
