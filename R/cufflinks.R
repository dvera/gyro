cufflinks <-
function( bamFiles, gtf=NULL, referenceFasta=NULL, sampleNames=NULL, librarytype="fr-unstranded", noLengthCorrection=FALSE, cores="max"){

	library(parallel)
	if(cores=="max"){cores<-detectCores()-1}
	numfiles=length(bamFiles)

	if(is.null(sampleNames)){
		sampleNames <- basename(dirname(bamFiles))
		outdirs=paste0(sampleNames,"_cufflinks")
		if(length(unique(sampleNames))!=numfiles){stop("cannot automatically specify sampleNames, you must manually define them")}
	} else{
		outdirs=paste0(sampleNames,"_cufflinks")
		if(numfiles != length(sampleNames)){stop("outnames and number of input files must be same length")}
	}

	cmdString <- paste(
		"cufflinks",
		"-p",cores,
		"-o",outdirs,
		"--library-type",librarytype,
		"-L",sampleNames,
		if(noLengthCorrection){"--no-length-correction"},
		if(!is.null(referenceFasta)){paste("-b",referenceFasta)},
		if(!is.null(gtf)){paste("-G",gtf)},
		bamFiles
	)

	for(i in 1:numfiles){
		print(cmdString[i])
		system(cmdString[i])
		#system(paste0("echo \"",cmdString,"\" > ",outname,"/cmd.log"))
	}
	#return(paste0(outname,"/gene_exp.diff"))
	return(outdirs)
}
