cuffdiff <-
function( bamFiles, gtf, outname, referenceFasta=NULL, fdr=0.05, sampleNames=NULL, dispersionMethod="pooled", librarytype="fr-unstranded", noLengthCorrection=FALSE, cores="max"){

	library(parallel)
	if(cores=="max"){cores<-detectCores()-1}
	numfiles=length(bamFiles)

	if(is.null(sampleNames)){
		sampleNames <- dirname(bamFiles)
		if(length(unique(sampleNames))!=numfiles){stop("cannot automatically specify sampleNames, you must manually define them")}
	} else{
		if(numfiles != length(sampleNames)){stop("outnames and number of input files must be same length")}
	}

	cmdString <- paste(
		"cuffdiff",
		"-p",cores,
		"-o",outname,
		"--FDR",fdr,
		"--library-type",librarytype,
		"--dispersion-method",dispersionMethod,
		"-L",paste(sampleNames,collapse=","),
		if(noLengthCorrection){"--no-length-correction"},
		if(!is.null(referenceFasta)){paste("-b",referenceFasta)},
		gtf,
		paste(bamFiles,collapse=" ")

	)

	print(cmdString)
	system(cmdString)
	system(paste0("echo \"",cmdString,"\" > ",outname,"/cmd.log"))

	return(paste0(outname,"/gene_exp.diff"))
}
