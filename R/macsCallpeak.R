#' Perform peak-calling with MACS2
#'
#' \code{macs} is a wrapper for \code{macs2 callpeak}.
#'
#' @param treatmentFiles A character vector of paths to treatment read files. treatmentFiles are treated as separate samples and are not used in combination when running macs2.
#' @param controlFiles A (optional) character vector of paths to control read files that correspond to treatmentFiles.
#' @param genomeSize A macs2-compatible string (hs,mm,ce,dm) or integer (in bp) specifying the genome size.
#' @param inputFormat String specifying the format of tag file, can be "ELAND", "BED", "ELANDMULTI", "ELANDEXPORT", "ELANDMULTIPET", "SAM", "BAM", "BOWTIE" or "BAMPE". Default is "AUTO" which will allow MACS to decide the format automatically.
#' @param noLambda A boolean indicating if local input read densities should be skipped when computing lambda.
#' @param sampleName Character vector of name strings corresponding to treatmentFiles. MACS will use this string NAME to create output files like 'NAME_peaks.xls', 'NAME_negative_peaks.xls', 'NAME_peaks.bed' , 'NAME_summits.bed', 'NAME_model.r' and so on. If NULL will reuse names of treatmentFiles.
#' @param callSummits Boolean. If TRUE, MACS will reanalyze the shape of the signal profile to deconvolve subpeaks within each peak called from general procedure. The output subpeaks of a big peak region will have the same peak boundaries, and different scores and peak summit positions.
#' @param qvalue The qvalue (minimum FDR) cutoff to call significant regions. Q-values are calculated from p-values using Benjamini-Hochberg procedure.
#' @param verbosity Integer. If you don't want to see any message during the running of MACS, set it to 0. But the CRITICAL messages will never be hidden. If you want to see rich information like how many peaks are called for every chromosome, you can set it to 3 or larger than 3.'
#' @param threads A positive integer specifying how many samples to process simultaneously.


macsCallpeak <- function ( treatmentFiles , controlFiles=NULL , genomeSize = "hs" , inputFormat = "AUTO" , noLambda=TRUE , sampleNames = NULL , callSummits = TRUE , qvalue = 0.01 , verbosity = 3 , threads=getOption("threads",1L) ){

	# create output name string
	if( is.null( sampleNames ) ){ sampleNames = basename( removeext( treatmentFiles ) ) }
	sampleNames <- paste0( sampleNames , "_" , tolower(inputFormat) , "_fdr" , 100*qvalue )

	cmdString <- paste(
			"macs2 callpeak -t",
			treatmentFiles,
			if( is.null( controlFiles ) == FALSE ) { paste( "-c", controlFiles ) },
			if(noLambda){"--nolambda"},
			"-n",sampleNames,
			"-g",genomeSize,
			if(callSummits){ "--call-summits" },
			"-q",qvalue,
			"-f",inputFormat,
			"--verbose",verbosity,
			"-B"
		)

	res<-cmdRun(cmdString,threads)

	peaknames <- paste0( sampleNames, "_peaks.narrowPeak" )
	if(threads <= 2){
		np <- unlist(lapply(peaknames, np.2.bed  ))
	} else{
		np <- unlist(mclapply(peaknames, np.2.bed , mc.cores=threads))
	}


	return(np)
}
