#' Calculate coverage of multiple bams files over intervals in a single bed file.
#'
#' \code{bedtools.multicov} is a wrapper for bedtools multicov.
#'
#' @param bamFiles A character vector of paths to bam files.
#' @param bedfile A string specifying the path to the bed file within which to calculate coverage of bams.
#' @param outname A character vector of length one specifying the concatenated file name.
#' @param split Boolean.  If TRUE, “split” BAMs or BED12 entries will be treated as distinct BED intervals.
#' @param samestrand Boolean. If TRUE, same strandedness is required for a read to be counted. That is, only report hits in B that overlap A on the same strand. If FALSE, overlaps are reported without respect to strand. Conflicts with diffstrand.
#' @param diffstrand Boolean. If TRUE, different strandedness is required for a read to be counted. That is, only report hits in B that overlap A on the opposite strand. If FALSE, overlaps are reported without respect to strand. Conflicts with samestrand.
#' @param q A positive integer specifying the minimum quality of a read for it to be counted.
#' @param duplicates Boolean. If TRUE, include duplicate reads. Default (FALSE) counts non-duplicates only.
#' @param onlyPaired. Boolean. If TRUE, only properly-paired reads will be counted. Default counts all alignments with MAPQ > q, regardless of the BAM FLAG field.


bedtoolsMulticov <-
function( bamFiles , bedfile , outname , split=FALSE , samestrand=FALSE , diffstrand=FALSE , q=10 , duplicates=FALSE , onlyPaired=FALSE ){

	#check parameters
	if(length(bedfile) > 1){stop("bedtoolsMulticov can only take 1 bedGraph file")}
	if(samestrand & diffstrand){stop("samestrand and diffstrand can't both be TRUE")}

	#calculate coverage over windowfile
	cmdString <- (paste(
		"bedtools multicov",
		"-bams",paste(bamFiles,collapse=" "),
		"-bed",bedfile,
		if(samestrand){"-s"},
		if(diffstrand){"-S"},
		if(split){"-split"},
		if(onlyPaired){"-p"},
		if(duplicates){"-D"},
		"-q",q,
		">",
		outname
	))

	res<-cmdRun(cmdString)
	return(outname)
}
