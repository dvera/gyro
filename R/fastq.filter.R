fastxFilter <-
function( fastq, qscore=20, percent=90, paired=FALSE){
	outname<-paste(basename(removeext(fastq)),"_filtered-q",qscore,"p",percent,".fastq",sep="")
	system(paste("fastq_quality_filter -Q33 -v -q",qscore,"-p",percent,"-i",fastq,"-o",outname))
	return(outname)
}
