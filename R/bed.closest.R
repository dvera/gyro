bed.closest <-
function( bed1, bed2, strand=TRUE ){


	#ADD:
	# check if files exist
	# check if bed2 has a name column
	library(tools)
	ext<-file_ext(bed1)
	bedname<-basename(removeext(bed1))
	refname<-basename(removeext(bed2))
	curbed<-read.tsv(bed1)

	curbed.call <- pipe(paste("cut -f 1,2,3",bed1,"| bedtools closest -t first -a stdin -b",bed2,"| cut -f 7"))
	curbed$V4<-readLines(curbed.call)
	close(curbed.call)

	if(strand){
		curbed$V5<-1
		curbed$V6<-readLines(pipe(paste("cut -f 1,2,3",bed1,"| bedtools closest -t first -a stdin -b",bed2,"| cut -f 9")))
	}


	outname<-paste(bedname,"_closest-",refname,".",ext,sep="")
	write.tsv(curbed,file=outname)
	return(outname)
}
