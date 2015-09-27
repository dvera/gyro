bed.closest2 <-
function( bed1, bed2, strand=TRUE ){
	
	
	#ADD:
	# check if files exist
	# check if bed2 has a name column
	library(tools)
	
	ext<-file_ext(bed1)
	bedname<-basename(removeext(bed1))
	refname<-basename(removeext(bed2))
	outname<-paste0(bedname,"_closest-",refname,".",ext)
	
	if(strand==TRUE){
		system(paste("cut -f 1,2,3",bed1,"| bedtools closest -t first -a stdin -b",bed2,"| cut -f 1,2,3,7,8,9 >",outname))
	} else{
		system(paste("cut -f 1,2,3",bed1,"| bedtools closest -t first -a stdin -b",bed2,"| cut -f 1,2,3,7 >",outname))
	}
	
	return(outname)
}
