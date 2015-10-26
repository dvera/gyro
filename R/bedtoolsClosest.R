bedtoolsClosest <-
function( bed1, bed2, strand=TRUE, threads=getOption("threads",1L) ){

	# todo:
	#   check if files exist
	#   check if bed2 has a name column

	nb1 <- length(bed1)
	nb2 <- length(bed2)

	ext<-file_ext(bed1)
	bednames<-basename(removeext(bed1))
	refnames<-basename(removeext(bed2))
	outnames<-paste0(bednames,"_closest-",refnames,".",ext)

	if( nb1 != nb2 ){
		if( nb1==1 ){
			bed1 <- rep( bed1, nb2)
		} else if( nb2==1 ){
			bed1 <- rep( bed2, nb1)
		} else{
			stop("too many bed1s or bed2s")
		}
	}


	cmdString <- paste0(
		"cut -f 1,2,3 ",bed1,
		" | bedtools closest -t first -a stdin -b ",bed2,
		" | cut -f 1,2,3,7",if(strand){paste0(",8,9")},
		" > ", outnames
	)

	res <- cmdRun(cmdString, threads=threads)

	return(outnames)

}
