bwaSw <-
function( read1files, read2files=NULL , indexfile , cores="max" , mode="end-to-end", input="-q", q=10, makebam = TRUE , makebed = TRUE ){

	library(parallel)
	if(cores=="max"){cores<-detectCores()-1}


	read1names<-basename(removeext(read1files))

	if(is.null(read2files)){
		paired=FALSE
		outnames<-paste(basename(removeext(read1files)),".sam",sep="")
	} else{
		paired=TRUE
		outnames<-paste(basename(removeext(read1files)),".sam",sep="")
	}

	for(i in 1:length(read1files)){
		cat(read1names[i],": aligning to genome\n")
		if(paired==TRUE){
			system(paste(
				"bwa bwasw",
				"-t",cores,
				indexfile,
				read1files[i],
				read2files[i],
				">",
				outnames[i]
			))
		}
		else{
			system(paste(
				"bwa bwasw",
				"-t",cores,
				indexfile,
				read1files[i],
				">",
				outnames[i]
			))
		}
	}

	if(cores > length(read1files)) { cores <- length(read1files) }
	if(makebam) { outnames<-unlist(mclapply(outnames,sam.2.bam,mc.cores=cores,q=q)) }
	if(makebed) { outnames<-unlist(mclapply(outnames,bam.2.bed,if(paired){paired=TRUE} else{paired=FALSE},mc.cores=cores))}

	return(outnames)
}
