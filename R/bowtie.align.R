bowtie <-
function( read1files , indexfile , mode="-n", maxmismatch=0 , reportmax=1 , all=FALSE, qvals=NULL , colorspace=FALSE , read2files=NULL , strata=FALSE , cores="max" , input="-q", q=10, makebam = TRUE , makebed = TRUE , chunkmbs=64){

	library(parallel)
	if(cores=="max"){cores<-detectCores()-1}
	if(cores > length(read1files)) { cores <- length(read1files) }

	read1names<-basename(removeext(read1files))
	outnames<-paste0(read1names,".sam")
	lognames <- paste0(read1names,".log")

	if(is.null(read2files)){
		paired=FALSE
	} else{
		paired=TRUE
	}

	if(paired==TRUE){
		cmdString <- paste(
			"bowtie",
			mode,
			maxmismatch,
			if(strata){"--strata --best"},
			if(all){"-a"}else{paste("-k",reportmax)},
			"-S",
			"--chunkmbs",chunkmbs,
			"-p",cores,
			if(colorspace){"-C"},
			input,
			indexfile,
			"-1",read1files,
			"-2",read2files,
			outnames,
			"2>>",lognames
		)
	} else{
		cmdString <- paste(
			"bowtie",
			mode,
			maxmismatch,
			if(strata){"--strata --best"},
			if(all){"-a"}else{paste("-k",reportmax)},
			"-S",
			"--chunkmbs",chunkmbs,
			"-p",cores,
			if(colorspace){"-C"},
			if(is.null(qvals)==FALSE){ "-Q" },
			if(is.null(qvals)==FALSE){ qvals },
			input,
			indexfile,
			read1files,
			outnames,
			"2>>",lognames
		)
	}

	#if(makebam) { outnames<-unlist(mclapply(outnames,sam.2.bam,mc.cores=cores,q=q)) }
	#if(makebed) { outnames<-unlist(mclapply(outnames,bam.2.bed,if(paired){paired=TRUE} else{paired=FALSE},mc.cores=cores))}

	for(i in 1:length(read1files)){
		print(cmdString[i])
		system(cmdString[i])

	}

	return(outnames)
}
