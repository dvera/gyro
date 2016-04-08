bowtie2 <-
function( read1files, indexfile , read2files=NULL,threads=getOption("threads",1L), alignMode="end-to-end",input="-q", all=FALSE , dovetail=FALSE , discordant=FALSE , mixed=FALSE , unaligned=TRUE , maxInsertSize=500 , minInsertSize=0 , appendIndexToName=FALSE , reorder=FALSE, extraargs="" ){


	read1names<-basename(removeext(read1files))
	if(appendIndexToName){ read1names=paste0(read1names,"_",basename(indexfile)) }
	outnames<-paste0(read1names,".sam")
	lognames <- paste0(read1names,".log")

	if(is.null(read2files)){
		paired=FALSE
	} else{
		paired=TRUE
	}

	if(paired==TRUE){
		cmdString <- paste(
			"bowtie2",
			extraargs,
			paste0("--",alignMode),
			"-p",threads,
			if(dovetail){"--dovetail"},
			if(mixed==FALSE){"--no-mixed"},
			if(discordant==FALSE){"--no-discordant"},
			if(unaligned==FALSE){"--no-unal"},
			if(reorder){"--reorder"},
			"-I",minInsertSize,
			"-X",maxInsertSize,
			if(all){"-a"},
			input,
			"-x",indexfile,
			"-1",read1files,
			"-2",read2files,
			"-S",outnames,
			"2>>",lognames
		)
	}	else{
		cmdString <- paste(
			"bowtie2",
			extraargs,
			paste0("--",alignMode),
			"-p",threads,
			if(unaligned==FALSE){"--no-unal"},
			if(reorder){"--reorder"},
			if(all){"-a"},
			input,
			"-x",indexfile,
			"-U",read1files,
			"-S",outnames,
			"2>>",lognames
		)
	}

	for( i in 1:length(read1files)){
		print(cmdString[i])
		system(cmdString[i])
	}
	#if(makebam) { outnames<-unlist(mclapply(outnames,sam.2.bam,mc.cores=cores,q=q)) }
	#if(makebed) { outnames<-unlist(mclapply(outnames,bam.2.bed,if(paired){paired=TRUE} else{paired=FALSE},mc.cores=cores))}

	return(outnames)

}
