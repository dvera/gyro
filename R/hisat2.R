hisat2 <-	function( read1files, indexfile , read2files=NULL, splicedAlignment=TRUE , strandedness=NULL, maxAlignments=NULL , threads=getOption("threads",1L), input="-q", all=FALSE , dovetail=FALSE , discordant=FALSE , mixed=FALSE , unaligned=TRUE , maxInsertSize=500 , minInsertSize=0 , appendIndexToName=FALSE , reorder=FALSE, softClipPenalty=NULL , extraargs="" ){


		read1names<-basename(removeext(read1files))
		if(appendIndexToName){ read1names=paste0(read1names,"_",basename(indexfile)) }
		outnames<-paste0(read1names,".sam")
		lognames <- paste0(outnames,".log")

		if(is.null(read2files)){
			paired=FALSE
		} else{
			paired=TRUE
		}

		if(paired==TRUE){
			cmdString <- paste(
				"hisat2",
				extraargs,
				"-p",threads,
				# search options
				if(!is.null(strandedness)){paste("--rna-strandness",strandedness)},
				if(!(splicedAlignment)){"--no-spliced-alignment"},
				# reporting options
				if(dovetail){"--dovetail"},
				if(mixed==FALSE){"--no-mixed"},
				if(discordant==FALSE){"--no-discordant"},
				if(unaligned==FALSE){"--no-unal"},
				if(reorder){"--reorder"},
				# filtering
				"-I",minInsertSize,
				"-X",maxInsertSize,
				if(all){"-a"},
				if(!is.null(maxAlignments)){paste("-k",maxAlignments)},
				if(!is.null(softClipPenalty)){paste("--sp",softClipPenalty)},
				# required arguments
				input,
				"-x",indexfile,
				"-1",read1files,
				"-2",read2files,
				"-S",outnames,
				"2>>",lognames
			)
		}	else{
			cmdString <- paste(
				"hisat2",
				extraargs,
				"-p",threads,
				if(unaligned==FALSE){"--no-unal"},
				if(reorder){"--reorder"},
				if(all){"-a"},
				if(!is.null(maxAlignments)){paste("-k",maxAlignments)},
				if(!is.null(softClipPenalty)){paste("--sp",softClipPenalty)},
				input,
				"-x",indexfile,
				"-U",read1files,
				"-S",outnames,
				"2>>",lognames
			)
		}

		res <- cmdRun(cmdString,threads=1)

		return(outnames)

	}


#
# 	library(parallel)
# 	if(threads=="max"){threads<-detectCores()-1}
# 	numfiles=length(read1files)
#
#
# 	if(!is.null(read2files)){
# 		paired<-TRUE
# 		if(numfiles!=length(read2files)){stop("must have the same number of mates")}
# 	} else{
# 		paired<-FALSE
# 	}
#
# 	if(is.null(outnames)){
# 		outnames <- paste0(basename(removeext(read1files)),".sam")
# 		lognames <- paste0(basename(removeext(read1files)),".sam.log")
# 	} else{
# 		if(numfiles != length(outnames)){stop("outnames and number of input files must be same length")}
# 	}
#
#
# 	cmdString <- paste(
# 		"hisat2",
# 		"-p",threads,
# 		"-x",index,
# 		if(!is.null(strandedness)){paste("--rna-strandness",strandedness)},
# 		if(paired){
# 			paste("-1",read1files,"-2",read2files)
# 		} else{
# 			paste("-U",read1files)
# 		},
# 		"-S",outnames,
# 		"2>",lognames
# 	)
#
# 	cmdRun(cmdString, threads=1)
#
# 	return(outnames)
# }
