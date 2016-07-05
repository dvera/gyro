#@param

cutadapt <-
function( fastq1 , fastq2=NULL , adapter = "AGATCGGAAGAGCACACGTCTG" , qualityCutoff=0 , minLength = 0 , minOverlap=1  , clipFromStart=NULL , clipFromEnd=NULL , threads=getOption("threads",1L) ){

  # check if filtering is done
  if( minLength>0){
    filt=TRUE
  } else{
    filt=FALSE
  }

  # check if input is paired-end reads
  if(is.null(fastq2)){
    paired=FALSE
  } else{
    if(length(fastq1) != length(fastq2)){stop("mate pair mismatch")}
    paired=TRUE
  }

  # set output names
  if( all(file_ext(fastq1)=="gz") & all(file_ext(removeext(fastq1))=="fastq")){
    outnamesLeft1<-paste0(basename(removeext(removeext(fastq1))),"_clip_tmp.fastq")
    outnamesRight1<-paste0(basename(removeext(removeext(fastq2))),"_clip_tmp.fastq")
    outnamesLeft2<-paste0(basename(removeext(removeext(fastq1))),"_clip.fastq")
    outnamesRight2<-paste0(basename(removeext(removeext(fastq2))),"_clip.fastq")
    logNames<-paste0(outnamesLeft2,".log")
  } else{
    outnamesLeft1<-paste0(basename(removeext(fastq1)),"_clip_tmp.fastq")
    outnamesRight1<-paste0(basename(removeext(fastq2)),"_clip_tmp.fastq")
    outnamesLeft2<-paste0(basename(removeext(fastq1)),"_clip.fastq")
    outnamesRight2<-paste0(basename(removeext(fastq2)),"_clip.fastq")
    logNames<-paste0(outnamesLeft2,".log")
  }


  if(filt & paired){

    cmdString1<-paste(
      "cutadapt",
      if(!is.null(clipFromStart)){"-u"},
      if(!is.null(clipFromStart)){abs(clipFromStart)},
      if(!is.null(clipFromEnd)){"-u"},
      if(!is.null(clipFromEnd)){-abs(clipFromEnd)},
      "-O",minOverlap,
      "-q",paste0(qualityCutoff,",",qualityCutoff),
      "-a",adapter,
      "-A",adapter,
      "-m",minLength,
      "-o",outnamesLeft1,
      "-p",outnamesRight1,fastq1,fastq2,
      ">",logNames
    )

    cmdString2<-paste(
      "cutadapt",
      if(!is.null(clipFromStart)){"-u"},
      if(!is.null(clipFromStart)){abs(clipFromStart)},
      if(!is.null(clipFromEnd)){"-u"},
      if(!is.null(clipFromEnd)){-abs(clipFromEnd)},
      "-O",minOverlap,
      "-q",qualityCutoff,
      "-a",adapter,
      "-A",adapter,
      "-m",minLength,
      "-o",outnamesRight2,
      "-p",outnamesLeft2,outnamesRight1,outnamesLeft1,
      ">>",logNames
    )

    res <- cmdRun(cmdString1,threads=threads)

  } else{
    cmdString2<-paste(
      "cutadapt",
      if(!is.null(clipFromStart)){"-u"},
      if(!is.null(clipFromStart)){abs(clipFromStart)},
      if(!is.null(clipFromEnd)){"-u"},
      if(!is.null(clipFromEnd)){-abs(clipFromEnd)},
      "-a",adapter,
      if(paired){paste("-A",adapter)},
      "-q",qualityCutoff,
      "-O",minOverlap,
      "-m",minLength,
      "-o",outnamesLeft2,
      if(paired){paste("-p",outnamesRight2)},
      fastq1,
      if(paired){fastq2},
      ">",logNames
    )
  }

  res <- cmdRun(cmdString2,threads=threads)

  unlink(c(outnamesLeft1,outnamesRight1))

  if(paired){
    return(list(outnamesLeft2,outnamesRight2))
  } else{
    return(outnamesLeft2)
  }

}
