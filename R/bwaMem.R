bwaMem <- function ( read1files , indexPrefix , read2files=NULL , threads=getOption("thread",1L) ){

  if(grepl(".fastq.gz",read1files)){
    outnames<-paste0(basename(removeext(removeext(read1files))),".sam")
    lognames<-paste0(basename(removeext(removeext(read1files))),".log")
  } else{
    outnames<-paste0(basename(removeext(read1files)),".sam")
    lognames<-paste0(basename(removeext(read1files)),".log")
  }

  numfiles=length(read1files)

  if(is.null(read2files)){
    paired=FALSE
  } else{
    paired=TRUE
    stopifnot(length(read1files)==length(read2files))
  }

  cmdString<-paste(
    "bwa mem",
    "-t",threads,
    indexPrefix,
    read1files,
    if(paired){read2files},
    ">",
    outnames,
    "2>>",
    lognames
  )

  cmdRun(cmdString,threads=1)

  return(outnames)

}
