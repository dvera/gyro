bwaAln <- function ( read1files , referenceFasta , read2files=NULL , cores="max" , qualThresh=NULL , maxInsSize=500 ){

  if(cores=="max"){cores<-detectCores()-1}


  read1names<-basename(removeext(read1files))
  if(grepl(".fastq.gz",read1files)){
    outnames1<-paste0(basename(removeext(removeext(read1files))),".sai")
    outnames3<-paste0(basename(removeext(removeext(read1files))),".sam")
    lognames<-paste0(basename(removeext(removeext(read1files))),".log")

  } else{
    outnames1<-paste0(basename(removeext(read1files)),".sai")
    outnames3<-paste0(basename(removeext(read1files)),".sam")
    lognames<-paste0(basename(removeext(read1files)),".log")
  }
  numfiles=length(read1files)

  if(is.null(read2files)){
    paired=FALSE
  } else{
    paired=TRUE
    outnames2<-paste0(basename(removeext(read2files)),".sai")
  }

  cmdString<-paste(
    "bwa aln",
    "-t",cores,
    if(!is.null(qualThresh)){paste("-q",qualThresh)},
    referenceFasta,
    read1files,
    ">",
    outnames1,
    "2>>",
    lognames
  )

  for(i in 1:numfiles){
    print(cmdString[i])
    system(cmdString[i])
  }

  if(paired){
    cmdString<-paste(
      "bwa aln",
      "-t",cores,
      if(!is.null(qualThresh)){paste("-q",qualThresh)},
      referenceFasta,
      read2files,
      ">",
      outnames2,
      "2>>",
      lognames
    )
    for(i in 1:numfiles){
      print(cmdString[i])
      system(cmdString[i])
    }
  }

  if(cores>numfiles){cores=numfiles}

  if(paired){

    cmdString=paste(
      "bwa sampe",
      "-P",
      "-a",maxInsSize,
      referenceFasta,
      outnames1,
      outnames2,
      read1files,
      read2files,
      ">",
      outnames3,
      "2>>",
      lognames
    )

    dump<-mclapply(1:numfiles, function(i){
      print(cmdString[i])
      system(cmdString[i])
    },mc.cores=cores,mc.preschedule=F)

  } else{

    cmdString=paste(
      "bwa samse",
      referenceFasta,
      outnames1,
      read1files,
      ">",
      outnames3,
      "2>>",
      lognames
    )

    dump<-mclapply(1:numfiles, function(i){
      print(cmdString[i])
      system(cmdString[i])
    },mc.cores=cores,mc.preschedule=F)

  }

  return(outnames3)

}
