cutadapt.paired <-
function( fastq1 , fastq2=NULL , adapter = "AGATCGGAA" , qualityCutoff=20 , minLength = 30 , minOverlap=1  , clipFromStart=NULL , clipFromEnd=NULL , cores="max" ){
  library(parallel)
  if(cores=="max"){cores=detectCores()-1}
  if(length(fastq1) < cores){cores=length(fastq1)}

  outnamesLeft1<-paste0(basename(removeext(fastq1)),"_clip_tmp.fastq")
  outnamesRight1<-paste0(basename(removeext(fastq2)),"_clip_tmp.fastq")
  outnamesLeft2<-paste0(basename(removeext(fastq1)),"_clip.fastq")
  outnamesRight2<-paste0(basename(removeext(fastq2)),"_clip.fastq")
  logNames<-paste0(basename(removeext(fastq1)),"_clip.log")


  cmdString1<-paste(
    "cutadapt",
    if(!is.null(clipFromStart)){"-u"},
    if(!is.null(clipFromStart)){abs(clipFromStart)},
    if(!is.null(clipFromEnd)){"-u"},
    if(!is.null(clipFromEnd)){-abs(clipFromEnd)},
    "-O",minOverlap,
    "-q",qualityCutoff,
    "-a",adapter,
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
    "-m",minLength,
    "-o",outnamesRight2,
    "-p",outnamesLeft2,outnamesRight1,outnamesLeft1,
    ">>",logNames
  )

  a<-mclapply(1:length(fastq1) , function(x){
    print(cmdString1[x])
    system(cmdString1[x])
  }, mc.cores=cores , mc.preschedule=F)

  b<-mclapply(1:length(fastq1) , function(x){
    print(cmdString2[x])
    system(cmdString2[x])
  }, mc.cores=cores , mc.preschedule=F)

  unlink(c(outnamesLeft1,outnamesRight1))

  return(list(outnamesLeft1,outnamesLeft2))
}
