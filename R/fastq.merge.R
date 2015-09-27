fastq.merge <- function( r1 , r2 , minLength=50 , minOverlap=10 , pValue=0.01 , memory="10G" , threads="max" ){

  library(parallel)
  if(threads=="max"){threads=detectCores()-1}

  if(length(r1)!=length(r2)){stop("must have equal number of forward and reverse reads")}
  numpairs <- length(r1)

  if(all(grepl("R1",r1))){
    outnames <- gsub("R1","merged",basename(removeext(r1)))
  } else{
    outnames <- basename(removeext(r1))
  }
  lognames=paste0(outnames,".log")
  cmdString <- paste(

    "pear-0.9.6-bin-64",
    "-n", minLength,
    "-v", minOverlap,
    "-p", pValue,
    "-y", memory,
    "-j", threads,
    "-o", outnames,
    "-f", r1,
    "-r", r2,
    "2>&1 > ",lognames

  )

  for(i in 1:numpairs){
    print(cmdString[i])
    system(cmdString[i])
  }

  return(outnames)

}
