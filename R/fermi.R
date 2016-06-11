fermi <- function(r1,r2,prefix,interleaved=FALSE , minOverlap=50, skipErrorCorrection=FALSE , threads=getOptions("threads",1L) ){

  if(missing(r2)){paired=FALSE} else{paired=TRUE}

  if(missing(prefix)){
    prefix=basename(removeext(r1))
  } else{
    stopifnot(length(prefix)==length(r1))
  }
  makefilenames <- paste0(prefix,".mak")
  cmdString <- paste(
    "run-fermi.pl",
    "-t",threads,
    if(paired){"-P"},
    if(interleaved){"-c"},
    "-k", minOverlap,
    "-p", prefix,
    if(skipErrorCorrection){"-C"},
    r1,
    if(paired){r2},
    ">",
    makefilenames
  )

  res <- cmdRun(cmdString)

  cmdString <- paste(
    "make -f",
    makefilenames,
    "-j",threads
  )

  res <- cmdRun(cmdString, threads=1)

  return(paste0(prefix,".p5.fq.gz"))

}
