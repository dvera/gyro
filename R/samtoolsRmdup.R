samtoolsRmdup <- function( inFiles , singleEnd=FALSE , pairedAsSingle=FALSE , outputFormat="BAM",threads=getOption("threads",1L) ){
  
  # generate output file names
  outnames <- paste0(
      basename(removeext(inFiles)),
      if( singleEnd ){ "_singleEnd" },
      if( pairedAsSingle ){ "_pairedAsSingle" },
      "_rmdup.",
      tolower(outputFormat)
    )
    
   # generate command string
  cmdString <- paste(
    "samtools rmdup",
    if( pairedAsSingle ){ "-S" },
    if( singleEnd ){ "-s" },
    inFiles,
    outnames
  )
    
  # print and execute command string
  res <- cmdRun(cmdString,threads)

  return(outnames)
  
}
