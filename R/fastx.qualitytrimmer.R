fastx.qualitytrimmer <- function( fastqFiles, threshold=25, minLength=0, phred33=TRUE ){

  outnames <- paste0(
    basename(removeext(fastqFiles)),
    "_clip",
    threshold,
    ".fastq"
  )

  cmdString <- paste(
    "fastq_quality_trimmer",
    if(phred33){"-Q33"},
    "-t", threshold,
    "-l", minLength,
    "-i", fastqFiles,
    ">",
    outnames
  )

  for(i in 1:length(fastqFiles)){
    print( cmdString[i] )
    system( cmdString[i] )
  }

  return(outnames)

}
