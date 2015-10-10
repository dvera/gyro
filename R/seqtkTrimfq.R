seqtkTrimfq <- function( fastqFiles, threshold=0.05, trimFromStart=NA, trimFromEnd=NA ){

  if(is.na(trimFromStart) & is.na(trimFromEnd)){
    outnames <- paste0(
      basename(removeext(fastqFiles)),
      "_qTrim",
      ".fastq"
    )
  } else{
    outnames <- paste0(
      basename(removeext(fastqFiles)),
      "_trim",
      if(!is.na(trimFromStart)){"L"},
      if(!is.na(trimFromStart)){trimFromStart},
      if(!is.na(trimFromEnd)){"R"},
      if(!is.na(trimFromEnd)){trimFromEnd},
      ".fastq"
    )
  }

  cmdString <- paste(
    "seqtk trimfq",
    "-q",threshold,
    if(!is.na(trimFromStart)){"-b"},
    if(!is.na(trimFromStart)){trimFromStart},
    if(!is.na(trimFromEnd)){"-e"},
    if(!is.na(trimFromEnd)){trimFromEnd},
    fastqFiles,
    ">",
    outnames
  )

  for(i in 1:length(fastqFiles)){
    print( cmdString[i] )
    system( cmdString[i] )
  }

  return(outnames)

}
