seqtkSeq <-  function( fastqFiles, randomlySample=NA, minLength=NA, reverseCompliment=FALSE, maskThreshold=NA, maskChar="N", outputFasta=FALSE ){

  outnames <- paste0(
      basename(removeext(fastqFiles)),
      if(reverseCompliment){"Revcomp"},
      if(!is.na(maskThreshold)){"Mask"},
      if(!is.na(maskThreshold)){maskThreshold},
      if(!is.na(minLength)){"Minlen"},
      if(!is.na(minLength)){minLength},
      if(!is.na(randomlySample)){"Sample"},
      if(!is.na(randomlySample)){minLength},
      if(outputFasta){".fa"} else{".fastq"}
  )

  cmdString <- paste(
    "seqtk seq",
    if(!is.na(maskThreshold)){paste("-q",maskThreshold)},
    if(!is.na(maskThreshold)){paste("-n",maskChar)},
    if(reverseCompliment){"-r"},
    if(!is.na(randomlySample)){paste("-f",randomlySample)},
    if(!is.na(minLength)){paste("-L",minLength)},
    if(outputFasta){"-A"},
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
