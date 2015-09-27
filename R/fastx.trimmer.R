fastx.trimmer <- function( fastqFiles, first=1, last=NA, phred33=TRUE ){

  outnames <- paste0(
    basename(removeext(fastqFiles)),
    "_trim",
    first,
    "-",
    if(is.na(last)){"e"} else{last},
    ".fastq"
  )

  cmdString <- paste(
    "fastx_trimmer",
    if(phred33){"-Q33"},
    "-f", 1,
    if(!is.na(last)){"-l"},
    if(!is.na(last)){last},
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
