samtoolsMpileup <-
function( bamFiles, fastaRef , minMQ=0 , minBQ=13 , maxDepth=250 , regions=NULL , redoBaq=FALSE , noBaq=FALSE , adjustMQ=0 , uncompressed=TRUE ){

  if( !is.null(regions) ){
    if(file.exists(regions)){
      regionString <- paste( "-l", regions )
      regionSuffix <- basename(removeext(regions))
    } else {
      regionString <- paste( "-r", regions )
      regionSuffix <- gsub( ":", "-", regions )
      regionSuffix <- gsub( ",", "", regions )
    }
  }
  outnames <- paste0(
    basename(removeext(bamFiles)),
    "_",
    "q", minMQ,
    "Q", minBQ,
    "d", maxDepth,
    "C", adjustMQ,
    if(redoBaq){"E"},
    if(noBaq){"B"},
    if(!is.null(regions)){"_"},
    if(!is.null(regions)){regionSuffix},
    ".bcf"
  )

  # generate command strings
  cmdString <- paste(
    "samtools mpileup",
    "-q", minMQ,
    "-Q", minBQ,
    "-d", maxDepth,
    "-C", adjustMQ,
    if(redoBaq){ "-E" },
    if(noBaq){ "-B" },
    if(uncompressed){ "-u" },
    if( !is.null(regions) ){ regionString },
    "-f", fastaRef,
    bamFiles,
    ">",
    outnames
  )

  # print and execute command string
  for( i in 1:length(bamFiles) ){
    print(cmdString[i])
    system(cmdString[i])
  }

  return(outnames)

}
