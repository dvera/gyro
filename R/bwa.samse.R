bwa.samse <- function ( saiFiles , fastqFiles , referenceFasta , maxInsSize=500 , maxOcc=100000 ){

  outnames<-paste0(basename(removeext(saiFiles)),".sam")

  cmdString <- paste(
    "bwa samse",
    "-n",maxOcc,
    referenceFasta,
    saiFiles,
    fastqFiles,
    ">",
    outnames
  )

  for(i in 1:length(saiFiles)){
    system(cmdString[i])
  }

  return(outnames)
}
