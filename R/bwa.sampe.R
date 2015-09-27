bwa.sampe <- function ( outnames1 , outnames2 , read1files , read2files , referenceFasta , maxInsSize=500 , maxOcc=100000 ){

  outnames3<-paste0(basename(removeext(outnames1)),".sam")

  for(i in 1:length(outnames1)){


    system(paste(
      "bwa sampe",
      "-P",
      "-a",maxInsSize,
      "-o",maxOcc,
      referenceFasta,
      outnames1[i],
      outnames2[i],
      read1files[i],
      read2files[i],
      ">",
      outnames3[i]
    ))

  }

  return(outnames3)
}
