bwaIndex <- function( genomeFasta, largeGenome=TRUE ){

  dir.create("bwaIndex")

  cmdString <- paste(
    "bwa index",
    "-p genome.fa",
    if(largeGenome){"bwtsw"} else{"in"},
    genomeFasta
  )

  print(cmdString)
  exitStatus <- system(cmdString)
  if(exitStatus==0){
    cat("indexing successful\n")
  } else{
    stop("indexing failed")
  }

  cmdString <- paste(
    "cp", genomeFasta,
    "; mv genome.fa.* bwaIndex/"
  )
}
