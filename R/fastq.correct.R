fastq.correct <- function( r1 , threads="max", kmerLength=33, minCoverage=3, dropUnique=FALSE, genomeSize=NA ){

  library(parallel)
  if(threads=="max"){threads=detectCores()-1}

  outnames <- paste0(basename(removeext(r1)),"_bfc.fastq")


  cmdString <- paste(

    "bfc",
    "-t",threads,
    "-c",minCoverage,
    "-k",kmerLength,
    if(!is.na(genomeSize)){paste("-s",genomeSize)},
    r1,
    ">",
    outnames
  )

  for(i in 1:length(r1) ){
    print(cmdString[i])
    system(cmdString[i])
  }

  return(outnames)

}
