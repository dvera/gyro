bam.rmdup <-
function( bamFiles, singleEnd=FALSE , pairedAsSingle=FALSE ){

  outnames <- paste0( basename(removeext(bamFiles)) , "_rmdup.bam" )

  # generate command strings
  cmdString <- paste(
    "sambamba markdup -r -p",
    #if(singleEnd){ "-s" },
    #if(pairedAsSingle){ "-S" },
    bamFiles,
    outnames
  )

  # print and execute command string
  for( i in 1:length(bamFiles)){
    print(cmdString)
    system(cmdString)
  }

  return(outnames)

}
