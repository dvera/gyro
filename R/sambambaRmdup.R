sambambaRmdup <-
function( bamFiles, singleEnd=FALSE , pairedAsSingle=FALSE, threads=getOption("threads",1L) ){

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
  res <- cmdRun( cmdString, threads=threads )

  return(outnames)

}
