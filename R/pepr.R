pepr <- function( treatmentFiles , controlFiles , prefix , fileFormat="bampe" , peakType="broad" , windowSize=NULL , threshold=0.00001 , threads=getOption("threads",1L) ){
  cmdString <- paste(
    "PePr",
    "--threshold", threshold,
    "--num-processors", threads,
    "-w", windowSize,
    "-c", paste(treatmentFiles,collapse=","),
    "-i", paste(controlFiles,collapse=","),
    "-f", fileFormat,
    "-n", prefix,
    "--peaktype",peakType
  )

  res <- cmdRun(cmdString,threads=1)

}
