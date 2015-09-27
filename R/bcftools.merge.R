bcftools.merge <- function( bcfFiles , outPrefix , outputType=NULL  ){

  if(!is.null(outputType)){
    if(!(outputType %in% c("b","u","v","z"))){stop("outputType must be b/u/v/z")}
  } else{
    ext <- file_ext(bcfFiles)[1]
    if(ext=="vcf"){ outputType <- "v" }
    if(ext=="bcf"){ outputType <- "b" }
    if(ext=="gz"){ outputType <- "z" }
  }

  outname <- paste0(
    outPrefix,
    if(outputType=="b"){".bcf"},
    if(outputType=="v"){".vcf"},
    if(outputType=="u"){".bcf"},
    if(outputType=="z"){".vcf.gz"}
  )

  # generate command strings
  cmdString <- paste(
    "bcftools merge",
    "-O",outputType,
    paste(bcfFiles,collapse=" "),
    ">",
    outname
  )

  # print and execute command string
  print(cmdString)
  system(cmdString)
  return(outname)


}
