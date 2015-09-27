bcftools.view.incomplete <- function( bcfFiles , outputType="v" , variantsOnly=FALSE ){

  if(multiAllelicCaller & consensusCaller){ stop("must choose consensusCaller OR multiAllelicCaller")}

  if(!(outputType %in% c("b","u","v","z"))){stop("outputType must be b/u/v/z")}

  outnames <- paste0(
    basename(removeext(bcfFiles)),
    paste0(
      if(variantsOnly){"_varOnly"},
      if(consensusCaller){"_cCall"},
      if(multiAllelicCaller){"_mCall"}
    ),

    if(outputType=="b"){".bcf"},
    if(outputType=="v"){".vcf"},
    if(outputType=="u"){".bcf"},
    if(outputType=="z"){".vcf.gz"}
  )

  # generate command strings
  cmdString <- paste(
    "bcftools call",
    if(variantsOnly){"-v"},
    #if(consensusCaller){"-c"},
    #if(multiAllelicCaller){"-m"},
    bcfFiles,
    ">",
    outnames
    )

    # print and execute command string
    for( i in 1:length(bcfFiles)){
      print(cmdString)
      system(cmdString)
    }

    return(outnames)


}
