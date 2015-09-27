bcftools.call <- function( bcfFiles , outputType="v" , consensusCaller=TRUE , multiAllelicCaller=FALSE , variantsOnly=TRUE, insertMissed=FALSE, regions=NULL, threads=getOption("threads",1L) ){

  if(multiAllelicCaller & consensusCaller){ stop("must choose consensusCaller OR multiAllelicCaller")}

  if(!(outputType %in% c("b","u","v","z"))){stop("outputType must be b/u/v/z")}

  if( !is.null(regions) ){
    if(file.exists(regions)){
      regionString <- paste( "-T", regions )
      regionSuffix <- basename(removeext(regions))
    } else {
      regionString <- paste( "-r", regions )
      if(length(regions)==1){
        regionSuffix <- gsub( ":", "-", regions )
        regionSuffix <- gsub( ",", "", regions )
      } else{
        regionSuffix <- "Regions"
      }
      regions <- paste(regions,collapse=",")
    }
  }

  outnames <- paste0(
    basename(removeext(bcfFiles)),
    paste(
      "",
      if(variantsOnly){"v"},
      if(consensusCaller){"c"},
      if(multiAllelicCaller){"m"},
      sep="_"
    ),
    if(!is.null(regions)){"regionSuffix"},
    if(outputType=="b"){".bcf"},
    if(outputType=="v"){".vcf"},
    if(outputType=="u"){".bcf"},
    if(outputType=="z"){".vcf.gz"}
  )

  # generate command strings
  cmdString <- paste(
    "bcftools call",
    if(variantsOnly){"-v"},
    if(consensusCaller){"-c"},
    if(multiAllelicCaller){"-m"},
    if(insertMissed){paste("-i","-C alleles")},
    if(!is.null(regions)){ regionString },
    "-O",outputType,
    bcfFiles,
    ">",
    outnames
    )

    rage.run(cmdString,threads)

    return(outnames)


}
