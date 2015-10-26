samtoolsView <- function( inFiles , outnames = NA , regions=NULL , genome.chrom.sizes=NULL , includeFlag=NULL , excludeFlag=NULL , minQual=NULL , outputBam=TRUE , includeHeader=FALSE , postsort=FALSE , count=FALSE , threads=getOption("threads",1L) ){

  if(includeHeader & is.null(genome.chrom.sizes)){stop("must specify genome.chrom.sizes of includeHeader is TRUE")}

  if( !is.null(regions) ){
    if(file.exists(regions) & length(regions)==1 ){
      oneRegion=FALSE
      regionString <- paste( "-L", regions )
      regionSuffix <- paste0("_",basename(removeext(regions)))
    } else if(grepl(":",regions) & grepl("-",regions)){
      oneRegion=TRUE
      theRegion <- regions
      regionSuffix <- paste0("_",gsub(":","-",regions))
    } else{
      stop("regions must be the path of a single existing bed file")
    }
  } else{
    oneRegion=FALSE
  }

  # generate output file names
  if(is.na(outnames)){
    outnames <- paste0(
      basename(removeext(inFiles)),
      if( !is.null(minQual) ){ "_q" },
      if( !is.null(minQual) ){ minQual },
      if( !is.null(includeFlag) ){ "_f" },
      if( !is.null(includeFlag) ){ includeFlag },
      if( !is.null(excludeFlag) ){ "_F" },
      if( !is.null(excludeFlag) ){ excludeFlag },
      if( !is.null(regions) ){ regionSuffix },
      if( includeHeader){"_reheader" },
      if( postsort ){ "_psort" },
      if( outputBam){ ".bam" } else{ ".sam" }
    )
    #if(postsort){ outnames=removeext(outnames) }

  } else{
    if( length(outnames) != length(inFiles) ){ stop( "lengths of inFiles and outnames must be equal" ) }
  }

  # generate command string
  cmdString <- paste(
    "samtools view",
    if( outputBam ){ "-b" },
    if( includeHeader ){ "-h" },
    if( count ){ "-c" },
    if( includeHeader ){ paste( "-t" , genome.chrom.sizes ) },
    if( !is.null(minQual) ){ paste( "-q" , minQual ) },
    if( !is.null(includeFlag) ){ paste( "-f" , includeFlag ) },
    if( !is.null(excludeFlag) ){ paste( "-F" , excludeFlag ) },
    if( !is.null(regions) & !oneRegion ){ paste( regionString ) },
    inFiles,
    if( oneRegion ){ theRegion },
    if( postsort ){ paste( "| samtools sort -T",outnames,"-o",outnames,"-" ) },
    ">",
    outnames
  )

  # print and execute command string
  res <- cmdRun(cmdString,threads,intern=if(count){TRUE}else{FALSE})

  if(count){
    return(res)
  } else{
    return(outnames)
  }
}
