bcftools.query <- function( bcfFiles, fields, suffix=NULL, exclude=NULL, include=NULL ){

  queryString <- paste0("\'", paste(c("%CHROM","%POS","%POS",paste0("%",fields)), collapse="\t"), "\n\'")
  outnames <- paste0(
    basename(removeext(bcfFiles)),
    "_query",
    if(!is.null(suffix)){suffix},
    ".txt"
  )

  cmdString <- paste(
    "bcftools query",
    "-f",queryString,
    if(is.null(include) == FALSE){ paste("-i","'",include,"'")},
    if(is.null(exclude) == FALSE){ paste("-e","'",exclude,"'")},
    bcfFiles,"| awk '{$2=$2-1; print $0}' OFS='\t' >",outnames
  )

  # print and execute command string
  for( i in 1:length(bcfFiles)){
    print(cmdString[i])
    system(cmdString[i])
  }

  return(outnames)

}
