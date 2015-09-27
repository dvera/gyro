kallisto.quant <- function( r1 , index , r2=NULL , outnames=NULL , fraglength=NULL , plaintext=TRUE ){

  numfiles <- length(r1)
  if(is.null(outnames)){ outnames <- paste0(basename(removeext(r1)),"_kallisto") }
  if(is.null(r2) & is.null(fraglength)){stop("if single-end, fraglength must be specified")}

  cmdString <- paste(
    "kallisto quant",
    "-i", index,
    "-o", outnames,
    if(plaintext){"--plaintext"},
    if(is.null(r2)){"--single"},
    if(is.null(r2)){paste("-l",fraglength)},
    r1,
    if(!is.null(r2)){r2}
  )
  for(i in 1:numfiles){
    print(cmdString[i])
    system(cmdString[i])
  }
  return(outnames)
}
