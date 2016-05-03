bedtoolsComplement <- function( bedfiles , chromsizes , outnames ){



  if(missing(chromsizes)){
		chromsizes<-getOption("chromsizes",NULL)
		if(is.null(chromsizes)){stop("must define file contain chromosome sizes")}
	}
	if(!file.exists(chromsizes)){
		stop("chromsizes file does not exist")
	}

  if(missing(outnames)){
    outnames <- paste0(basename(removeext(bedfiles)),"_complement.bed")
  } else{
    if(length(outnames) != length(bedfiles)){stop("length of outnames must match length of bedfiles")}
  }

  cmdString <- paste(
    "bedtools complement -i",
    bedfiles,
    "-g", chromsizes,
    ">", outnames
  )

  res <- cmdRun(cmdString)

  return(outnames)


}
