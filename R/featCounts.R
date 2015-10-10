featCounts <- function( alignments , annotation , outname , minQual=NULL , featureType="exon" , groupBy="genes" , groupByFeature=FALSE, threads=getOption("threads",1L) ){


  cmdString <- paste(
    "featureCounts",
    if(!is.null(minQual) & is.numeric(minQual) ){paste("-Q",minQual)},
    "-a", annotation,
    "-o", outname,
    "-t", featureType,
    "-g", groupBy,
    "-T", threads,
    if(groupByFeature){"-f"},
    paste(alignments,collapse=" ")
  )

  res <- rage.run(cmdString)

  return(outname)
}
