featureCounts <- function( alignments , annotation , outname , pairCounts=FALSE , pairInsertRange=NULL , pairAlignmentRequired=FALSE , minQual=0 , stranded=0, promiscuousReads=FALSE , ignoreDup=FALSE , featureType="exon" , groupBy="genes" , groupByFeature=FALSE, threads=getOption("threads",1L) ){


  cmdString <- paste(
    "featureCounts",
    "-Q", minQual,
    "-a", annotation,
    "-o", outname,
    "-t", featureType,
    "-g", groupBy,
    "-T", threads,
    "-s", stranded,
    if(pairCounts){"-p"},
    if(!is.null(pairInsertRange)){paste("-P","-d",pairInsertRange[1],"-D",pairInsertRange[2]},
    if(pairAlignmentRequired){"-B"},
    if(ignoreDup){"--ignoreDup"},
    if(promiscuousReads){"-O"},
    if(groupByFeature){"-f"},
    paste(alignments,collapse=" ")
  )

  res <- rage.run(cmdString)

  return(outname)
}
