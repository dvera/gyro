fastqp <- function( fastqps , pdfname , metadata=NULL , sampleNames=NULL,  ... ){

  library(pheatmap)
  #sampleNames=basename(removeext(fastqps))


  if(!is.null(metadata)){md=read.tsv(metadata,header=TRUE,row.names=1)}

  sampleNames=basename(removeext(fastqps))
  row.names(md)=removeext(row.names(md))

  fl=lapply(fastqps,read.tsv,header=T)
  for(i in 1:length(fl)){ fl[[i]][,8:12] <- fl[[i]][,8:12]/rowSums(fl[[i]][1,8:12]) }

  # get quality scores
  q5=as.data.frame(lapply(fl,"[",3))
  q25=as.data.frame(lapply(fl,"[",4))
  q50=as.data.frame(lapply(fl,"[",5))
  q75=as.data.frame(lapply(fl,"[",6))
  q95=as.data.frame(lapply(fl,"[",7))

  # get base composition
  ac=as.data.frame(lapply(fl,"[",8))
  cc=as.data.frame(lapply(fl,"[",9))
  tc=as.data.frame(lapply(fl,"[",10))
  gc=as.data.frame(lapply(fl,"[",11))
  nc=as.data.frame(lapply(fl,"[",12))

  colnames(q5)<-colnames(q25)<-colnames(q50)<-colnames(q75)<-colnames(q95)<-sampleNames
  colnames(ac)<-colnames(cc)<-colnames(tc)<-colnames(gc)<-colnames(nc)<-sampleNames

  rownames(q5)<-rownames(q25)<-rownames(q50)<-rownames(q75)<-rownames(q95)<-as.character(rownames(q5))
  rownames(ac)<-rownames(cc)<-rownames(tc)<-rownames(gc)<-rownames(nc)<-as.character(rownames(q5))

  #pheatmap(t(q50),cluster_cols=F,show_colnames=T,main="A composition",)
  #pheatmap(t(ac),cluster_cols=F,show_colnames=T,main="A composition",)
  pdf(file=pdfname, width=18, height=9)

  pheatmap(t(q5), cluster_cols=F,show_colnames=T,main="Quality (5th percentile)", annotation_row=md, ...)
  pheatmap(t(q25),cluster_cols=F,show_colnames=T,main="Quality (25th percentile)",annotation_row=md, ...)
  pheatmap(t(q50),cluster_cols=F,show_colnames=T,main="Quality (50th percentile)",annotation_row=md, ...)
  pheatmap(t(q75),cluster_cols=F,show_colnames=T,main="Quality (75th percentile)",annotation_row=md, ...)
  pheatmap(t(q95),cluster_cols=F,show_colnames=T,main="Quality (95th percentile)",annotation_row=md, ...)

  pheatmap(t(ac), cluster_cols=F,show_colnames=T,main="A (base) composition", annotation_row=md, ...)
  pheatmap(t(cc), cluster_cols=F,show_colnames=T,main="C (base) composition", annotation_row=md, ...)
  pheatmap(t(tc), cluster_cols=F,show_colnames=T,main="T (base) composition", annotation_row=md, ...)
  pheatmap(t(gc), cluster_cols=F,show_colnames=T,main="G (base) composition", annotation_row=md, ...)

  dev.off()


}
