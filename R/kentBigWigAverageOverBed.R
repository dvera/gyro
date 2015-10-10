kentBigWigAverageOverBed <-
function( beds, bigwigs, bednames=basename(removeext(beds)), wignames=basename(removeext(bigwigs)), targetregions="" ){
	numbeds<-length(beds)
	numwigs<-length(bigwigs)
	shufbeds<-unlist(lapply(beds,bedtoolsShuffle,include=targetregions))
	shufbednames<-paste(bednames,"_shuffled",sep="")
	beds<-c(beds,shufbeds)
	bednames<-c(bednames,shufbednames)
	numbeds<-length(beds)
	bedlist<-lapply(beds,read.tsv)
	beds<-unlist(lapply(1:numbeds,function(x) {
		bed<-bedlist[[x]][,1:3]
		bed[,4]<-1:nrow(bed)
		outname<-paste(bednames[x],".tmp",sep="")
		write.tsv(bed,file=outname)
		outname
	}))

	scoresbybed<-lapply(1:numbeds,function(x){
		lapply(1:numwigs,function(y){
			outname<-paste(bednames[x],"_avg_",wignames[y],".tmp",sep="")
			system(paste("bigWigAverageOverBed",bigwigs[y],beds[x],outname))
			as.numeric(readLines(pipe(paste("cut -f 6",outname))))
		})
	})

	scoresbywig<-lapply(1:numwigs,function(x){
		lapply(lapply(scoresbybed,"[",x),unlist)
	})
	pdf(file="bedwigaverages.pdf")
	par(mar=c(10,4,4,2))

	for(i in 1:numbeds){
		boxplot(scoresbybed[[i]],names=wignames,main=bednames[i],las=2,cex.names=0.5,outline=F)
	}
	for(i in 1:numwigs){
		boxplot(scoresbywig[[i]],names=bednames,main=wignames[i],las=2,cex.names=0.5,outline=F)
	}
	dev.off()
}
