bed.makewindows <-
function( bedfile, windowsize=25, stepsize=windowsize, mergebed=TRUE, mergeflank=0, outname="default", genome=FALSE){
	options(scipen=99999)
	cat(bedfile,": making windows\n")
	if(outname=="default"){outname<-paste(basename(removeext(bedfile)),"_win",windowsize,".bed",sep="")}
	if(mergebed==TRUE & genome==FALSE){
		bedfile<-bed.merge(bedfile,flank=mergeflank)
	}
	if(windowsize>stepsize){
		extraargs<-"| awk 'x != $3; {x=$3}'"
	}
	else{
		extraargs<-""
	}
	if(genome==TRUE){
		inputarg="-g"
	} else{
		inputarg="-b"
	}
	print(paste("bedtools makewindows -w",windowsize,"-s",stepsize,inputarg,bedfile,extraargs,">",outname))

	system(paste("bedtools makewindows -w",windowsize,"-s",stepsize,inputarg,bedfile,extraargs,">",outname))
	return(outname)
}
