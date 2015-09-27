bed.slop <-
function( bedfile, genomefile , expandleft, expandright, strand = FALSE ){
	library(tools)
	bedname<-basename(removeext(bedfile))
	ext<-file_ext(bedfile)
	outname<-paste0(basename(removeext(bedfile)),"_sl",expandleft,"_sr",expandright,".",ext)
	system(paste("bedtools slop",if(strand){"-s"} , "-i",bedfile,"-g",genomefile,"-l",expandleft,"-r",expandright,">",outname))
	return(outname)
}
