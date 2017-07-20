bedtoolsWindow <-
function( b,a, extraargs="-u", header=TRUE, count=FALSE, left=1000, right=1000 , threads=getOption("threads",1L) ){

	bed1name<-basename(removeext(a))
	bed2name<-basename(removeext(b))
	ext<-file_ext(a)

	outname<-paste0(bed1name,"_winL",left,"R",right,"_",bed2name,".",ext)

	
		cmdString<-paste("bedtools window","-l",left,"-r",right,extraargs,if(header){"-header"},"-a",a,"-b",b,">",outname)
		res <- cmdRun(cmdString, threads)
		return(outname)
	

}
