fastq.clip.fastx <-
function( fastqfiles , adaptor="GATCGGAA", minlength=10  ){
	outnames<-paste0(basename(removeext(fastqfiles)),"_clipped.fastq")
	for(i in 1:length(fastqfiles)){
		system(paste("fastx_clipper -Q33 -v -n -k -l",minlength,"-a",adaptor,"-i",fastqfiles[i],"-o",outnames[i]))
	}
	return(outnames)
}
