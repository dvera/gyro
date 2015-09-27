fastq.clip <-
function( fastqfiles, adapter = "GATCGGAA" , quality.cutoff=20 , minlength = 10 , cores="max"){
	library(parallel)
	if(cores=="max"){cores=detectCores()-1}
	if(length(fastqfiles) < cores){cores=length(fastqfiles)}
	outnames<-paste0(basename(removeext(fastqfiles)),"_aclip.fastq")
	a<-mclapply(1:length(fastqfiles) , function(x) system(paste("cutadapt -O 1","-m",minlength,"-q",quality.cutoff,"-a",adapter,fastqfiles[x],">",outnames[x])) , mc.cores=cores , mc.preschedule=F)
	return(outnames)
}
