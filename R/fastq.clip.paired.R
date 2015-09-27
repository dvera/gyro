fastq.clip.paired <-
function( fastq1 , fastq2=NULL , adapter = "GATCGGAA" , quality.cutoff=20 , minlength = 10 , cores="max"){
	library(parallel)
	if(cores=="max"){cores=detectCores()-1}
	if(length(fastq1) < cores){cores=length(fastq1)}
	outnames<-paste0(basename(removeext(fastq1)),"_aclip.fastq")
	a<-mclapply(1:length(fastq1) , function(x) system(paste("cutadapt -O 1 -a",adapter,fastq1[x],">",outnames[x])) , mc.cores=cores , mc.preschedule=F)
	return(outnames)
}
