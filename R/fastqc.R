fastqc <-
function( fastqs ){
	for(i in 1:length(fastqs)){
		system(paste("fastqc",fastqs[i]))
	}
}
