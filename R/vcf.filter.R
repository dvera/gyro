bcftools.filter <- function ( vcf , include="DP>6" , exclude=NULL , outname , outputtype=NULL ){
	system(paste(
		"bcftools filter",
		if(is.null(include) == FALSE){ paste("-i","'",include,"'")},
		if(is.null(exclude) == FALSE){ paste("-e","'",exclude,"'")},
		if(is.null(outputtype) == FALSE){ paste("-O",outputtype)},
		vcf,">",outname
	))
	return(outname)
}
