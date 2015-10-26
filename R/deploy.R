
deploy<-function(message="no message"){
	library(gyro)
	res <- system(paste0("git -C /lustre/maize/home/dlv04c/software/r/gyro/ add /lustre/maize/home/dlv04c/software/r/gyro/ &&\
	git -C /lustre/maize/home/dlv04c/software/r/gyro/ commit -a -m '",message,"' &&\
	git -C /lustre/maize/home/dlv04c/software/r/gyro/ push"))
	library(devtools)
	detach("package:gyro",unload=T)
	install_github("dvera/gyro")
	library(gyro)
}
