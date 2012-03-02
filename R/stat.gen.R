stat.gen <-
function(num.subj, target.mean, target.sd, data.dec=2, non.neg=FALSE){
	iteration<-0
	target.mean<-round(target.mean,data.dec+1)
	target.sd<-round(target.sd,data.dec+1)
	dif<-target.mean-target.sd	
		repeat{
			iteration=iteration+1
			cat("working on iteration",iteration,"\r")
			ifelse(non.neg==TRUE&dif<=0.5,x<-scale(rexp(num.subj)),x<-scale(rnorm(num.subj,target.mean,target.sd)))
			ifelse(non.neg==TRUE,x.2<-abs(round(matrix(x[,1]*target.sd+target.mean),data.dec)),x.2<-round(matrix(x[,1]*target.sd+target.mean),data.dec))			
			if(round(apply(x.2,2,mean),data.dec+1)==target.mean&round(apply(x.2,2,sd),data.dec+1)==target.sd|iteration==100000)
			break
			}
	if(iteration==100000)	cat("Maximum Number of Iterations Reached Without A Solution. Try Again!","\b")
		else{
			x.2<-round(matrix(x[,1]*target.sd+target.mean),data.dec)
			colnames(x.2)<-"SCORES"
			write.table(x.2,file="MorseGen Sample Results.txt",row.names=FALSE)
			sim.perf<-data.frame(rbind(c(target.mean,round(apply(x.2,2,mean),data.dec+2),target.sd,round(apply(x.2,2,sd),data.dec+2),iteration)))
			colnames(sim.perf)<-c("Target Mean","Sample Mean","Target SD","Sample SD","Iterations")
			cat("Summary of the Simulation Results (note: raw data has been exported to the file 'MorseGen Sample Results.txt' in your working directory","\b")
			cat(")","\n")
			list(DATA=x.2, PERFORMANCE=sim.perf)
			}
}