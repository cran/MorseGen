stat.gen <-
function(num.subj, target.mean, target.sd, data.dec=2, non.neg=FALSE, max.iter=100000){
		iteration<-0
		target.mean<-round(target.mean,2) 
		target.sd<-round(target.sd,2)
			repeat{
				iteration=iteration+1
				cat("working on iteration",iteration,"\r")
				ifelse(non.neg==TRUE,b<-abs(round(rnorm(num.subj,target.mean,target.sd),data.dec)),b<-round(rnorm(num.subj,target.mean,target.sd),data.dec))
				mean.b<-round(mean(b),2)
				sd.b<-round(sd(b),2)
				if(mean.b==target.mean&sd.b==target.sd|iteration==max.iter)
				break
				}	
		if(iteration==max.iter)	cat("Maximum Number of Iterations Reached Without A Solution. Try Again! (see manual for known convergence issues)","\b")
		else{
		write.table(b,file="MorseGen Results.txt",row.names=FALSE)
		sim.perf<-data.frame(rbind(c(target.mean,round(mean(b),3),target.sd,round(sd(b),3),iteration)))
		colnames(sim.perf)<-c("Target Mean","Sample Mean","Target SD","Sample SD","Iterations")
		cat("Summary of the Simulation Results (note: raw data has been exported to the file 'MorseGen Results.txt' in your working directory","\b")
		cat(")","\n")
		list(DATA=b,PERFORMANCE=sim.perf)
			}
}

