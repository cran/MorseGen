cor.gen <-
function(num.subj, x.mean, x.sd, y.mean, y.sd, correlation, data.dec=2){
	x<-rnorm(num.subj)
	y<-rnorm(num.subj)
	xy.res<-resid(lm(y~x))
	x.z<-scale(x)
	res.z<-scale(xy.res)
	z<-correlation*x.z+sqrt(1-correlation^2)*res.z
	Variable.X<-x.mean+x.sd*x.z
	Variable.Y<-y.mean+y.sd*z
	data<-round(data.frame(Variable.X,Variable.Y),data.dec)
	sim.perf<-data.frame(rbind(c(apply(Variable.X,2,mean),apply(Variable.X,2,sd),apply(Variable.Y,2,mean),apply(Variable.Y,2,sd),cor(Variable.X,Variable.Y))))
	colnames(sim.perf)<-c("Sample X Mean","  Sample X SD","  Sample Y Mean","  Sample Y SD","  Sample Correlation")
	targets<-data.frame(rbind(c(x.mean,x.sd,y.mean,y.sd,correlation)))
	colnames(targets)<-c("Target X Mean","  Target X SD","  Target Y Mean","  Target Y SD","  Target Correlation")
	cat("Summary of the Simulation Results (note: raw data has been exported to the file 'MorseGen Correlation Results.txt' in your working directory","\n")
	write.table(data,file="MorseGen Correlation Results.txt",row.names=FALSE,sep="\t")
	list(DATA=data, TARGETS=targets, RESULTS=sim.perf)
}