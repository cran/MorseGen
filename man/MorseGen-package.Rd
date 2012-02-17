\name{MorseGen-package}
\Rdversion{1.1}
\alias{MorseGen-package}
\alias{MorseGen}
\docType{package}
\title{
Simple raw data generator based on user-supplied summary statistics (n, mean, & standard deviation)
}
\description{
MorseGen is a program for generating raw data scores based on user-supplied summary statistics (n, mean, & standard deviation). Samples based on the supplied statistics are drawn from a normal distribution until a sample of raw scores (with size = n) is found that recreates the specified mean and standard deviation (within a tolerance of .01). Intended uses include recreating a set of raw data for students to analyze that will replicate a published study.
}
\details{
\tabular{ll}{
Package: \tab MorseGen\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2012-02-17\cr
License: \tab GPL\cr
LazyLoad: \tab yes\cr
}
To use MorseGen, you must enter three required pieces of information: the sample size (num.subj), the mean that you want the data to emulate (target.mean), and the standard deviation that you want the data to emulate (target.sd). These values are added as numerical values in the stat.gen function and must all be specified. You can also enter three optional commands after these parameters to adjust the number of decimal points in the raw data (data.dec=), whether the raw data should all be positive values (non.neg=), and you can reset the maximum number of iterations for the simulation (max.iter=). The default values are 2 decimal points in the raw data (data.dec=2), negative scores are allowed (non.neg=FALSE), and 100,000 iterations before a scheduled timeout (max.iter=100000). BE SURE TO SEE THE MANUAL FOR KNOWN CONVERGENCE PROBLEMS ASSOCIATED WITH THESE OPTIONAL COMMANDS.
}
\author{
Brendan J. Morse

Maintainer: Brendan Morse <bmorse@bridgew.edu>
}
\keyword{ package
datagen
}
\examples{
#Generate a sample of 10 scores with a mean equal to 18.4 and a standard deviation of 3.2.
stat.gen(10,18.4,3.2) 
#Generate a sample of 20 scores with a mean equal to 9.1, and a standard deviation of 3.2. Additionally, the raw data must be in whole-unit integers, must be positive, and the simulation will time out at 50,000 iterations if no solution is found.
stat.gen(10,18.4,3.2,data.dec=0,non.neg=TRUE,max.iter=50000) 
}
