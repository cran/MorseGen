\name{stat.gen}
\Rdversion{1.1}
\alias{stat.gen}
\title{
stat.gen
}
\description{
Stat.gen is a function to generate raw data scores based on user-supplied summary statistics (n, mean, & standard deviation). Samples based on the supplied statistics are drawn from a normal distribution until a sample of raw scores (with size = n) is found that recreates the specified mean and standard deviation (within a tolerance of .01).
}
\usage{
stat.gen(num.subj, target.mean, target.sd, data.dec = 2, non.neg = FALSE, max.iter = 1e+05)
}
\arguments{
  \item{num.subj}{Specifies the intended sample size (n)}
  \item{target.mean}{Specifies the intended sample mean}
  \item{target.sd}{Specifies the intended sample standard deviation}
  \item{data.dec}{Specifies the number of decimal places in the sample data}
  \item{non.neg}{Specifies whether the sample data can include negative values}
  \item{max.iter}{Specifies the number of simulation iterations before a timeout if no solution is found}
}
\value{
The values returned are the raw data which is printed to the R console as well as saved to a text file to be imported to other programs. The R console will also display performance data including the descriptive statistics and the number of iterations needed to find a solution.
}
\author{
Brendan J. Morse
}
\note{
MorseGen will save the output file "MorseGen Results.txt" in the current working directory. To change this directory, use the setwd() command prior to simulating data.

KNOWN CONVERGENCE ISSUES WITH MorseGen:

For most applications, MorseGen will be able to generate a succesful dataset. However, recognize that the raw data may look more or less like you would expect the original raw data to look given that an infinite combination of values could emulate any of the supplied descriptive statistics. Modifying the data.dec command and/or the non.neg command can help the generated raw data look more realistic, however, it can create some convergence problems given that the scores are being sampled from a normal distribution.

1. As sample size (n) decreases, MorseGen will take longer to find an acceptable solution. This becomes exasperated as the SD increases due to the underrperesentation of true variability in small random samples.
2. When data.dec=0 (i.e., the raw data is in whole-unit integers) and the SD is small, MorseGen will take longer to find an acceptable solution.
--In the above two conditions, you may want to increase the maximum number of iterations.--
3. When data.dec=0 (i.e., the raw data is in whole-unit integers) you should supply no more than 2 decimal places for the target mean and standard deviation.
4. If non.neg=TRUE (i.e., the raw data cannot be negative), and the target mean is near zero (i.e., the sample is positively skewed with a floor effect at 0), MorseGen will not likely find an acceptable solution.
}
\examples{
#In a study published in Body Image on the effects of massage on state (temporary) body image, Dunigan, King, & Morse (2011) reported as a confound check that there were no significant differences in body mass index scores (BMI) between the control (n=26, M=25.26, SD=6.51) and experimental (n=23, M=23.30, SD=5.83). We can use MorseGen to generate a sample of raw scores to match these statistics and perhaps use this data as a teaching example for independent samples t-tests in an introductory statistics course. 

#To generate the control group data#
#I set non.neg=TRUE because the supplied descriptive statistics could return a negative value (0 is within 4 standard deviations from each mean) and BMI cannot be negative.
#Note that the output file "MorseGen Results.txt" will overwrite the data with each run of the stat.gen function so the scores must be copied out or saved as a different file name after each run.

stat.gen(26, 25.26, 6.51, non.neg=TRUE)

#To generate the experimental group data#

stat.gen(23, 23.30, 5.83, non.neg=TRUE) 

}
\keyword{datagen}
