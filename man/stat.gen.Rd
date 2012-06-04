\name{stat.gen}
\alias{stat.gen}
\title{
stat.gen
}
\description{
stat.gen is a function to generate raw data based on user-specified descriptive statistics (n, mean, & standard deviation). A sample of N scores will be drawn from an appropriate distribution (normal or exponential) and scaled to match the desired mean and standard deviation. The raw data will be written to a text file in the user's working directory.
}
\usage{
stat.gen(num.subj, target.mean, target.sd, data.dec = 2, neg.data = TRUE, x.name = "Variable X")
}
\arguments{
  \item{num.subj}{Specifies the intended sample size (n)}
  \item{target.mean}{Specifies the intended sample mean}
  \item{target.sd}{Specifies the intended sample standard deviation}
  \item{data.dec}{Specifies the number of decimal places in the sample data}
  \item{neg.data}{Specifies whether the sample data can include negative values}
  \item{x.name}{Specifies the name of the variable}
}
\value{
The values returned are the raw data which is printed to the R console as well as saved to a text file to be imported to other programs or back into R. The R console will also display performance data including the descriptive statistics and the number of iterations needed to find a solution.
}
\author{
Brendan Morse
}
\note{
For most applications, stat.gen will be able to generate a succesful dataset. However, recognize that the raw data may look more or less like you would expect the original raw data to look given that an infinite combination of values could emulate any of the supplied descriptive statistics. Modifying the data.dec command and/or the neg.data command can help the generated raw data look more realistic.

If neg.data=FALSE (i.e., the raw data cannot be negative), and the target mean is near zero (i.e., the sample is positively skewed with a floor effect at 0), stat.gen will draw scores from an exponential distribution to facilitate finding a solution.
}
\examples{
# In a study published in Body Image on the 
# effects of massage on state (temporary) body 
# image, Dunigan, King, & Morse (2011) that there 
# were no significant differences in body mass 
# index scores (BMI) between the control (n=26, 
# M=25.26, SD=6.51) and experimental (n=23, M=23.30, 
# SD=5.83) conditions as a check for potential confounds.
# We can use MorseGen to generate a sample of raw scores 
# to match these statistics and use this data as a 
# teaching example for an independent samples t-test in an 
# introductory statistics course. 

# Generate the control group data.
# I set non.neg=TRUE because the supplied descriptive 
# statistics could return a negative value (0 is within 4 standard 
# deviations from each mean) and BMI cannot be negative.
# Note that the output file "MorseGen Sample Results.txt" will 
# overwrite the data with each run of the stat.gen function so 
# the scores must be copied out or saved as a different file name 
# after each run.

stat.gen(26, 25.26, 6.51, data.dec=2, neg.data=FALSE, x.name="Control")

#To generate the experimental group data

stat.gen(23, 23.30, 5.83, data.dec=2, neg.data=FALSE, x.name="Massage")
}
\keyword{datagen}