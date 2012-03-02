\name{cor.gen}
\alias{cor.gen}
\title{
cor.gen
}
\description{
cor.gen is a function to generate two vectors of N scores drawn from a normal distribution with the user-specified means, standard deviations, and  correlation. The raw data will be written to a text file in the user's working directory.
}
\usage{
cor.gen(num.subj, x.mean, x.sd, y.mean, y.sd, correlation, data.dec = 2)
}
\arguments{
  \item{num.subj}{Specifies the intended sample size (n)}
  \item{x.mean}{Specifies the intended mean for variable X}
  \item{x.sd}{Specifies the intended standard deviation for variable X}
  \item{y.mean}{Specifies the intended mean for variable Y}
  \item{y.sd}{Specifies the intended standard deviation for variable Y}
  \item{correlation}{Specifies the indended correlation between X & Y}
  \item{data.dec}{Specifies the number of decimal places in the sample data}
}
\value{
The values returned are the raw data which is printed to the R console as well as saved to a text file to be imported to other programs or back into R. The R console will also display performance data including the descriptive statistics.
}
\author{
Brendan J. Morse
}
\examples{
# In a study published in Body Image on the effects 
# of massage on state (temporary) body image, Dunigan, 
# King, & Morse (2011) reported that their observed 
# correlation between body mass index scores (BMI) and 
# body image state scores (BISS) to be -.56 for their 
# 49 participants. We can use cor.gen to generate a sample 
# of raw scores to match these statistics and use this 
# data as a teaching example for Pearson correlations in 
# an introductory statistics course. 

# To generate the data

cor.gen(49, 24.38, 6.22, 5.16, 1.47, -.56) 
}
\keyword{datagen}