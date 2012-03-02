\name{MorseGen-package}
\alias{MorseGen-package}
\alias{MorseGen}
\docType{package}
\title{
Simple raw data generator based on user-specified summary statistics
}
\description{
MorseGen is a program for generating raw data based on user-specified summary (descriptive) statistics. Samples based on the supplied statistics are drawn from a normal distribution (or, in some cases, an exponential distribution) and scaled to match the desired descriptive statistics. Intended uses include creating raw data that fits desired characteristics or to replicate the results in a published study.
}
\details{
\tabular{ll}{
Package: \tab MorseGen\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2012-03-02\cr
License: \tab GPL 2\cr
LazyLoad: \tab yes\cr
}
MorseGen includes three different functions for generating data based on user-specified summary (descriptive) statistics. The stat.gen function will create a single vector of N scores with a specified mean and standard deviation. The cor.gen function will create two vectors of N scores with the specified means and standard deviations as well as a specified correlation. The survey.gen function will create a NxK matrix of scores that represent scale responses from N individuals for a scale with K items and a specified number of response options (from 3 to 10) for each item.
}
\author{
Brendan J. Morse

Maintainer: Brendan Morse <bmorse@bridgew.edu>
}
\keyword{ package 
datagen
}
\examples{
# Generate a sample of 10 scores 
# with a mean equal to 18.4 and a 
# standard deviation of 3.2.

stat.gen(10,18.4,3.2)

# Generate a sample of 20 scores with 
# a mean equal to 9.1, and a standard 
# deviation of 3.2. Additionally, the 
# raw data must be in positive, whole-
# unit integers.

stat.gen(10,18.4,3.2,data.dec=0,non.neg=TRUE)

# Generate two variables (with means 5.65 
# & 100.2 and standard deviations 2.13 & 16.8 
# respectively) from 15 individuals that are 
# correlated at r = -.68

cor.gen(15,5.65,2.13,100.2,16.8,-.68)

# Generate a set of scale scores from 100 individuals 
# who responded a 15-item scale in which items have a 
# 5-point Likert-type response format

survey.gen(100,15,5)
}
