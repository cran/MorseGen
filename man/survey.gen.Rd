\name{survey.gen}
\alias{survey.gen}
\title{
survey.gen
}
\description{
survey.gen is a function to create a NxK matrix of scores representing scale responses from N individuals for a scale with K items and a specified number of response options (from 3 to 10) for each item. These items are meant to simulate Likert-type response scales (e.g., strongly disagree to strongly agree). The raw data will be written to a text file in the user's working directory.
}
\usage{
survey.gen(num.subj, num.item, num.resp)
}
\arguments{
  \item{num.subj}{Specifies the intended sample size (n)}
  \item{num.item}{Specifies the intended number of items in the scale}
  \item{num.resp}{Specifies the intended number of response options for each item}
}
\value{
The values returned are the raw data which is printed to the R console as well as saved to a text file to be imported to other programs or back into R. The R console will also display the reliability (internal consistency) of the items.
}
\author{
Brendan J. morse
}
\note{
The scale items are simulated to include a latent factor using the Graded Response Model.

Possible response options are 3, 4, 5, 6, 7, 8, 9, or 10.
}
\examples{
# Generate a set of scale scores from 100 individuals 
# who responded a 15-item scale in which items have a 
# 5-point Likert-type response format

survey.gen(100,15,5)
}
\keyword{datagen}