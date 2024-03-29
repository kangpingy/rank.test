% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my_package.R
\name{my_rank}
\alias{my_rank}
\title{my_rank}
\usage{
my_rank(x)
}
\arguments{
\item{x}{A numeric vector}
}
\value{
A vector with average rank of each element of input x
}
\description{
Simulate the  function 'rank()' in the 'base' package to give the rank of each element in the vector
}
\examples{
my_rank(rnorm(100))
my_rank(c(4,3,3,8,-1,4,7,5,4,4,3,0))   ## The average rank of ties would return correspondingly to the tied elements.

}
