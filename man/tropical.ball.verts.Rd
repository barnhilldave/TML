% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tropical.ball.verts.R
\name{trop.bal.vert}
\alias{trop.bal.vert}
\alias{trop.bal.all_vert}
\title{Calculate the minimum or entire generating vertex set of a tropical ball using a max- or min-plus algebra}
\usage{
trop.bal.vert(x, d, tadd = max)

trop.bal.all_vert(x, d)
}
\arguments{
\item{x}{matrix where each row is a point defining a tropical polytope}

\item{d}{radius of the tropical ball in terms of tropical distance}

\item{tadd}{function; max indicates max-plus addition, min indicates min-plus addition, 'all' indicates all vertices. Defaults to 'max'}
}
\value{
matrix of normalized tropical points defining the tropical ball. Rows are the points
}
\description{
This function calculates the coordinates of the minimum or entire vertex set of a tropical ball in terms of either a max- or min-plus algebra for a given a center point
}
\examples{
x <-c(0,3,7,5)
d <- 2
trop.bal.vert(x,d)
trop.bal.vert(x,d,tadd=min)
trop.bal.all_vert(x,d)
}
\references{
Barnhill, David, Ruriko Yoshida and Keiji Miura (2023). Maximum Inscribed and Minimum Enclosing Tropical Balls of Tropical Polytopes and Applications to Volume Estimation and Uniform Sampling.
}
\author{
David Barnhill \email{david.barnhill@nps.edu}
}
