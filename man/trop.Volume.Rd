% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Trop.Volume.R
\name{trop.Volume}
\alias{trop.Volume}
\title{Estimate the volume of a tropical polytope}
\usage{
trop.Volume(B, P, x0, s, I, r)
}
\arguments{
\item{B}{matrix of points defining a minimum enclosing ball for a polytope P; rows are the points}

\item{P}{matrix of points defining a tropical polytope; rows are the points}

\item{x0}{initial point used for the HAR sampler}

\item{s}{number of points to sample from the minimum enclosing ball}

\item{I}{number of iterations for the HAR sampler}

\item{r}{radius of the minimum enclosing tropical ball}
}
\value{
list containing ratio of points falling in P; volume of the tropical ball; volume estimate of P
}
\description{
This function uses tropical HAR with a uniform target distribution to estimate the volume of a tropical polytope
}
\examples{
P <-matrix(c(0,0,0,0,3,1,0,2,5),3,3,TRUE)
BR<-min_enc.ball(P)
B<-trop.bal.vert(BR[[1]],BR[[2]])
x0<-c(0,1.5,.4)
S<-200
I<-50
R<-BR[[2]]
trop.Volume(B,P,x0,S,I,R)
}
\references{
Barnhill, David, Ruriko Yoshida and Keiji Miura (2023). Maximum Inscribed and Minimum Enclosing Tropical Balls of Tropical Polytopes and Applications to Volume Estimation and Uniform Sampling.
}
\author{
David Barnhill \email{david.barnhill@nps.edu}
}
