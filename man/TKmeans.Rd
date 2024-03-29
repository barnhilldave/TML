% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trop.k.means.R
\name{TKmeans}
\alias{TKmeans}
\title{K-means clustering over the tropical projective torus}
\usage{
TKmeans(A, C, M)
}
\arguments{
\item{A}{matrix of points defining a tropical polytope; rows are the tropical
points}

\item{C}{number of clusters}

\item{M}{maximum number of iterations of algorithm to find cluster centroids}
}
\value{
list with matrix of observation classified by centroid; matrix of
centroid coordinates; number of iterations used
}
\description{
This function performs k-means clustering over the tropical projective torus
}
\examples{
P <-Sim_points
C<-3
M<-10
res<-TKmeans(P,C,M)
try<-res[[1]]
cen<-res[[2]]
plot(try[,2],try[,3],col=try[,4],asp=1)
plot(try[,2],try[,3],col=try[,4],asp=1,xlab='x2',ylab='x3')
points(cen[,2],cen[,3],col=c('purple','hotpink','orange'),pch=19)
}
\references{
David Barnhill, Ruriko Yoshida (2023). Clustering Methods Over
the Tropically Convex Sets.
}
\author{
David Barnhill \email{david.barnhill@nps.edu}
}
