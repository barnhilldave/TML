% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trop.clus.bet.KM.R
\name{over_bet_KM}
\alias{over_bet_KM}
\title{Tropical cluster betweeness measure for a each of a set of k-means derived
set of clusters}
\usage{
over_bet_KM(A, C)
}
\arguments{
\item{A}{matrix of tropical points; rows are points with the last column
representing a numbered cluster assignment}

\item{C}{number of clusters}
}
\value{
betweenness cluster measure
}
\description{
This function calculates an overall betweenness measure between a set of
clusters derived from tropical k-means clustering
}
\examples{

hars<-Sim_points
cls<-c(rep(1,50),rep(2,50),rep(3,50))
cl_pt<-cbind(hars,cls)

C<-3
over_bet_KM(cl_pt,C)
}
\references{
David Barnhill, Ruriko Yoshida (2023). Clustering Methods Over
the Tropically Convex Sets.
}
\author{
David Barnhill \email{david.barnhill@nps.edu}
}
