% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HC.bet.wit.ratio.R
\name{cluster.ratio_HC}
\alias{cluster.ratio_HC}
\title{Ratio of within and between tropical measures for tropical hierarchical
clusters}
\usage{
cluster.ratio_HC(A, V, method = mean)
}
\arguments{
\item{A}{matrix of tropical points; rows are points}

\item{V}{list of clusters where each cluster is defined as a matrix}

\item{method}{method to use for within cluster measure; mean or max}
}
\value{
vector of ratios for each cluster
}
\description{
Ratio of within and between cluster tropical measures for a set hierarchical
clusters
}
\examples{
har<-rbind(Sim_points[1:20,],Sim_points[51:70,])

V<-Tropical.HC.AGNES(har, method=mean)
inds<-V[[2]][[38]]
cluster.ratio_HC(har,inds,method=mean)
}
\references{
David Barnhill, Ruriko Yoshida (2023). Clustering Methods Over
the Tropically Convex Sets.
}
\author{
David Barnhill \email{david.barnhill@nps.edu}
}
