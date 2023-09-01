#' Constructs the dissimilarity matrix for a set of ultrametrics
#'
#' Constructs the dissimilarity matrix based on the tropical distance between points in a dataset
#'
#' @param D1 matrix of ultrametrics
#' @param D2 matrix of ultrametrics
#' @return matrix; dissimilarity matrix showing the tropical pairwise distance between each point
#' @author Ruriko Yoshida \email{ryoshida@@nps.edu}
#' @references Weyenberg, G., Huggins, P., Schardl, C., Howe, D. K., & Yoshida, R. (2014). kdetrees: Nonparametric Estimation of Phylogenetic Tree Distributions. In Bioinformatics.
#' @references Yoshida, Ruriko, David Barnhill, Keiji Miura and Daniel Howe (2022). Tropical Density Estimation of Phylogenetic Trees.
#' @references \url{https://github.com/grady/kdetrees/blob/master/R/dist.diss.R}
#' @export
#' @examples
#' \donttest{
#' T1<-Sim_Trees1
#' T2<-Sim_Trees2
#' D <- rbind(T1, T2[1,])
#' pw.trop.dist(D, D)
#' }
pw.trop.dist <- function(D1, D2){
  d1 <- dim(D1)
  d2 <- dim(D2)
  D <- matrix(rep(100000, d1[1]*d2[1]), d1[1], d2[1])
  for(i in 1:d1[1])
    for(j in 1:d2[1])
      D[i, j] <- trop.dist(D1[i, ], D2[j, ])
  return(D)
}
