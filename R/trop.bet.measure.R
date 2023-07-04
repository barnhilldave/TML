#' Tropical cluster betweeness measure for two clusters
#'
#' This function calculates a betweenness measure between a set of clusters
#' @param D1 matrix of tropical points; rows are points
#' @param D2 matrix of tropical points; rows are points
#' @return betweenness cluster measure
#' @references David Barnhill, Ruriko Yoshida (2023). Clustering Methods Over the Tropically Convex Sets.
#' @author David Barnhill \email{david.barnhill@@nps.edu}
#' @noRd

trop_bet_dist<-function(D1,D2){
  dstar<-0
  for(i in 1:nrow(D2)){
    pD2<-project_pi(D1,D2[i,])
    td<-trop.dist(D2[i,],pD2)
    dstar<-dstar+td
  }
  return(dstar/nrow(D2))
}

pairws.distances <- function(D1){
  ## D1 is a matrix.  the rows are observations.
  d1 <- dim(D1)
  D <- matrix(rep(0, d1[1]*d1[1]), d1[1], d1[1])
  for(i in 1:d1[1])
    for(j in 1:d1[1])
      D[i, j] <- trop.dist(D1[i, ], D1[j, ])
  return(D)
}
