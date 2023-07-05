#' Agglomerative (AGNES) tropical hierarchical clustering
#'
#' This function performs agglomerative (AGNES) hierarchical clustering over the space of ultrametrics defining the space of equidistant trees
#'
#' @param D matrix of points defining a tropical polytope.  Rows are the tropical points
#' @param method linkage method: "average", "min", or "max"
#' @return list of distances in when merges occur; list of indices of points in each cluster
#' @author David Barnhill \email{david.barnhill@@nps.edu}
#' @references David Barnhill, Ruriko Yoshida (2023). Clustering Methods Over the Tropically Convex Sets.
#' @export
#' @examples
#' \dontrun{P <-Sim_points
#' Tropical.HC.AGNES(P, method="average")
#' }

Tropical.HC.AGNES <- function(D, method="average"){
  ### method is one of "average", "min", "max"
  d <- dim(D)
  distance <- rep(0, d[1])
  index.list <- list()
  index <- list()
  for(i in 1:d[1])
    index[[i]] <- c(i)
  index.list[[1]] <- index
  for(i in 2:d[1]){
    D1 <- make.list.matrices(D, index)
    best.pair <- finding.pair(D1, method)
    #print(best.pair[[1]])
    index[[best.pair[[1]][1]]] <- cbind(index[[best.pair[[1]][1]]], index[[best.pair[[1]][2]]])
    index <- index[-best.pair[[1]][2]]
    #print(index[best.pair[[1]][2]])
    index.list[[i]] <- index
    distance[i] <- best.pair[[2]]
  }
  return(list(distance, index.list))
}
