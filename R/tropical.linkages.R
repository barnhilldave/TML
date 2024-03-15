#' Tropical linkage measures for tropical hierarchical clustering
#'
#' These functions represent three possible linkage measures, average, maximum,
#' and minimum, measured in terms of the tropical distance. These are tropical
#' analogues to linkages in Euclidean hierarchical clustering.
#'
#' @param D1 matrix of points defining a tropical polytope; rows are the
#'   tropical points
#' @param D2 matrix of points assigned to a cluster
#' @return value of the associated linkage method
#' @references David Barnhill, Ruriko Yoshida (2023). Clustering Methods Over
#'   the Tropically Convex Sets.
#' @author Ruriko Yoshida \email{ryoshida@@nps.edu}
#' @noRd


tropical.complete.linkage <- function(D1, D2){
  ## D1 is a set of vertices for a tropical polytope (raws are obs)
  ## D2 is a set of observations of one cluster
  x.star <- 0 ## max distance
  d1 <- dim(D1)
  d2 <- dim(D2)
  for(i in 1:d2[1]){
    x <- trop.dist(project.pi(D1, D2[i, ]), D2[i, ])
    if(x > x.star)
      x.star <- x
  }
  for(i in 1:d1[1]){
    x <- trop.dist(project.pi(D2, D1[i, ]), D1[i, ])
    if(x > x.star)
      x.star <- x
  }
  return(x.star)
}

tropical.minimum.linkage <- function(D1, D2){
  ## D1 is a set of vertices for a tropical polytope (raws are obs)
  ## D2 is a set of observations of one cluster
  x.star <-trop.dist(project.pi(D1, D2[1, ]), D2[1, ]) ## max distance
  d1 <- dim(D1)
  d2 <- dim(D2)
  for(i in 1:d2[1]){
    x <- trop.dist(project.pi(D1, D2[i, ]), D2[i, ])
    if(x < x.star)
      x.star <- x
  }
  for(i in 1:d1[1]){
    x <- trop.dist(project.pi(D2, D1[i, ]), D1[i, ])
    if(x < x.star)
      x.star <- x
  }
  return(x.star)
}

tropical.average.linkage <- function(D1, D2){
  x.star <- 0 ## max distance
  d1 <- dim(D1)
  d2 <- dim(D2)
  x <- rep(0, (d1[1] + d2[1]))
  for(i in 1:d2[1]){
    x[i] <- trop.dist(project.pi(D1, D2[i, ]), D2[i, ])
  }
  for(i in 1:d1[1]){
    x[i + d2[1]] <- trop.dist(project.pi(D2, D1[i, ]), D1[i, ])
  }
  return(mean(x))
}
