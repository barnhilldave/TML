#' Tropical within-cluster measure
#'
#' This function calculates a within cluster measure by measuring the pairwise
#' tropical distance between points in the cluster.
#' @param D1 matrix of tropical points; rows are points
#' @param method function; metric to measure; mean is the average pairwise tropical
#'   distance; max is the maximum pairwise tropical distance
#' @return within cluster measure
#' @references David Barnhill, Ruriko Yoshida (2023). Clustering Methods Over
#'   the Tropically Convex Sets.
#' @author David Barnhill \email{david.barnhill@@nps.edu}
#' @export
#' @examples
#' D<-Sim_points
#' avg.m<-trop_wi_dist(D, method=mean)
#' max.m<-trop_wi_dist(D, method=max)

trop_wi_dist<-function(D1,method=mean){
  md<-method(lower_tri(pairws.distances(D1)))
  return(md)
}

