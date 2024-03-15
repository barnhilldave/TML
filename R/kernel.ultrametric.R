#' Kernel Function for Ultrametrics
#'
#' This function calculates a non-parametric density for ultrametrics using a
#' Gaussian kernel in conjunction with a tropical distance
#'
#' @param x numeric vector representing a sampled ultrametric
#' @param mu numeric vector representing the centroid ultrametric
#' @param s bandwidth parameter based on tropical distance
#' @return density value based on sampled point x
#' @author Ruriko Yoshida \email{ryoshida@@nps.edu}
#' @noRd

kernel.ultrametric <- function(x, mu, s){
  return(exp(-(trop.dist(mu, x)/s)))
}
