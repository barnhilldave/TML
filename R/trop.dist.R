#'Compute the tropical distance
#'
#'This function computes the tropical distance between two points in the
#'tropical projective torus
#'
#'@param D1 point in the tropical projective torus
#'@param D2 point in the tropical projective torus
#'@return tropical distance between D1 and D2
#'@author Ruriko Yoshida \email{ryoshida@@nps.edu}
#'@export
#' @examples
#'D1 <-c(0,4,2)
#'D2 <- c(0,7,-1)
#'trop.dist(D1, D2)
#'

trop.dist <- function(D1, D2){
  return(max(D2 - D1) - min(D2 - D1))
}
