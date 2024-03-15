#' Calculate the tropical distance to a max-tropical hyperplane
#'
#' Calculate the tropical distance to a max-tropical hyperplane
#'
#' @param O normal vector of a tropical hyperplane; numeric vector
#' @param x0 point of interest; numeric vector
#' @param add string; 'max' indicates max-plus addition, 'min' indicates
#'   min-plus addition. Defaults to 'max'
#' @return tropical distance to max-plus tropical hyperplane
#' @author David Barnhill \email{david.barnhill@@nps.edu}
#' @export
#' @examples
#' O <-c(0,-1,-1)
#' x0 <- c(0,-2,-8)
#' trop.dist.hyp(O,x0)
#' trop.dist.hyp(O,x0,add='min')

#### Calculate distance from a point to a max hyperplane ####
trop.dist.hyp<-function(O,x0,add='max'){
  if (add=='max'){
    x<-O+x0
    x_prime<-x[order(x,decreasing = TRUE)]
    dist<-x_prime[1]-x_prime[2]
  }
  if(add=='min'){
    x<-O+x0
    x_prime<-x[order(x,decreasing = FALSE)]
    dist<-x_prime[2]-x_prime[1]
  }
  return(dist)
}


