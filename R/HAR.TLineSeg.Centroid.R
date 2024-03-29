#'Gaussian-like Sampling on a max- or min-plus tropical line segment
#'
#'This function samples points on a tropical line segment about a location
#'parameter for a given scale parameter defined in terms of tropical distance
#'
#'@param D1 point in the tropical projective torus
#'@param D2 point in the tropical projective torus
#'@param m location parameter
#'@param s scale parameter
#'@param add string; 'max' indicates max-plus addition, 'min' indicates min-plus
#'  addition. Defaults to 'max'
#'@return point on the line segment defined by D1 and D2 sampled about mu
#'@author David Barnhill \email{david.barnhill@@nps.edu}
#'@export
#' @examples
#'D1 <-c(0,4,2)
#'D2 <- c(0,7,-1)
#'m<-c(0,7,2)
#'s<-1
#'HAR.TLineSeg.centroid(D1, D2,m,s)
#'HAR.TLineSeg.centroid(D1, D2,m,s,add='min')

HAR.TLineSeg.centroid <- function(D1, D2, m, s, add='max'){
  d <- length(D1)
  D<-rbind(D1,D2)
  if(length(D1) != length(D2))
    warning("dimension is wrong!")
  if(add=='max'){
  proj_mu<-project.pi(D,m)
  l0<-trop.dist(D1,proj_mu)+min(D1-D2)
  pro_true<-FALSE
  while(pro_true==FALSE){
    l <- rnorm(1, mean = 0, sd=s)
    x_p <- rep(0, d)
    for(i in 1:d){
      x_p[i] <-max(((l0+l) + D2[i]), D1[i])
    }
    pro_x<-project.pi(D,x_p)
    dist_pro<-trop.dist(x_p,pro_x)
    if(dist_pro<=1e-8){
      x1<-normaliz.vector(x_p)
      pro_true<-TRUE
    }
  }
  }
  if(add=='min'){
    proj_mu<-project.pi(D,m,add='min')
    l0<-trop.dist(D1,proj_mu)+min(D1-D2)
    pro_true<-FALSE
    while(pro_true==FALSE){
      l <- rnorm(1, mean = 0, sd=s)
      x_p <- rep(0, d)
      for(i in 1:d){
        x_p[i] <-min(((l0+l) + D2[i]), D1[i])
      }
      pro_x<-project.pi(D,x_p,add='min')
      dist_pro<-trop.dist(x_p,pro_x)
      if(dist_pro<=1e-8){
        x1<-normaliz.vector(x_p)
        pro_true<-TRUE
      }
    }
  }
  return(x1)
}

