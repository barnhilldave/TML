#' Centroid-based sampling using Metropolis filter
#'
#' This function samples points on a tropical line segment about a location
#' parameter for a given scale parameter defined in terms of tropical distance
#'
#' @param D matrix of vertices of a tropical polytope; each row is a vertex
#' @param x0 initial point for sampler, numeric vector
#' @param m location parameter; numeric vector
#' @param s scale parameter; scalar
#' @param n number of points to sample
#' @param I states in Markov chain
#' @param add string; 'max' indicates max-plus addition, 'min' indicates
#'   min-plus addition. Defaults to 'max'
#' @return matrix of n sampled points where each point is a row
#' @author David Barnhill \email{david.barnhill@@nps.edu}
#' @references Yoshida, Ruriko, Keiji Miura and David Barnhill (2022). Hit and
#'   Run Sampling from Tropically Convex Sets.
#' @name tropical.centroid.MH
#' @examples
#' D1 <-matrix(c(0,0,0,0,10,0,0,0,10),3,3,TRUE)
#' D2 <-matrix(c(0,10,10,0,10,0,0,0,10),3,3,TRUE)
#' x0 <- c(0,0,0)
#' m1<-c(0,5,5)
#' m2<-c(0,-1,1)
#' s<-1
#' n<-10
#' trop.centroid.MH(D1, x0, m1, s, n, I=50)
#' trop.centroid.MH.square(D1, x0,m1, s, n, I=50)
#' trop.centroid.MH(D2, x0, m1, s, n, I=50,add='min')
#' trop.centroid.MH.square(D2, x0,m2, s, n, I=50,add='min')

#' @export
#' @rdname tropical.centroid.MH
trop.centroid.MH <- function(D, x0, m, s, n, I=50, add='max'){
  d <- dim(D)
  m <- normaliz.vector(m)
  N <- matrix(rep(0, n*d[2]), n, d[2])
  k <- ncol(D)-1
  i <- 1
  if(add=='max'){
  while(i <= n){
    x1 <- VE.HAR(D, x0, I = I)
    x1 <- normaliz.vector(x1)
    r<-exp(-trop.dist(m, x1)/s)/exp(-trop.dist(m, x0)/s)
    if(runif(1) < r){
      x0 <- x1
      N[i, ] <- x0
      N[i, ] <-  normaliz.vector(N[i, ])
      i <- i + 1
    }
  }
  }
  if(add=='min'){
    while(i <= n){
      x1 <- VE.HAR(D, x0, I = I,add='min')
      x1 <- normaliz.vector(x1)
      r<-exp(-trop.dist(m, x1)/s)/exp(-trop.dist(m, x0)/s)
      if(runif(1) < r){
        x0 <- x1
        N[i, ] <- x0
        N[i, ] <-  normaliz.vector(N[i, ])
        i <- i + 1
      }
    }
  }
  return(N)
}

#' @export
#' @rdname tropical.centroid.MH
trop.centroid.MH.square <- function(D, x0, m, s, n, I=50, add='max'){
  d <- dim(D)
  m <- normaliz.vector(m)
  N <- matrix(rep(0, n*d[2]), n, d[2])
  k <- ncol(D)-1
  i <- 1
  if(add=='max'){
    while(i <= n){
      x1 <- VE.HAR(D, x0, I = I)
      x1 <- normaliz.vector(x1)
      r <- exp(-trop.dist(m, x1)^2/s)/exp(-trop.dist(m, x0)^2/s)
      if(runif(1) < r){
        x0 <- x1
        N[i, ] <- x0
        N[i, ] <-  normaliz.vector(N[i, ])
        i <- i + 1
      }
    }
  }
  if(add=='min'){
    while(i <= n){
      x1 <- VE.HAR(D, x0, I = I, add = add)
      x1 <- normaliz.vector(x1)
      r <- exp(-trop.dist(m, x1)^2/s)/exp(-trop.dist(m, x0)^2/s)
      if(runif(1) < r){
        x0 <- x1
        N[i, ] <- x0
        N[i, ] <-  normaliz.vector(N[i, ])
        i <- i + 1
      }
    }
  }
  return(N)
}

