#' Gaussian-like sampling using Metropolis filter
#'
#' This function samples points on a tropical line segment about a location parameter for a given scale parameter defined in terms of tropical distance
#'
#' @param D matrix of vertices of a tropical polytope; each row is a vertex
#' @param x0 initial point for sampler, numeric vector
#' @param mu location parameter; numeric vector
#' @param s scale parameter; scalar
#' @param n number of points to sample
#' @param I states in Markov chain
#' @return matrix of n sampled points where each point is a row
#' @author David Barnhill \email{david.barnhill@@nps.edu}
#' @references Yoshida, Ruriko, Keiji Miura and David Barnhill (2022). Hit and Run Sampling from Tropically Convex Sets.
#' @name tropical.Gaussian.MH
#' @examples
#' D <-matrix(c(0,0,0,0,10,0,0,0,10),3,3,TRUE)
#' x0 <- c(0,0,0)
#' mu<-c(0,5,5)
#' s<-1
#' n<-10
#' trop.Gaussian.MH(D, x0, mu, s, n, I=50)
#' trop.Gaussian.MH.square(D, x0,mu, s, n, I=50)

#' @export
#' @rdname tropical.Gaussian.MH
trop.Gaussian.MH <- function(D, x0, mu, s, n, I=50){
  d <- dim(D)
  mu <- normaliz.vector(mu)
  N <- matrix(rep(0, n*d[2]), n, d[2])
  k <- ncol(D)-1
  i <- 1
  while(i <= n){
    x1 <- TropicalPolytope.extrapolation.HAR(D, x0, I = I)
    x1 <- normaliz.vector(x1)
    r<-exp(-trop.dist(mu, x1)/s)/exp(-trop.dist(mu, x0)/s)
    if(runif(1) < r){
      x0 <- x1
      N[i, ] <- x0
      N[i, ] <-  normaliz.vector(N[i, ])
      i <- i + 1
    }
  }
  return(N)
}

#' @export
#' @rdname tropical.Gaussian.MH
trop.Gaussian.MH.square <- function(D, x0, mu, s, n, I=50){
  d <- dim(D)
  mu <- normaliz.vector(mu)
  N <- matrix(rep(0, n*d[2]), n, d[2])
  k <- ncol(D)-1
  i <- 1
  while(i <= n){
    x1 <- TropicalPolytope.extrapolation.HAR(D, x0, I = I)
    x1 <- normaliz.vector(x1)
    r <- exp(-trop.dist(mu, x1)^2/s)/exp(-trop.dist(mu, x0)^2/s)
    if(runif(1) < r){
      x0 <- x1
      N[i, ] <- x0
      N[i, ] <-  normaliz.vector(N[i, ])
      i <- i + 1
    }
  }
  return(N)
}
