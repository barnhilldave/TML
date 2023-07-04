#' Tropical principal component analysis (PCA) on over tropical projective torus
#'
#' This function conducts tropical PCA to find the best fit tropical triangle given data defined in the tropical projective torus.
#' It employs the vertex HAR with extrapolation sampler to sample points to determine the vertices of the tropical triangle.
#' @param S inital set of vertices for the tropical triangle
#' @param D matrix of data where each row is an observation in the tropical projective torus
#' @param V matrix of vertices defining a polytope encompassing D
#' @param I number of iterations to perform
#' @param k number of iterations for the HAR sampler
#' @return list with the sum of residuals
#' @references Page, Robert and others (2020), Tropical principal component analysis on the space of phylogenetic trees, Bioinformatics, Volume 36, Issue 17, Pages 4590–4598.
#' @references Yoshida, R., Zhang, L. & Zhang, X (2019). Tropical Principal Component Analysis and Its Application to Phylogenetics. Bull Math Biol 81, 568–597.
#' @author Ruriko Yoshida \email{ryoshida@@nps.edu}
#' @name tropical.PCA
#' @examples
#' \dontrun{
#' s <- 3 #number of vertices.  Here it is a tropical triangle
#'d <- 3 ## dimension
#'N <- 100 ## sample size
# Vertices of a bigger tropical polytope
#'V <- matrix(c(100, 0, 0, 0, 100, 0, 0, 0, 100, -100, 0, 0, 0, -100, 0, 0, 0, -100), 6, 3, TRUE)
#'D <- matrix(rep(0, N*d), N, d)
#'D[, 1] <- rnorm(N, mean = 5, sd = 5)
#'D[, 2] <- rnorm(N, mean = -5, sd = 5)
#'D[, 3] <- rnorm(N, mean = 0, sd = 5)

#' index <- sample(1:N, s)
#' S <- D[index,]

#'DD <- pre.pplot.pro(S, D)
#'for(i in 1:N)
#'  DD[i, ] <- normaliz.vector(DD[i, ])
#'
#' res <- tropical.PCA.Polytope(S, D, V, I = 1000,50)
#' DD <- pre.pplot.pro(res[[2]], res[[3]])
#' trop.tri.plot.w.pts(normaliz.ultrametrics(res[[2]]), DD)
#' }


#' @export
#' @rdname tropical.PCA
tropical.PCA.Polytope <- function(S, D, V, I = 1,k){
  ## I is the number of iterations, V is a matrix whose row is a vertex of a bigger tropical polytope.
  d <- dim(D)
  s <- dim(S)
  ## s[1] is the number of principal components of the tropical PCA
  P <- Sum.Residuals(S, D)
  S.star <- S
  S0 <- S
  for(i in 1:I){
    for(j in 1:s[1]){
      Sp <- S0
      Sp[j, ] <- TropicalPolytope.extrapolation.HAR(V, Sp[j, ], k)
      A <- Sum.Residuals(S0, D)/Sum.Residuals(Sp, D)
      if(runif(1)<A){
        S0 <- Sp       # accept move with probabily min(1,A)
      }
      if(Sum.Residuals(S0, D) < Sum.Residuals(S.star, D))
        S.star <- S0
    }
  }
  D1 <- D
  for(i in 1:d[1])
    D1[i, ] <- normaliz.vector(project_pi(S.star, D[i,]))

  return(list(Sum.Residuals(S.star, D), S.star, D1))
}

Sum.Residuals <- function(S, D){
  ## S is the set of vertices of a tropical polytope and D is a set of data
  d <- dim(D)
  ## d[1] is the sample size
  y <- 0
  for(i in 1:d[1]){
    y <- y + trop.dist(D[i,], project_pi(S, D[i,]))
  }
  return(y)
}
