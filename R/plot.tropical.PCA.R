#' Plotting PCA-derived tropical triangles
#'
#' This function conducts tropical PCA to find the best fit tropical triangle given data defined in the tropical projective torus.
#' It employs the vertex HAR with extrapolation sampler to sample points to determine the vertices of the tropical triangle.
#' @param S inital set of vertices for the tropical triangle
#' @param D matrix of data where each row is an observation in the tropical projective torus
#' @return rendering of tropical triangle saved to current directory
#' @author Ruriko Yoshida \email{ryoshida@@nps.edu}
#' @export
#' @examples
#' s <- 3 #number of vertices.  Here it is a tropical triangle
#' d <- 3 ## dimension
#' N <- 100 ## sample size
#' V <- matrix(c(100, 0, 0, 0, 100, 0, 0, 0, 100, -100, 0, 0, 0, -100, 0, 0, 0, -100), 6, 3, TRUE)
#' D <- matrix(rep(0, N*d), N, d)
#' D[, 1] <- rnorm(N, mean = 5, sd = 5)
#' D[, 2] <- rnorm(N, mean = -5, sd = 5)
#' D[, 3] <- rnorm(N, mean = 0, sd = 5)
#' index <- sample(1:N, s)
#' S <- D[index,]
#' res <- tropical.PCA.Polytope(S, D, V, I = 1000,50)
#' DD <- pre.pplot.pro(res[[2]], res[[3]])
#' trop.tri.plot.w.pts(normaliz.ultrametrics(res[[2]]), DD)
#'
#'

trop.tri.plot.w.pts <- function(S, D){
  k <- ncol(S)
  plot(S[1,],S[2,],xlab='x1',ylab='x2',asp = 1)
  for(i in 1:(k - 1)){
    for(j in (i + 1):k){
      tseg1 <- tropical.geodesic.dim.2(S[,i],S[,j])
      tseg2 <- tropical.geodesic.dim.2(S[,i],S[,j],flag=1)
      if(tseg1[[2]] < tseg2[[2]]) tseg <- tseg1
      else tseg <- tseg2
      lines(c(tseg[[1]][1,1],tseg[[1]][1,2]),c(tseg[[1]][2,1],tseg[[1]][2,2]),col= 'black')
      lines(c(tseg[[1]][1,2],tseg[[1]][1,3]),c(tseg[[1]][2,2],tseg[[1]][2,3]),col= 'black')
    }
  }
  points(x=D[,2],y=D[,3],pch=16,cex=0.6,col= "red")
}

