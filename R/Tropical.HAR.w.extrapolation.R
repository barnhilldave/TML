#' Vertex HAR with extrapolation (VHE) MCMC with uniform target distribution
#'
#' This function samples points uniformly the space defined by a tropical simplex
#'
#' @param D_s matrix of vertices of a tropical simplex; each row is a vertex
#' @param x0 initial point for sampler, numeric vector
#' @param I number of states in Markov chain
#' @param tadd function; max indicates max-plus addition, min indicates min-plus addition. Defaults to max
#' @return next sampled point from the tropical polytope
#' @author David Barnhill \email{david.barnhill@@nps.edu}
#' @references Yoshida, Ruriko, Keiji Miura and David Barnhill (2022). Hit and Run Sampling from Tropically Convex Sets.
#' @export
#' @examples
#' D_s <-matrix(c(0,0,0,0,10,0,0,0,10),3,3,TRUE)
#' x0 <- c(0,0,0)
#' VE.HAR(D_s, x0, I = 50)
#' VE.HAR(D_s, x0, I = 50,tadd=min)

VE.HAR <- function(D_s, x0, I = 1, tadd=max){
  k<-ncol(D_s)-1
  d <- dim(D_s) # dimension of matrix of vertices
  D <- normaliz.polytope(D_s) #D_s
  D_bp<-D
  x <- normaliz.vector(x0) #normalize x0

  for(i in 1:I){
    x1 <- x
    v <- x
    u <- x
    ind<-seq(1,d[1])

    (index <- sample(ind, k)) # Randomly choose k vertices
    U <- D[index,] # Subset U
    V <- D[-index,] # Complement of U

    # If subset k=1 then this is just a vertex the projection of x0 on a vertex is just the vertex
    if(k == 1) u <- U
    # If k > 1 project x0 onto the polytope defined by the subset of vertices
    else u <- project.pi(U, x1, tadd=tadd)

    # If the cardinality of U is e-1 the complement is just the leftover vertex
    if(k == (d[1] - 1)) v <- V
    # Otherwise it is a projection
    else v <- project.pi(V, x1, tadd=tadd)

    # Get unique bendpoints in a tropical line
    Gam <- unique(TLineSeg(v,u, tadd = tadd))
    # Normalize the bendpoints
    bps <- normaliz.polytope(matrix(unlist(Gam),length(Gam),d[2],byrow=TRUE))

    # We calculate the tropical distance of the bendpoints (not the end points) of the tropical
    # line segment to each boundary vertex defining a hyperplane.
    # If there is more than one bendpoint, calculate the distance of each one.  If not then just
    # conduct HAR on the line segment.
    if(nrow(bps)>2){
      l<-matrix(u,1,ncol(bps),TRUE) # Matrix only consisting of the starting point.
      bp<-bps[2:(nrow(bps)),] # bendpoints of the line segment without the endpoints
      t=0
      while (t <nrow(bp)){
        t=t+1

        # Calculates distance of bend point to all vertices
        dists<-as.vector(apply(D,1,function(x) trop.hyper.dist(-x,bp[t,],tadd=tadd)))

        if(all(dists>1e-8)) l<-rbind(l,bp[t,])
        else{
          l<-rbind(l,bp[t,]) # If there is a zero add the point and then exit the loop.
          break
        }
      }
      x<-HAR.TLineSeg(l[1,], l[nrow(l),], tadd=tadd) # Conduct HAR on line segment between u and the last bendpoint.
      x<-normaliz.vector(x)
    }
    else{
      x<-HAR.TLineSeg(u,v, tadd=tadd) # Conduct HAR if there are no bendpoints.
      x<-normaliz.vector(x)
    }
  }

  return(normaliz.vector(x))
}



