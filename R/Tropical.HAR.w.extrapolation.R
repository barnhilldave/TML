#' Uniform sampling from a tropical simplex
#'
#' This function samples points uniformly the space defined by a tropical simplex.
#'
#' @param D_s matrix of vertices of a tropical simplex; each row is a vertex.
#' @param x0 initial point for sampler, numeric vector
#' @param I number of states in Markov chain
#' @return next sampled point from the tropical polytope
#' @author David Barnhill \email{david.barnhill@@nps.edu}
#' @export
#' @examples
#' D_s <-matrix(c(0,0,0,0,10,0,0,0,10),3,3,TRUE)
#' x0 <- c(0,0,0)
#' TropicalPolytope.extrapolation.HAR(D_s, x0, I = 50)

TropicalPolytope.extrapolation.HAR <- function(D_s, x0, I = 1){
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
    if(k == 1) # If subset k=1 then this is just a vertex
      u <- U   # the projection of x0 on a vertex is just the vertex
    else{
      u <- project_pi(U, x1) # If k>1 project x0 onto the polytope
    }                      # defined by the subset of vertices
    if(k == (d[1] - 1)) # If the cardinality of U is e-1
      v <- V            # the complement is just the leftover vertex
    else
      v <- project_pi(V, x1) # Otherwise it is a projection
    Gam<-unique(TLineSeg(v,u)) # Get unique bendpoints in a tropical line
    bps<-normaliz.polytope(matrix(unlist(Gam),length(Gam),d[2],byrow=TRUE)) # Normalize the bendpoints
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
        dists<-as.vector(apply(D,1,function(x) trop.dist.hyp_min(-x,bp[t,]))) # Calculates distance of bend point to all vertices
        if(all(dists>1e-8)){
          l<-rbind(l,bp[t,])
        }
        else{
          l<-rbind(l,bp[t,]) # If there is a zero add the point and then exit the loop.
          break
        }
      }
      x<-HAR.TLineSeg(l[1,], l[nrow(l),]) # Conduct HAR on line segment between u and the last bendpoint.
      x<-normaliz.vector(x)
    }
    else{
      x<-HAR.TLineSeg(u,v) # Conduct HAR if there are no bendpoints.
      x<-normaliz.vector(x)
    }
  }

  return(normaliz.vector(x))
}
