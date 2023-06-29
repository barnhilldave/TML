#' Tropical Gaussian sampling about a center of mass
#'
#' This function is a Gaussian-like HAR sampler about a center of mass denoted by a location parameter with scale parameter in terms of the tropical distance.
#'
#' @param D_s matrix of vertices of a tropical simplex; each row is a vertex.
#' @param x0 initial point for sampler, numeric vector
#' @param I number of states in Markov chain
#' @param M location parameter; numeric vector indicating centroid
#' @param S scale parameter; in terms of tropical distance
#' @return next sampled point from the tropical polytope
#' @author David Barnhill \email{david.barnhill@@nps.edu}
#' @name tropical.Gaussian
#' @examples
#' D_s <-matrix(c(0,0,0,0,10,0,0,0,10),3,3,TRUE)
#' x0 <- c(0,0,0)
#' M <- c(0,5,5)
#' S <- 2
#' TropicalPolytope.extrapolation.HAR_NORM(D_s, x0, I = 50,M,S)

#'@export
#'@rdname tropical.Gaussian
TropicalPolytope.extrapolation.HAR_NORM <- function(D_s, x0, I = 1,M,S){
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
      x<-HAR.TLineSeg.Norm_Poly.V2(l[1,], l[nrow(l),],M,S) # Conduct HAR on line segment between u and the last bendpoint.
      x<-normaliz.vector(x)
    }
    else{
      x<-HAR.TLineSeg.Norm_Poly.V2(u,v,M,S) # Conduct HAR if there are no bendpoints.
      x<-normaliz.vector(x)
    }
  }

  return(normaliz.vector(x))
}

HAR.TLineSeg.Norm_Poly.V2<-function(D1,D2,mu,stdev){
  d <- length(D1)
  D<-rbind(D1,D2)
  if(length(D1) != length(D2))
    warning("dimension is wrong!")
  proj_mu<-project_pi(D,mu)
  d_mu<-trop.dist(mu,proj_mu)

  E_hi<-D2
  E_lo<-D1
  D_lo<-proj_mu
  D_hi<-proj_mu
  d_hi<-trop.dist(E_hi,D_lo)
  d_lo<-trop.dist(E_lo,D_hi)
  while(d_hi>(1e-8)){
    prop<-HAR.TLineSeg(E_hi,D_lo)
    d_mi<-trop.dist(prop,mu)
    if(d_mi<=(d_mu+1e-8)){
      D_lo<-normaliz.vector(prop)
    }
    else{
      E_hi<-normaliz.vector(prop)}
    d_hi<-trop.dist(E_hi,D_lo)

  }
  while(d_lo>(1e-8)){
    prop<-HAR.TLineSeg(E_lo,D_hi)
    d_mi<-trop.dist(prop,mu)
    if(d_mi<=(d_mu+1e-8)){
      D_hi<-normaliz.vector(prop)
    }
    else{
      E_lo<-normaliz.vector(prop)}
    d_lo<-trop.dist(D_hi,E_lo)
  }
  pro_mu<-HAR.TLineSeg(E_hi,E_lo)
  pts<-rbind(D1,D2,pro_mu)
  l0<-trop.dist(D2,pro_mu)+min(D2-D1)
  l <- rnorm(1, mean = 0, sd=stdev)
  #print(l)
  x_p <- rep(0, d)
  for(i in 1:d){
    x_p[i] <-max(((l+ l0) + D1[i]), D2[i])
  }
  x1<-normaliz.vector(x_p)
  return(x1)
}

