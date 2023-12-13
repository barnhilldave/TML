#' Calculate the minimum generating vertex set of a tropical ball
#'
#' This function calculates the coordinates of the minimum or entire vertex set of a tropical ball given a center point
#'
#' @param x matrix where each row is a point defining a tropical polytope
#' @param d radius of the tropical ball in terms of tropical distance
#' @param al logical; TRUE or FALSE to determine whether to enumerate all vertices of the tropical ball
#' @return matrix of normalized tropical points defining the tropical ball. Rows are the points
#' @author David Barnhill \email{david.barnhill@@nps.edu}
#' @references Barnhill, David, Ruriko Yoshida and Keiji Miura (2023). Maximum Inscribed and Minimum Enclosing Tropical Balls of Tropical Polytopes and Applications to Volume Estimation and Uniform Sampling.
#' @export
#' @examples
#'x <-c(0,3,7,5)
#'d <- 2
#'trop_bal.vert(x,d)
#'trop_bal.vert(x,d,al=TRUE)

trop_bal.vert<-function(x,d,al=FALSE){
  dm<-length(x)
  if(al==FALSE){
    A<-matrix(0,dm,dm,TRUE)
    i=1
    while (i < dm){
      i=i+1
      a<-c(rep(0,dm))
      a[i]<-d
      A[i,]<-x+a
    }
    A[1,]<-x-c(0,rep(d,(dm-1)))
  }
  else{
    vecs<-matrix(0,0,dm-1,TRUE)
    for (i in 1:(2^(dm-1)-1)){
      vec<-c()
      curi<-i
      for (j in (dm-2):0){
        if(curi>=2^j){
          vec<-append(vec,1)
          curi<-curi-2^j
        }
        else{vec<-append(vec,0)}
      }

      vecs<-rbind(vecs,vec)
    }
    Avecs_p<-ifelse(vecs==1,d,0)
    As<-rbind(Avecs_p,-Avecs_p)
    Asp<-cbind(rep(0,2^dm-2),As)
    A<-t(apply(Asp,1,function(y) y+x))
  }
  return(A)
}

