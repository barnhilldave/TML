#' Calculate the minimum or entire generating vertex set of a tropical ball using a max- or min-plus algebra
#'
#' This function calculates the coordinates of the minimum or entire vertex set of a tropical ball in terms of either a max- or min-plus algebra for a given a center point
#'
#' @param x matrix where each row is a point defining a tropical polytope
#' @param d radius of the tropical ball in terms of tropical distance
#' @param tadd function; max indicates max-plus addition, min indicates min-plus addition, 'all' indicates all vertices. Defaults to 'max'
#' @return matrix of normalized tropical points defining the tropical ball. Rows are the points
#' @author David Barnhill \email{david.barnhill@@nps.edu}
#' @references Barnhill, David, Ruriko Yoshida and Keiji Miura (2023). Maximum Inscribed and Minimum Enclosing Tropical Balls of Tropical Polytopes and Applications to Volume Estimation and Uniform Sampling.
#' @export
#' @examples
#'x <-c(0,3,7,5)
#'d <- 2
#'trop.bal.vert(x,d)
#'trop.bal.vert(x,d,tadd=min)
#'trop.bal.all_vert(x,d)
#' @export
#' @rdname tropical.ball.verts
trop.bal.vert<-function(x, d, tadd=max){
  dm<-length(x)
  sign <- if (identical(tadd, base::max)) -1 else 1

  A<-matrix(0,dm,dm,TRUE)

  i=1
  while (i < dm){
    i=i+1
    a<-c(rep(0,dm))
    a[i]<-d
    A[i,]<-x+a
  }

  A[1,]<-x + sign*c(0,rep(d,(dm-1)))


  return(A)
}

#' @export
#' @rdname tropical.ball.verts
trop.bal.all_vert<-function(x, d) {
  dm<-length(x)
  A<-matrix(0,dm,dm,TRUE)
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

  return(A)
}


