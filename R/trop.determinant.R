#' Calculate the tropical determinant of a square matrix.
#'
#' This function calculates the tropical determinant (or singularity) of a
#' square matrix
#'
#' @param P matrix of points defining a tropical polytope. Rows are the points
#' @param tadd function; max indicates max-plus addition, min indicates
#'   min-plus addition. Defaults to max
#' @return list containing the value of the determinant and reordered matrix P
#' @author David Barnhill \email{david.barnhill@@nps.edu}
#' @export
#' @examples
#' P<-matrix(c(0,0,0,0,2,5,0,3,1),3,3,TRUE)
#' tdets(P)
#' tdets(P,tadd=min)

tdets<-function(P, tadd=max){
  dd<-dim(P)
  B<-P
  sign <- if (identical(tadd, base::min)) 1 else -1

  if(dd[[1]]!=dd[[2]]){
    warning("Not a Square Matrix!")
  }
  else {
    tds<-c(0,0)
    perms<-permn(dd[[1]])
    tdet<-sign*Inf

    i=1
    ind<-0
    while(i<=length(perms)){
      k<-perms[[i]]
      t<-0
      for(j in 1:length(k)){
        t <- t+P[j,k[j]]
      }

      if(t == tadd(t, tdet)){
        if (t==tdet){
          tds<-c(tdet,t)
        } else {
          tdet<-t
          tds<-c(tdet,0)
          ind<-perms[[i]]
        }
      }
      i=i+1
    }

    if(tds[1]==tds[2]){
      warning('Singular')
    }
    else{
      for (i in 1:length(ind)){
        P[ind[i],]<-B[i,]
      }
      return(list(tdet,P))
    }
  }
}
