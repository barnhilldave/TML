#' Calculate the center point and radius of the maximum inscribed ball for a
#' tropical simplex
#'
#' This function calculates the center point and radius of the maximum inscribed
#' ball for a max- or min-plus tropical simplex
#'
#' @param A matrix of points defining a tropical polytope; rows are the points
#' @param add string; 'max' indicates max-plus addition, 'min' indicates
#'   min-plus addition. Defaults to 'max'
#' @return list containing the radius and center point of a maximum inscribed
#'   ball
#' @author David Barnhill \email{david.barnhill@@nps.edu}
#' @references Barnhill, David, Ruriko Yoshida and Keiji Miura (2023). Maximum
#'   Inscribed and Minimum Enclosing Tropical Balls of Tropical Polytopes and
#'   Applications to Volume Estimation and Uniform Sampling.
#' @export
#' @examples
#' P<-matrix(c(0,0,0,0,2,5,0,3,1),3,3,TRUE)
#' max_ins.ball(P)
#' max_ins.ball(P,add='min')

max_ins.ball<-function(A,add='max'){
  n<-rep(0,(ncol(A)))
  if(add=='max'){
    A<-tdets(A)[[2]]
    for (i in 2:ncol(A)){
      if(min(A[,i])<0){
        n[i]<-min(A[,i])
        A[,i]<-A[,i]-min(A[,i])
      }
    }
    W<-matrix(0,0,ncol(A))
    P<-permutations((ncol(A)),2)
    bb<-c()
    for(i in 1:nrow(A)){
      A[i,]<-A[i,]-A[i,i]
    }
    A<-t(A)
    for (i in 1:nrow(P)){
      if(P[i,1]==1){
        a<-rep(0,ncol(A))
        a[P[i,2]-1]=1
        a[length(a)]=2
        W<-rbind(W,a)
        bb<-append(bb,A[P[i,1],P[i,2]])
      }
      else if(P[i,2]==1){
        a<-rep(0,ncol(A))
        a[P[i,1]-1]<--1
        W<-rbind(W,a)
        bb<-append(bb,A[P[i,1],P[i,2]])
      }
      else{
        a<-rep(0,ncol(A))
        a[P[i,1]-1]<--1
        a[P[i,2]-1]<-1
        a[length(a)]<-1
        W<-rbind(W,a)
        bb<-append(bb,A[P[i,1],P[i,2]])
      }
    }
    f.con<-W
    f.dir<-c(rep("<=",(ncol(A)^2-ncol(A))))
    f.rhs<--bb
    f.obj<-c(rep(0,(ncol(A)-1)),1)
    sol<-lp ("max", f.obj, f.con, f.dir, f.rhs)
    solu<-sol$solution
    rad<-solu[length(solu)]
    cent<-solu[1:(length(solu)-1)]+n[2:length(n)]+rad
  }
    if(add=='min'){
      A<-tdets(A,add='min')[[2]]
      for (i in 2:ncol(A)){
        if(min(A[,i])<0){
          n[i]<-min(A[,i])
          A[,i]<-A[,i]-min(A[,i])
        }
      }
      W<-matrix(0,0,ncol(A))
      P<-permutations((ncol(A)),2)
      bb<-c()
      for(i in 1:nrow(A)){
        A[i,]<-A[i,]-A[i,i]
      }
      A<-t(A)
      for (i in 1:nrow(P)){
        if(P[i,1]==1){
          a<-rep(0,ncol(A))
          a[P[i,2]-1]=-1
          a[length(a)]=2
          W<-rbind(W,a)
          bb<-append(bb,A[P[i,1],P[i,2]])
        }
        else if(P[i,2]==1){
          a<-rep(0,ncol(A))
          a[P[i,1]-1]<-1
          W<-rbind(W,a)
          bb<-append(bb,A[P[i,1],P[i,2]])
        }
        else{
          a<-rep(0,ncol(A))
          a[P[i,1]-1]<-1
          a[P[i,2]-1]<--1
          a[length(a)]<-1
          W<-rbind(W,a)
          bb<-append(bb,A[P[i,1],P[i,2]])
        }
      }
      f.con<-W
      f.dir<-c(rep("<=",(ncol(A)^2-ncol(A))))
      f.rhs<-bb
      f.obj<-c(rep(0,(ncol(A)-1)),1)
      sol<-lp ("max", f.obj, f.con, f.dir, f.rhs)
      solu<-sol$solution
      rad<-solu[length(solu)]
      cent<-solu[1:(length(solu)-1)]+n[2:length(n)]-rad
  }
  return(list(rad,c(0,cent)))
}
