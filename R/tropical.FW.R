#'Calculate the tropical Fermat-Weber point
#'
#'This function calculates the Fermat-Weber point for a tropical polytope
#'@param A matrix with normalized tropical points as rows
#'@return numeric vector providing the tropical Fermat-Weber point for the
#'  tropical polytope
#'@author David Barnhill \email{david.barnhill@@nps.edu}
#'@references Lin, Bo and Ruriko Yoshida (2016). Tropical Fermat-Weber Points.
#'  SIAM J. Discret. Math. 32: 1229-1245.
#'@export
#' @examples
#'P <-matrix(c(0,0,0,0,2,5,0,3,1),3,3,TRUE)
#'
#'trop.FW(P)
#'

trop.FW<-function(A){
  perms<-permutations(ncol(A),2)
  mins<-apply(A,2,min)
  if(any(mins<0)){
    mins_ind<-which(mins<0)
    A[,mins_ind]<--rep(mins[mins_ind],each=nrow(A))+A[,mins_ind]
  }

  yys<-matrix(0,nrow(perms),ncol(A),TRUE)
  for(i in 1:nrow(yys)){
    yys[i,perms[i,1]]=1
    yys[i,perms[i,2]]=-1
  }
  L<-matrix(rep(t(yys),nrow(A)),ncol=ncol(yys),byrow=TRUE)

  R<-matrix(0,0,nrow(A),TRUE)
  for(j in 1:nrow(A)){
    rrs<-matrix(0,nrow(perms),nrow(A),TRUE)
    rrs[,j]=-1
    R<-rbind(R,rrs)
  }
  f.con<-cbind(L,R)
  zer<-c(1,rep(0,(ncol(f.con)-1)))
  f.con<-rbind(f.con,zer)

  f.rhs<-c()
  for(j in 1:nrow(A)){
    for(i in 1:nrow(perms)){
      diff<--A[j,perms[i,1]]+A[j,perms[i,2]]
      f.rhs<-append(f.rhs,-diff)
    }
  }
  f.rhs<-append(f.rhs,0)
  f.dir <- c(rep("<=",nrow(f.con)))
  f.obj<-c(rep(0,(ncol(f.con)-ncol(R))),rep(1,ncol(R)))
  res<-lp ("min", f.obj, f.con, f.dir, f.rhs)
  FW<-res$solution[1:ncol(A)]
  if(any(mins<0)){
    FW[mins_ind]<-FW[mins_ind]+mins[mins_ind]
  }
  return(FW)
}

