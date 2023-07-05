#' Tropical support vector machine
#'
#' Supervised learning using tropical support vector machines in conjunction with simulated annealing
#'
#' @param V matrix of data points where each row is an observation; does not include dependent variables
#' @param clses vector of classifications of each observation (response variable)
#' @param L maximum number of iterations to use
#' @param tr proportion of data used for training set
#' @param st number of iterations to terminate MCMC if current state does not change
#' @param trop logical; 1 if data possesses a leading column of zeros; 0 otherwise
#' @return list with confusion matrix and correct classification rate
#' @author David Barnhill \email{david.barnhill@@nps.edu}
#' @references Gärtner, Bernd & Jaggi, Martin (2023). Tropical Support Vector Machines.
#' @references Ruriko Yoshida, Misaki Takamori, Hideyuki Matsumoto, Keiji Miura (2023).Tropical support vector machines: Evaluations and extension to function spaces,
#' Neural Networks, Volume 157, Pages 77-89, ISSN 0893-6080.
#' @export
#' @examples
#'\dontrun{
#' library(HDclassif)
#' wine1<-as.matrix(wine)
#' wines<-wine1[,2:ncol(wine1)]
#' w_type<-wine1[,1]
#' T.SVM(wines,w_type,L=1000,tr=.8,st=100,trop=1)
#'}

T.SVM<-function(V,clses,L=1000,tr=.8,st=100,trop=1){
  specs<-as.vector(unique(clses))
  dt<-sample(nrow(V),tr*nrow(V))
  if(trop==1){
    V<-cbind(rep(0,nrow(V)),V)
  }
  else{
    V<-normaliz.polytope(V)
  }
  V_tr<-V[dt,]
  V_test<-V[-dt,]
  V_cl<-clses
  V_cl_test<-V_cl[-dt]

  yy<-V_cl[dt]
  xx<-V_tr

  v<-apply(xx,2,min)
  max_v<-apply(xx,2,max)
  cc<-matrix(0,0,ncol(xx),TRUE)
  cc<-rbind(cc,v)
  for (i in 2:length(max_v)){
    v1<-v
    v1[i]<-max_v[i]
    cc<-rbind(cc,v1)
  }

  # Starting values
  x0<-apply(xx,2,mean)
  w_star<--x0 # normal vector of initial point

  ## This tells me which sector each point is in relative to the new
  Tstar<-apply(xx,1,function(x) which.max(x+w_star))

  gin<-cbind(yy,Tstar)
  c0<-matrix(rep(0,ncol(xx)*length(specs)),ncol(xx),length(specs),TRUE)
  for (j in 1:nrow(gin)){
    sp<-gin[j,1]
    sec<-gin[j,2]
    c0[sec,sp]<-c0[sec,sp]+1
  }

  rc0<-rowSums(c0)
  c1<-c0
  for (i in 1:nrow(c0)){
    if(rc0[i]>0){
      c1[i,]<-c0[i,]/rc0[i]*(1-c0[i,]/rc0[i])
    }
    else{
      c1[i,]<-0
    }
  }
  g<-rowSums(c1)
  rc1<-rc0/nrow(xx)
  gini_star<-g%*%rc1

  teaky<-cbind(xx,Tstar)
  co<-0
  for (i in 1:ncol(xx)){
    teaks<-matrix(teaky[teaky[,ncol(teaky)]==i,],ncol=ncol(teaky))
    if(nrow(teaks)>0){
      max_min_dis<-apply(matrix(teaks[,-ncol(teaks)],ncol=ncol(teaks)-1),1,function(x) trop.dist.hyp_max(w_star,x))
      ma_mi_dis<-min(max_min_dis)
      co<-co+ma_mi_dis
    }
  }
  c_n<-co*(gini_star-1)
  c_star<-c_n
  ##### loop

  N<-L # number of points
  har_norms1<-matrix(0,N,ncol(xx)+1,TRUE) # matrix to capture the walk
  k<-1
  costs=1
  sto<-st
  while(k<=N){
    print(k)
    if(costs<sto){
      T<-T<-1-k/(N+.0001)
      y<-TropicalPolytope.extrapolation.HAR(cc,x0,I=50) # choose a point for the next normal vector
      w<--y # convert to normal vector
      # From here we find the sectors
      Tx<-apply(xx,1,function(x) which.max(x+w))
      gin<-cbind(yy,Tx)
      c0<-matrix(rep(0,ncol(xx)*length(specs)),ncol(xx),length(specs),TRUE)
      for (j in 1:nrow(gin)){
        sp<-gin[j,1]
        sec<-gin[j,2]
        c0[sec,sp]<-c0[sec,sp]+1
      }
      rc0<-rowSums(c0)
      c1<-c0
      for (i in 1:nrow(c0)){
        if(rc0[i]>0){
          c1[i,]<-c0[i,]/rc0[i]*(1-c0[i,]/rc0[i])
        }
        else{
          c1[i,]<-0
        }
      }
      g<-rowSums(c1)
      rc1<-rc0/nrow(xx)
      gini_prop<-g%*%rc1
      teaky<-cbind(xx,Tstar)
      co<-0
      for (i in 1:ncol(xx)){
        teaks<-matrix(teaky[teaky[,ncol(teaky)]==i,],ncol=ncol(teaky))
        if(nrow(teaks)>0){
          max_min_dis<-min(apply(matrix(teaks[,-ncol(teaks)],ncol=ncol(teaks)-1),1,function(x) trop.dist.hyp_max(w_star,x)))
          ma_mi_dis<-min(max_min_dis)
          co<-co+ma_mi_dis
        }
      }
      c_prop<-co*(gini_prop-1)
      ## MH from here
      A<-exp(-(c_prop/T))/exp(-(c_n/T))
      p<-runif(1,0,1)
      if(p<A){
        har_norms1[k,1:(ncol(har_norms1)-1)]<-y
        har_norms1[k,ncol(har_norms1)]<-c_prop
        if(c_prop < c_star){
          y_star<-y
          c_star<-c_prop
        }
        x0<-y
        c_n<-c_prop
        k<-k+1
        costs<-1
      }
      else{
        costs<-costs+1
      }
    }
    else{
      break
    }
  }
#
  #ind<-which.min(har_norms1[1:(k-1),ncol(har_norms1)])
  #ind2<-max(which(har_norms1[,ncol(har_norms1)]==har_norms1[ind,ncol(har_norms1)]))
  #min_vec<-har_norms1[ind2,1:(ncol(har_norms1)-1)]
  w_star<--y_star # normal vector of initial point
  ## This tells me which sector each point is in relative to the new
  Tstar<-apply(V_test,1,function(x) which.max(x+w_star))
  gin<-cbind(V_cl_test,Tstar)
  cc0<-matrix(rep(0,ncol(V_test)*length(specs)),ncol(V_test),length(specs),TRUE)
  for (j in 1:nrow(gin)){
    sp<-gin[j,1]
    sec<-gin[j,2]
    cc0[sec,sp]<-cc0[sec,sp]+1
  }

  max_inds<-apply(cc0,1,function(x) which.max(x))
  maxes<-apply(cc0,1,function(x) max(x))
  ma<-cbind(maxes,max_inds)
  conf_mat<-matrix(0,length(specs),length(specs))
  for (i in 1:nrow(ma)){
    conf_mat[ma[i,2],ma[i,2]]<-conf_mat[ma[i,2],ma[i,2]]+ma[i,1]
  }
  corr_prob<-sum(conf_mat)/nrow(V_test)
  pres<-list(conf_mat,corr_prob)
  return(pres)
}



