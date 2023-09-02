#' Tropical Logistic Regression
#'
#' Performs tropical logistic regression, by finding the optimal statistical parameters for the
#' training dataset (D,Y), where D is the matrix of covariates and Y is the binary response vector
#'
#' @param D matrix of dimension N*e, where N is the number of observations which lie in R^e
#' @param Y binary vector with dimension N, with each component corresponding to an observation
#' @param penalty scalar; positive real number
#' @param model_type string; options are "two-species" (default),  "one-species", "general"
#' @return vector; optimal model parameters (two normal vectors and two scaling factors)
#' @author Georgios Aliatimis \email{g.aliatimis@lancaster.ac.uk}
#' @references Aliatimis, Georgios, Ruriko Yoshida, Burak Boyaci and James A. Grant (2023). Tropical Logistic Regression on Space of Phylogenetic Trees.
#' @examples
#' \donttest{
#' library(ROCR)
#' T0 = Sim_Trees15
#' T1 = Sim_Trees25
#' D  = rbind(T0,T1)
#' Y = c(rep(0,dim(T0)[1]), rep(1,dim(T1)[1]))
#' N = length(Y)
#' set.seed(1)
#' train_set = sample(N,floor(0.8 * N)) ## 80/20 train-test split
#' pars <- trop.logistic.regression(D[train_set,],Y[train_set], penalty=1e4)
#' test_set = (1:N)[-train_set]
#' Y.hat <- rep(0, length(test_set))
#' for(i in 1:length(test_set))   Y.hat[i] <- prob.class(pars, D[test_set[i],])
#' Logit.ROC <- performance(prediction(Y.hat, Y[test_set]), measure="tpr", x.measure="fpr")
#' plot(Logit.ROC, lwd = 2, main = "ROC Curve for Logistic Regression Model")
#' print(paste("Logit.AUC=", performance(prediction(Y.hat, Y[test_set]), measure="auc")@y.values))
#' }
#' @export
#' @rdname trop.logistic.regression
trop.logistic.regression <- function(D,Y,penalty=0,model_type="two_species"){
  N = dim(D)[1]
  e = dim(D)[2]
  omega = list()

  if (model_type == "one_species"){
    tmp = FWpoint.num.w.reg(D)
    omega[[1]] = tmp
    omega[[2]] = tmp
  } else {
    omega[[1]] = FWpoint.num.w.reg(D[Y==0,],penalty)
    omega[[2]] = FWpoint.num.w.reg(D[Y==1,],penalty)
  }

  # Find optimal scalars sigma_0 and sigma_1
  f <- function(t){
    lambda = exp(t)
    ans = 0
    for(i in 1:N){
      x = D[i,]
      h = lambda[1] * trop.dist(x,omega[[1]]) - lambda[2] * trop.dist(x,omega[[2]]) + (e-1)*log(lambda[2]/lambda[1])
      ans <- ans + Y[i] * log(sigmoid(h)) + (1-Y[i]) * log(sigmoid(-h))
    }
    ans/N
  }

  pars = c(omega[[1]],omega[[2]])
  pars.init = 1/sigma_est(pars,Y,D)
  if(model_type == "two_species"){
    #Under this model, we assume that lambda[1] = lambda[2] = lambda
    pars.init = mean(pars.init)
    grad <- function(t){
      lambda <- exp(t)
      ans <- 0
      for(i in 1:N){
        x = D[i,]
        h = lambda * (trop.dist(x,omega[[1]]) - trop.dist(x,omega[[2]]))
        ans <- ans + (Y[i] - sigmoid(h)) * h
      }
      ans/N
    }
    t = optim(par=log(pars.init),fn=function(x) f(c(x,x)),gr=grad,method="CG",control=list(fnscale=-1))$par
    lambda = exp(t)
    lambda = c(lambda, lambda)
  } else {
    grad <- function(t){
      lambda = exp(t)
      ans <- rep(0,2)
      for(i in 1:N){
        x = D[i,]
        h = lambda[1] * trop.dist(x,omega[[1]]) - lambda[2] * trop.dist(x,omega[[2]]) + (e-1)*log(lambda[2]/lambda[1])
        ans[1] <- ans[1] + (Y[i] - sigmoid(h)) * (lambda[1] * trop.dist(x,omega[[1]]) + (e-1))
        ans[2] <- ans[2] - (Y[i] - sigmoid(h)) * (lambda[2] * trop.dist(x,omega[[2]]) + (e-1))
      }
      ans/N
    }
    t = optim(par=log(pars.init),fn=f,gr=grad,method="CG",control=list(fnscale=-1))$par
    lambda = exp(t)
  }
  return(c(pars,lambda))
}

sigma_est <- function(pars,Y,D){
  e = length(pars)/2
  N = dim(D)[1]
  omega <- pars[1:e]

  D0 = D[Y==0,]
  N0 = sum(Y==0)
  d0 = 0
  for(i in 1:N0){
    d0 = d0 + trop.dist(omega,D0[i,])
  }
  d0 = d0/N0

  D1 = D[Y==1,]
  N1 = sum(Y==1)
  d1 = 0
  for(i in 1:N1){
    d1 = d1 + trop.dist(omega,D1[i,])
  }
  d1 = d1/N1

  c(d0,d1) /(e-1)
}
