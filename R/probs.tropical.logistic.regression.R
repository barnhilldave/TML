#' Estimated probability for binary class assignment
#'
#' Estimates the probability that an observation x belongs to class 1.
#'
#' @param pars vector of parameters, which can be decomposed as two normal
#'   vectors and two scaling parameters and has dimension 2*e+2
#' @param x vector of dimension e
#' @return real number
#' @author Georgios Aliatimis \email{g.aliatimis@lancaster.ac.uk}
#' @references Aliatimis, Georgios, Ruriko Yoshida, Burak Boyaci and James A.
#'   Grant (2023). Tropical Logistic Regression on Space of Phylogenetic Trees
#' @export
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

prob.class <- function(pars,x){
  e= (length(pars)-2)/2
  lambda = pars[(2*e+1):(2*e+2)]
  omega =list(pars[1:e],pars[(e+1):(2*e)])
  h = lambda[1] * trop.dist(x,omega[[1]]) - lambda[2] * trop.dist(x,omega[[2]]) + (e-1)*log(lambda[2]/lambda[1])
  sigmoid(h)
}
