#' Modified Fermat-Weber point numerical solver for ultrametrics
#'
#' Returns a modified Fermat-Weber point of N points using a gradient based numerical method
#' This method is appropriate for points coming from ultrametrics.
#' The algorithm tries to find a point that minimizes the sum of tropical distances from the samples, but also
#' also tries to find a point that is as close as possible to the space of ultrametrics.
#' The tradeoff between these two objectives is controlled by the penalty parameter.
#' If penalty=0, the method is identical to FWpoint_numerical; it finds the Fermat-Weber point, which may not be an ultrametric.
#' If penalty is very large, the algorithm is trying to find the Fermat-Weber point in the space of ultrametrics.
#' @param datamatrix matrix of dimension N*e, where N is the number of observations which lie in R^e
#' @param penalty positive real number; the regularization rate
#' @return vector; Fermat-Weber point approximation
#' @author Georgios Aliatimis \email{g.aliatimis@lancaster.ac.uk}
#' @references Aliatimis, Georgios, Ruriko Yoshida, Burak Boyaci and James A. Grant (2023). Tropical Logistic Regression on Space of Phylogenetic Trees
#' @examples
#' D = matrix(c(0,0,0,0,2,5,0,3,1),3,3,TRUE)
#' FWpoint.num.w.reg(D,1e4) # (0,2,5/3) not ultrametric
#' FWpoint.num.w.reg(D,1e4) # (0,5/3,5/3) ultrametric

#' @export
#' @rdname FWpoint.regularization
FWpoint.num.w.reg <- function(datamatrix, penalty = 0){
    if (penalty < 0) stop("Penalty should be non-negative.")
    N = dim(datamatrix)[1]
    e = dim(datamatrix)[2]
    fn <- function(par){
        s=sum(apply(datamatrix,1,function(x) trop.dist(x,par)))
        s/N + penalty * regularization_term(par)
    }
    gr <- function(par){
        grad <- rep(0,e)
        for(i in 1:N){
            j_min = which.min(par - datamatrix[i,])
            j_max = which.max(par - datamatrix[i,])
            grad[j_min] = grad[j_min] - 1
            grad[j_max] = grad[j_max] + 1
        }
        grad/N + penalty * regularization_term_grad(par)
    }
    pars.init = colMeans(datamatrix)
    opt <- optim(par=pars.init,fn=fn,gr=gr,method="CG",control=c(maxit = 200,reltol = 1e-8))
    opt$par
}

regularization_term <- function(pars){
  e = length(pars)

  omega = pars[1:e]
  L = clust(omega) - omega
  1/2 * (L %*% L)
}

regularization_term_grad <- function(pars){
  e = length(pars)
  omega  = pars[1:e]
  reg_grad <- rep(0,length(pars))
  EPS = 1e-10

  proj = clust(omega)
  l = proj - omega
  for(j in 1:e){
    if(abs(l[j]) < EPS){
      J = (abs(proj[j] - proj) < EPS)
      reg_grad[j] <- reg_grad[j] + sum(l[J])
    }
  }
  reg_grad
}

clust <- function(omega){
  e = length(omega)
  m = (1+sqrt(1+8*e))/2 # so that m choose 2 = e
  dist_mat = matrix(0,m,m)
  dist_mat[lower.tri(dist_mat)]= omega
  dist_mat = dist_mat + t(dist_mat)
  u = hclust(as.dist(dist_mat),method="complete")
  u = as.phylo(u)
  C =cophenetic(u)
  C[lower.tri(C)]
}
