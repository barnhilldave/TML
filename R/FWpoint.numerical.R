#' Fermat-Weber point numerical solver
#'
#' Returns the Fermat-Weber point of N points using a gradient based numerical
#' method
#'
#' @param datamatrix matrix of dimension N*e, where N is the number of
#'   observations which lie in R^e.
#' @return Fermat-Weber point approximation (vector in R^e)
#' @author Georgios Aliatimis \email{g.aliatimis@lancaster.ac.uk}
#' @references Aliatimis, Georgios, Ruriko Yoshida, Burak Boyaci and James A.
#'   Grant (2023). Tropical Logistic Regression on Space of Phylogenetic Trees
#' @export
#' @examples
#' D = matrix(c(0,0,0,0,2,5,0,3,1),3,3,TRUE)
#' FWpoint.numerical(D)

FWpoint.numerical <- function(datamatrix){
    N = dim(datamatrix)[1]
    e = dim(datamatrix)[2]
    fn <- function(par){
        s=sum(apply(datamatrix,1,function(x) trop.dist(x,par)))
        s/N
    }
    gr <- function(par){
        grad <- rep(0,e)
        for(i in 1:N){
            j_min = which.min(par - datamatrix[i,])
            j_max = which.max(par - datamatrix[i,])
            grad[j_min] = grad[j_min] - 1
            grad[j_max] = grad[j_max] + 1
        }
        grad/N
    }
    pars.init = colMeans(datamatrix)
    opt <- optim(par=pars.init,fn=fn,gr=gr,method="CG",control=c(maxit = 200,reltol = 1e-8))
    return(opt$par)
}
