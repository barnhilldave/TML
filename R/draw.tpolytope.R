#' Draw a 2-D or 3-D tropical polytope
#'
#' This command draws a three dimensional tropical polytope
#'
#' @param D matrix of vertices of a tropical polytope; rows are the vertices
#' @param c string; color to render the polytope.
#' @param cc string; color to render the vertices.
#' @return 2-D or 3-D rendering of a tropical polytope.
#' @author Ruriko Yoshida \email{ryoshida@@nps.edu}
#' @name draw.tpolytope
#' @import rgl
#' @examples
#'D <-matrix(c(0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1),4,4,TRUE)
#'c<-'blue'
#'cc<-'red'
#'draw.tpolytope.3d(D,c,cc)
#'
#'D <- matrix(c(0,-2,2,0,-2,5,0,2,1,0,1,-1),4,3,TRUE)
#'c <- 'blue'
#'cc <- 'red'
#'draw.tpolytope.2d(D,c,cc)
#'
#'

### D is the set of vertices for tropical polytope with row is a vertex
### Only for e = 4.
#' @rdname draw.tpolytope
#' @export
draw.tpolytope.3d <- function(D,c,cc,plt=TRUE){
  d <- dim(D)
  D1 <- D
  for(i in 1:(d[1] - 1)){
    for(j in (i+1):d[1]){
      M <- Points.TLineSeg(D[i, ], D[j, ])
      D1 <- rbind(D1, M)
    }
  }
  pre.draw.tpolytope.3d(D1, d[1],c,cc,plt)

}

#' @rdname draw.tpolytope
#' @export
draw.tpolytope.2d<-function(D,c,cc,plt=TRUE){
  coms<-combn(c(1:nrow(D)),2)
  if(plt==TRUE){
    plot(D[,2],D[,3],asp=1,pch=19,col='white',xlab='x2',ylab='x3')
  }
  for (i in 1:ncol(coms)){
    a<-coms[1,i]
    b<-coms[2,i]
    LS<-lapply(TLineSeg(D[a,],D[b,]),function(x) normaliz.vector(x))
    for (i in 2:length(LS)){
      lines(c(LS[[i-1]][2],LS[[i]][2]),c(LS[[i-1]][3],LS[[i]][3]),col=c)
    }
  }
  points(D[,2],D[,3],pch=19,col=cc)
}

pre.draw.tpolytope.3d <- function(D, v,c,cc,plt=TRUE){
  d <- dim(D)
  seg <- matrix(rep(0, 3*choose(d[1], 2)*(2*3)), 3*choose(d[1], 2), 6)
  counter <- 1
  for(i in 1:(d[1] - 1)){
    for(j in (i+1):d[1]){
      t <- TLineSeg(D[i, ], D[j, ])
      for(k in 1:3){
        seg[counter, 1:3] <- normaliz.vector(t[[k]])[2:4]
        seg[counter, 4:6] <- normaliz.vector(t[[k+1]])[2:4]
        counter <- counter + 1
      }
    }
  }

  segments3d(x=as.vector(t(seg[1:(counter-1), c(1,4)])),
             y=as.vector(t(seg[1:(counter-1), c(2,5)])),
             z=as.vector(t(seg[1:(counter-1), c(3,6)])), col = c, lwd = .2,tcl=-.9)
  for(i in 1:v)
    spheres3d(D[i, 2:4], radius = 0.05, color = cc)
  if(plt==TRUE){
    axes3d()
    title3d(xlab="X",ylab="Y",zlab="Z")
  }

}
