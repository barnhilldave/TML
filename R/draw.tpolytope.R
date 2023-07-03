#' Draw a 3-D tropical polytope
#'
#' This command draws a three dimensional tropical polytope
#'
#' @param D matrix of vertices of a tropical polytope; rows are the vertices
#' @param c string; color to render the polytope.
#' @return 3-D rendering of a tropical polytope.
#' @author Ruriko Yoshida \email{ryoshida@@nps.edu}
#' @name draw.tpolytope
#' @import rgl
#' @examples
#'D <-matrix(c(0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1),4,4,TRUE)
#'c<-'blue'
#'
#'draw.tpolytope.3d(D,c)
#'

### D is the set of vertices for tropical polytope with row is a vertex
### Only for e = 4.
#' @rdname draw.tpolytope
#' @export
draw.tpolytope.3d <- function(D,c){
  d <- dim(D)
  D1 <- D
  for(i in 1:(d[1] - 1)){
    for(j in (i+1):d[1]){
      M <- Points.TLineSeg(D[i, ], D[j, ])
      D1 <- rbind(D1, M)
    }
  }
  pre.draw.tpolytope.3d(D1, d[1],c)

}

pre.draw.tpolytope.3d <- function(D, v,c){
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
    spheres3d(D[i, 2:4], radius = 0.1, color = "black")
  axes3d()
  title3d(xlab="X",ylab="Y",zlab="Z")
}
