#'Draw a 2-D or 3-D tropical polytope
#'
#'This command draws a three dimensional tropical polytope
#'
#'@param D matrix of vertices of a tropical polytope; rows are the vertices
#'@param col_lines string; color to render the polytope.
#'@param col_verts string; color to render the vertices.
#'@param plot logical; initiate new plot visualization or not.
#'@param add string; 'max' indicates max-plus addition, 'min' indicates min-plus
#'  addition. Defaults to 'max'
#'@return 2-D or 3-D rendering of a tropical polytope.
#'@author Ruriko Yoshida \email{ryoshida@@nps.edu}
#'@name draw.tpolytope
#'@import rgl
#' @examples
#'D <-matrix(c(0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1),4,4,TRUE)
#'col_lines<-'blue'
#'col_verts<-'red'
#'draw.tpolytope.3d(D,col_lines,col_verts,plot=TRUE)
#'draw.tpolytope.3d(D,col_lines,col_verts,plot=TRUE,add='min')
#'
#'D <- matrix(c(0,-2,2,0,-2,5,0,2,1,0,1,-1),4,3,TRUE)
#'col_lines <- 'blue'
#'col_verts <- 'red'
#'draw.tpolytope.2d(D,col_lines,col_verts,plot=TRUE)
#'draw.tpolytope.2d(D,col_lines,col_verts,plot=TRUE,add='min')
#'

### D is the set of vertices for tropical polytope with row is a vertex
### Only for e = 4.
#' @rdname draw.tpolytope
#' @export
draw.tpolytope.3d <- function(D, col_lines, col_verts, plot=TRUE, add='max'){
  d <- dim(D)
  D1 <- D
  if(add=='max'){
    for(i in 1:(d[1] - 1)){
      for(j in (i+1):d[1]){
        M <- Points.TLineSeg(D[i, ], D[j, ],add='max')
        D1 <- rbind(D1, M)
      }
    }
  }
  if(add=='min'){
    for(i in 1:(d[1] - 1)){
      for(j in (i+1):d[1]){
        M <- Points.TLineSeg(D[i, ], D[j, ],add='min')
        D1 <- rbind(D1, M)
      }
    }
  }
  pre.draw.tpolytope.3d(D1, d[1],col_lines,col_verts,plot,add=add)

}

#' @rdname draw.tpolytope
#' @export
draw.tpolytope.2d<-function(D, col_lines, col_verts, plot=TRUE, add='max'){
  coms<-combn(c(1:nrow(D)),2)
  if(plot==TRUE){
    plot(D[,2],D[,3],asp=1,pch=19,col='white',xlab='x2',ylab='x3')
  }
  if(add=='max'){
  for (i in 1:ncol(coms)){
    a<-coms[1,i]
    b<-coms[2,i]
    LS<-lapply(TLineSeg(D[a,],D[b,]),function(x) normaliz.vector(x))
    for (i in 2:length(LS)){
      lines(c(LS[[i-1]][2],LS[[i]][2]),c(LS[[i-1]][3],LS[[i]][3]),col=col_lines)
    }
  }
  }
  if(add=='min'){
    for (i in 1:ncol(coms)){
      a<-coms[1,i]
      b<-coms[2,i]
      LS<-lapply(TLineSeg(D[a,],D[b,],add='min'),function(x) normaliz.vector(x))
      for (i in 2:length(LS)){
        lines(c(LS[[i-1]][2],LS[[i]][2]),c(LS[[i-1]][3],LS[[i]][3]),col=col_lines)
      }
    }
  }
  points(D[,2],D[,3],pch=19,col=col_verts)
}

pre.draw.tpolytope.3d <- function(D, v,col_lines,col_verts,plot=TRUE,add='max'){
  d <- dim(D)
  seg <- matrix(rep(0, 3*choose(d[1], 2)*(2*3)), 3*choose(d[1], 2), 6)
  counter <- 1
  if(add=='max'){
  for(i in 1:(d[1] - 1)){
    for(j in (i+1):d[1]){
      t <- TLineSeg(D[i, ], D[j, ],add='max')
      for(k in 1:3){
        seg[counter, 1:3] <- normaliz.vector(t[[k]])[2:4]
        seg[counter, 4:6] <- normaliz.vector(t[[k+1]])[2:4]
        counter <- counter + 1
      }
    }
  }
  }
  if(add=='min'){
    for(i in 1:(d[1] - 1)){
      for(j in (i+1):d[1]){
        t <- TLineSeg(D[i, ], D[j, ],add='min')
        for(k in 1:3){
          seg[counter, 1:3] <- normaliz.vector(t[[k]])[2:4]
          seg[counter, 4:6] <- normaliz.vector(t[[k+1]])[2:4]
          counter <- counter + 1
        }
      }
    }
  }


  segments3d(x=as.vector(t(seg[1:(counter-1), c(1,4)])),
             y=as.vector(t(seg[1:(counter-1), c(2,5)])),
             z=as.vector(t(seg[1:(counter-1), c(3,6)])), col = col_lines, lwd = .2,tcl=-.9)
  for(i in 1:v)
    spheres3d(D[i, 2:4], radius = 0.05, color = col_verts)
  if(plot==TRUE){
    axes3d()
    title3d(xlab="X",ylab="Y",zlab="Z")
  }

}
