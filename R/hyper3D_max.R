#' 2D or 3D rendering of max-plus or min-plus tropical hyperplane
#'
#' This function renders a 2D or 3D max-plus or min-plus tropical hyperplane
#'
#' @param D point in the tropical projective torus representing the apex of the hyperplane
#' @param di scalar; indicates how far the hyperplane should extend
#' @param mi scalar; minimum value on axes of the plot
#' @param ma scalar; maximum value on axes of the plot
#' @param plt logical; if true produces a new plot otherwise overlays tropical hyperplane on existing plot
#' @return 2D or 3D rendering of max-plus or min-plus tropical hyperplane
#' @author David Barnhill \email{david.barnhill@@nps.edu}
#' @name hyper_3D
#' @examples
#' # 2D Example
#' D <-t(as.matrix(c(0,0,0)))
#' di<-4
#' mi<- -5
#' ma<-5
#' hyper3d_max(D,di,mi,ma,plt=TRUE)
#' hyper3d_min(D,di,mi,ma,plt=TRUE)
#'
#' # 3D Example
#' D <-t(as.matrix(c(0,0,0,0)))
#' di<-4
#' mi<- -5
#' ma<-5
#' hyper3d_max(D,di,mi,ma,plt=TRUE)
#' hyper3d_min(D,di,mi,ma,plt=TRUE)

#' @export
#' @rdname hyper_3D
hyper3d_max<-function(D,di,mi,ma,plt=FALSE){
  cl<-rainbow(nrow(D))
  d<-dim(D)
  if((d[2]-1)==3){
    if(plt==TRUE){
      plot3d(D[,2],D[,3],D[,4],type='n',xlim=c(mi,ma),ylim=c(mi,ma),zlim=c(mi,ma),xlab='x2',ylab = 'x3',zlab='x4',asp=1)
    }
    for (i in 1:(nrow(D))){
      x<-D[i,]
      xs1<-c(x[2],x[2]+di,x[2]+di,x[2])
      ys1<-c(x[3],x[3]+di,x[3]+di,x[3])
      zs1<-c(x[4],x[4]+di,x[4]-di,x[4]-di)

      xs2<-c(x[2],x[2]+di,x[2]+di,x[2])
      ys2<-c(x[3],x[3]+di,x[3]-di,x[3]-di)
      zs2<-c(x[4],x[4]+di,x[4]+di,x[4])

      xs3<-c(x[2],x[2]+di,x[2]-di,x[2]-di)
      ys3<-c(x[3],x[3]+di,x[3]+di,x[3])
      zs3<-c(x[4],x[4]+di,x[4]+di,x[4])

      xs4<-c(x[2],x[2]-di,x[2]-di,x[2])
      ys4<-c(x[3],x[3],x[3]-di,x[3]-di)
      zs4<-c(x[4],x[4],x[4],x[4])

      xs5<-c(x[2],x[2]-di,x[2]-di,x[2])
      ys5<-c(x[3],x[3],x[3],x[3])
      zs5<-c(x[4],x[4],x[4]-di,x[4]-di)

      xs6<-c(x[2],x[2],x[2],x[2])
      ys6<-c(x[3],x[3],x[3]-di,x[3]-di)
      zs6<-c(x[4],x[4]-di,x[4]-di,x[4])

      polygon3d(xs1,ys1,zs1,coords=c(1,3),col=cl[i])
      polygon3d(xs2,ys2,zs2,col=cl[i],coords = c(2,3))
      polygon3d(xs3,ys3,zs3,col=cl[i],coords=c(1,2))
      polygon3d(xs4,ys4,zs4,col=cl[i],coords = c(1,2),plot = TRUE)
      polygon3d(xs5,ys5,zs5,col=cl[i],coords = c(1,3),plot = TRUE)
      polygon3d(xs6,ys6,zs6,col=cl[i],coords = c(2,3),plot=TRUE)
    }
  }
  else{
    if(plt==TRUE){
      plot(D[,2],D[,3],type='n',xlim=c(mi,ma),ylim=c(mi,ma),xlab='x2',ylab = 'x3',asp=1)
    }
    for (i in 1:(nrow(D))){
      x<-D[i,]
      xs1<-c(x[2],x[2]+di)
      ys1<-c(x[3],x[3]+di)

      xs2<-c(x[2],x[2]-di)
      ys2<-c(x[3],x[3])

      xs3<-c(x[2],x[2])
      ys3<-c(x[3],x[3]-di)

      lines(xs1,ys1,col=cl[i])
      lines(xs2,ys2,col=cl[i])
      lines(xs3,ys3,col=cl[i])
    }
  }
}
#' @export
#' @rdname hyper_3D
hyper3d_min<-function(D,di,mi,ma,plt=FALSE){
  cl<-rainbow(nrow(D))
  d<-dim(D)
  if((d[2]-1)==3){
    if(plt==TRUE){
      plot3d(D[,2],D[,3],D[,4],type='n',xlim=c(mi,ma),ylim=c(mi,ma),zlim=c(mi,ma),xlab='x2',ylab = 'x3',zlab='x4',asp=1)
    }
    for (i in 1:(nrow(D))){
      x<-D[i,]
      xs1<-c(x[2],x[2]-di,x[2]-di,x[2])
      ys1<-c(x[3],x[3]-di,x[3]-di,x[3])
      zs1<-c(x[4],x[4]-di,x[4]+di,x[4]+di)

      xs2<-c(x[2],x[2]-di,x[2]-di,x[2])
      ys2<-c(x[3],x[3]-di,x[3]+di,x[3]+di)
      zs2<-c(x[4],x[4]-di,x[4]-di,x[4])

      xs3<-c(x[2],x[2]-di,x[2]+di,x[2]+di)
      ys3<-c(x[3],x[3]-di,x[3]-di,x[3])
      zs3<-c(x[4],x[4]-di,x[4]-di,x[4])

      xs4<-c(x[2],x[2]+di,x[2]+di,x[2])
      ys4<-c(x[3],x[3],x[3]+di,x[3]+di)
      zs4<-c(x[4],x[4],x[4],x[4])

      xs5<-c(x[2],x[2]+di,x[2]+di,x[2])
      ys5<-c(x[3],x[3],x[3],x[3])
      zs5<-c(x[4],x[4],x[4]+di,x[4]+di)

      xs6<-c(x[2],x[2],x[2],x[2])
      ys6<-c(x[3],x[3],x[3]+di,x[3]+di)
      zs6<-c(x[4],x[4]+di,x[4]+di,x[4])

      polygon3d(xs1,ys1,zs1,col=cl[i],coords = c(1,3))
      polygon3d(xs2,ys2,zs2,col=cl[i],coords=c(1,2))
      polygon3d(xs3,ys3,zs3,col=cl[i],coords=c(1,2))
      polygon3d(xs4,ys4,zs4,col=cl[i],coords = c(1,2),plot = TRUE)
      polygon3d(xs5,ys5,zs5,col=cl[i],coords = c(1,3),plot = TRUE)
      polygon3d(xs6,ys6,zs6,col=cl[i],coords = c(2,3),plot=TRUE)
    }
  }
  else{
    if(plt==TRUE){
      plot(D[,2],D[,3],type='n',xlim=c(mi,ma),ylim=c(mi,ma),xlab='x2',ylab = 'x3',asp=1)
    }
    for (i in 1:(nrow(D))){
      x<-D[i,]
      xs1<-c(x[2],x[2]-di)
      ys1<-c(x[3],x[3]-di)

      xs2<-c(x[2],x[2]+di)
      ys2<-c(x[3],x[3])

      xs3<-c(x[2],x[2])
      ys3<-c(x[3],x[3]+di)

      lines(xs1,ys1,col=cl[i])
      lines(xs2,ys2,col=cl[i])
      lines(xs3,ys3,col=cl[i])
    }
  }
}
