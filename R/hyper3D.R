#' 2D or 3D rendering of max-plus or min-plus tropical hyperplane
#'
#' This function renders a 2D or 3D max-plus or min-plus tropical hyperplane
#'
#' @param D point in the tropical projective torus representing the apex of the
#'   hyperplane
#' @param ext scalar; indicates how far the hyperplane should extend
#' @param min.ax scalar; value applied to define the minimum limits of the axes
#'   of the plot
#' @param max.ax scalar; value applied to define the maximum limits of the axes
#'   of the plot
#' @param plot logical; if true produces a new plot otherwise overlays tropical
#'   hyperplane on existing plot
#' @param add string; 'max' indicates max-plus addition, 'min' indicates
#'   min-plus addition. Defaults to 'max'
#' @return 2D or 3D rendering of max-plus or min-plus tropical hyperplane
#' @author David Barnhill \email{david.barnhill@@nps.edu}
#' @name hyper_3D
#' @examples
#' # 2D Example
#' D <-c(0,0,0)
#' ext<-4
#' min.ax<- 5
#' max.ax<- 5
#' hyper3d(D,ext,min.ax,max.ax,plot=TRUE)
#'
#' # 3D Example
#' D <-c(0,0,0,0)
#' ext<-4
#' min.ax<- 5
#' max.ax<- 5
#' hyper3d(D,ext,min.ax,max.ax,plot=TRUE)
#' hyper3d(D,ext,min.ax,max.ax,plot=TRUE,add='min')

#' @export
#' @rdname hyper_3D
hyper3d<-function(D,ext,min.ax,max.ax,plot=FALSE,add='max'){
  D<-t(as.matrix(D))
  cl<-rainbow(nrow(D))
  d<-dim(D)
  if((d[2]-1)==3){
    if(plot==TRUE){
      plot3d(D[,2],D[,3],D[,4],type='n',xlim=c(D[2]-min.ax-.1*min.ax,D[2]+max.ax+.1*max.ax),ylim=c(D[3]-min.ax-.1*min.ax,D[3]+max.ax+.1*max.ax),zlim=c(D[4]-min.ax-.1*min.ax,D[4]+max.ax+.1*max.ax),xlab='x2',ylab = 'x3',zlab='x4',asp=1)
    }
    if(add=='max'){
    for (i in 1:(nrow(D))){
      x<-D[i,]
      xs1<-c(x[2],x[2]+ext,x[2]+ext,x[2])
      ys1<-c(x[3],x[3]+ext,x[3]+ext,x[3])
      zs1<-c(x[4],x[4]+ext,x[4]-ext,x[4]-ext)

      xs2<-c(x[2],x[2]+ext,x[2]+ext,x[2])
      ys2<-c(x[3],x[3]+ext,x[3]-ext,x[3]-ext)
      zs2<-c(x[4],x[4]+ext,x[4]+ext,x[4])

      xs3<-c(x[2],x[2]+ext,x[2]-ext,x[2]-ext)
      ys3<-c(x[3],x[3]+ext,x[3]+ext,x[3])
      zs3<-c(x[4],x[4]+ext,x[4]+ext,x[4])

      xs4<-c(x[2],x[2]-ext,x[2]-ext,x[2])
      ys4<-c(x[3],x[3],x[3]-ext,x[3]-ext)
      zs4<-c(x[4],x[4],x[4],x[4])

      xs5<-c(x[2],x[2]-ext,x[2]-ext,x[2])
      ys5<-c(x[3],x[3],x[3],x[3])
      zs5<-c(x[4],x[4],x[4]-ext,x[4]-ext)

      xs6<-c(x[2],x[2],x[2],x[2])
      ys6<-c(x[3],x[3],x[3]-ext,x[3]-ext)
      zs6<-c(x[4],x[4]-ext,x[4]-ext,x[4])

      polygon3d(xs1,ys1,zs1,coords=c(1,3),col=cl[i])
      polygon3d(xs2,ys2,zs2,col=cl[i],coords = c(2,3))
      polygon3d(xs3,ys3,zs3,col=cl[i],coords=c(1,2))
      polygon3d(xs4,ys4,zs4,col=cl[i],coords = c(1,2),plot = TRUE)
      polygon3d(xs5,ys5,zs5,col=cl[i],coords = c(1,3),plot = TRUE)
      polygon3d(xs6,ys6,zs6,col=cl[i],coords = c(2,3),plot=TRUE)
      }
    }
    if(add=='min'){
      for (i in 1:(nrow(D))){
        x<-D[i,]
        xs1<-c(x[2],x[2]-ext,x[2]-ext,x[2])
        ys1<-c(x[3],x[3]-ext,x[3]-ext,x[3])
        zs1<-c(x[4],x[4]-ext,x[4]+ext,x[4]+ext)

        xs2<-c(x[2],x[2]-ext,x[2]-ext,x[2])
        ys2<-c(x[3],x[3]-ext,x[3]+ext,x[3]+ext)
        zs2<-c(x[4],x[4]-ext,x[4]-ext,x[4])

        xs3<-c(x[2],x[2]-ext,x[2]+ext,x[2]+ext)
        ys3<-c(x[3],x[3]-ext,x[3]-ext,x[3])
        zs3<-c(x[4],x[4]-ext,x[4]-ext,x[4])

        xs4<-c(x[2],x[2]+ext,x[2]+ext,x[2])
        ys4<-c(x[3],x[3],x[3]+ext,x[3]+ext)
        zs4<-c(x[4],x[4],x[4],x[4])

        xs5<-c(x[2],x[2]+ext,x[2]+ext,x[2])
        ys5<-c(x[3],x[3],x[3],x[3])
        zs5<-c(x[4],x[4],x[4]+ext,x[4]+ext)

        xs6<-c(x[2],x[2],x[2],x[2])
        ys6<-c(x[3],x[3],x[3]+ext,x[3]+ext)
        zs6<-c(x[4],x[4]+ext,x[4]+ext,x[4])

        polygon3d(xs1,ys1,zs1,col=cl[i],coords = c(1,3))
        polygon3d(xs2,ys2,zs2,col=cl[i],coords=c(1,2))
        polygon3d(xs3,ys3,zs3,col=cl[i],coords=c(1,2))
        polygon3d(xs4,ys4,zs4,col=cl[i],coords = c(1,2),plot = TRUE)
        polygon3d(xs5,ys5,zs5,col=cl[i],coords = c(1,3),plot = TRUE)
        polygon3d(xs6,ys6,zs6,col=cl[i],coords = c(2,3),plot=TRUE)
      }
    }
  }
  else{
    if(plot==TRUE){
      plot(D[,2],D[,3],type='n',xlim=c(D[,2]-min.ax-.1*min.ax,D[,2]+max.ax+.1*max.ax),ylim=c(D[,3]-min.ax-.1*min.ax,D[,3]+max.ax+.1*max.ax),xlab='x2',ylab = 'x3',asp=1)
    }
    if(add=='max'){
    for (i in 1:(nrow(D))){
      x<-D[i,]
      xs1<-c(x[2],x[2]+ext)
      ys1<-c(x[3],x[3]+ext)

      xs2<-c(x[2],x[2]-ext)
      ys2<-c(x[3],x[3])

      xs3<-c(x[2],x[2])
      ys3<-c(x[3],x[3]-ext)

      lines(xs1,ys1,col=cl[i])
      lines(xs2,ys2,col=cl[i])
      lines(xs3,ys3,col=cl[i])
    }
    }
    if(add=='min'){
      for (i in 1:(nrow(D))){
        x<-D[i,]
        xs1<-c(x[2],x[2]-ext)
        ys1<-c(x[3],x[3]-ext)

        xs2<-c(x[2],x[2]+ext)
        ys2<-c(x[3],x[3])

        xs3<-c(x[2],x[2])
        ys3<-c(x[3],x[3]+ext)

        lines(xs1,ys1,col=cl[i])
        lines(xs2,ys2,col=cl[i])
        lines(xs3,ys3,col=cl[i])
      }
    }
    }
}
