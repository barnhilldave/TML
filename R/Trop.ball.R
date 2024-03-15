#' Visualize a Tropical ball in 2D or 3D
#'
#' This function constructs a visualization of a 2D or 3D tropical ball
#'
#' @param v center of tropical ball; numeric vector of length 3 or 4
#' @param d radius of tropical ball
#' @param a shading level; 1 is opaque
#' @param cls string indicating color of interior of ball
#' @param cent.col string indicating color of center point
#' @param fil logical for 3D plots; if TRUE 2D facets of 3D ball fill in color
#'   of cls parameter
#' @param plt logical; indicates plot a new object; defaults to TRUE; if FALSE,
#'   overlays the ball on existing plot
#' @param bord string indicating color of border of ball (only for 2D plots)
#' @return 2D or 3D visualization of tropical ball
#' @author David Barnhill \email{david.barnhill@@nps.edu}
#' @export
#' @examples
#' v <-c(0,0,0)
#' d <- 2
#' Trop_ball(v,d,a=.1,cls='white',cent.col='black',fil=TRUE,plt=TRUE,bord='black')
#' v <-c(0,0,0,0)
#' d <- 2
#' Trop_ball(v,d,a=1,cls='red',cent.col='black',fil=FALSE,plt=TRUE)

Trop_ball<-function(v,d,a=1,cls='black',cent.col='black',fil=TRUE,plt=TRUE,bord='black'){
  dm<-length(v)-1
  if(dm==2){
    if (plt==TRUE){
      plot(v[2],v[3],type='n',xlim =c(v[2]-d-.25,(v[2]+d+.25)) ,
           ylim =c(v[3]-d-.25,(v[3]+d+.25)) ,
           xlab='x2',ylab='x3',asp=1)
    }
    polygon(c(v[2]-d,v[2]-d,v[2],v[2]+d,v[2]+d,v[2]),c(v[3]-d,v[3],v[3]+d,v[3]+d,v[3],v[3]-d),col=cls,density=70,border = bord,lwd=2)
    points(v[2],v[3],pch=19,col=cent.col)
  }

  if(dm==3){
    if (plt==TRUE){
      plot3d(v[2],v[3],v[4],pch=19,col=cent.col,size=6,xlim =c(v[2]-d-.25,(v[2]+d+.25)) ,
             ylim =c(v[3]-d-.25,(v[3]+d+.25)) ,
             zlim=c(v[4]-d-.25,(v[4]+d+.25)),
             xlab='x2',ylab='x3',zlab='x4',asp=1)
    }

    polygon3d(c(v[2]-d,v[2]-d,v[2]-d,v[2]-d),c(v[3],v[3]-d,v[3]-d,v[3]),c(v[4],v[4],v[4]-d,v[4]-d),
              coords = c(2,3),fill = fil,alpha=a,col=cls)
    polygon3d(c(v[2],v[2]-d,v[2]-d,v[2]),c(v[3]-d,v[3]-d,v[3]-d,v[3]-d),c(v[4],v[4],v[4]-d,v[4]-d),
              coords = c(1,3),fill = fil,alpha=a,col=cls)
    polygon3d(c(v[2],v[2]-d,v[2]-d,v[2]),c(v[3],v[3],v[3]-d,v[3]-d),c(v[4]-d,v[4]-d,v[4]-d,v[4]-d),
              coords = c(1,2),fill = fil,alpha=a,col=cls)
    polygon3d(c(v[2]+d,v[2]+d,v[2]+d,v[2]+d),c(v[3],v[3]+d,v[3]+d,v[3]),c(v[4],v[4],v[4]+d,v[4]+d),
              coords = c(2,3),fill = fil,alpha=a,col=cls)
    polygon3d(c(v[2],v[2]+d,v[2]+d,v[2]),c(v[3]+d,v[3]+d,v[3]+d,v[3]+d),c(v[4],v[4],v[4]+d,v[4]+d),
              coords = c(1,3),fill = fil,alpha=a,col=cls)
    polygon3d(c(v[2],v[2]+d,v[2]+d,v[2]),c(v[3],v[3],v[3]+d,v[3]+d),c(v[4]+d,v[4]+d,v[4]+d,v[4]+d),
              coords = c(1,2),fill = fil,alpha=a,col=cls)
    polygon3d(c(v[2]-d,v[2],v[2],v[2]-d),c(v[3],v[3]+d,v[3]+d,v[3]),c(v[4],v[4]+d,v[4],v[4]-d),
              coords = c(2,3),fill = fil,alpha=a,col=cls)
    polygon3d(c(v[2]-d,v[2],v[2],v[2]-d),c(v[3],v[3]+d,v[3],v[3]-d),c(v[4],v[4]+d,v[4]+d,v[4]),
              coords = c(2,3),fill = fil,alpha=a,col=cls)
    polygon3d(c(v[2]-d,v[2],v[2]+d,v[2]),c(v[3]-d,v[3],v[3],v[3]-d),c(v[4],v[4]+d,v[4]+d,v[4]),
              coords = c(1,2),fill = fil,alpha=a,col=cls)
    polygon3d(c(v[2],v[2]+d,v[2]+d,v[2]),c(v[3]-d,v[3],v[3],v[3]-d),c(v[4],v[4]+d,v[4],v[4]-d),
              coords = c(2,3),fill = fil,alpha=a,col=cls)
    polygon3d(c(v[2],v[2],v[2]+d,v[2]+d),c(v[3]-d,v[3],v[3]+d,v[3]),c(v[4]-d,v[4]-d,v[4],v[4]),
              coords = c(2,3),fill = fil,alpha=a,col=cls)
    polygon3d(c(v[2]-d,v[2],v[2]+d,v[2]),c(v[3],v[3],v[3]+d,v[3]+d),c(v[4]-d,v[4]-d,v[4],v[4]),
              coords = c(1,2),fill = fil,alpha=a,col=cls)
    if (plt==FALSE){
      points3d(v[2],v[3],v[4],pch=19,col='white',size=6)
    }
  }
}
