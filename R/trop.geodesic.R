#' Construct geodesic between points in the tropical projective torus
#'
#' Construct a tropical geodesic between points in the tropical projective torus
#' @param D1 point in the tropical projective torus
#' @param D2 point in the tropical projective torus
#' @return list with the tropical line segment between the two points and the distance between points
#' @author Ruriko Yoshida \email{ryoshida@@nps.edu}
#' @noRd
tropical.geodesic.dim.2 <- function(D1, D2, flag = 0){
  k <- length(D1)
  if(k != 2) warning("dimension has to be 2!")
  for(i in 1:k)
    D1[i] <- round(D1[i], 4)
  for(i in 1:k)
    D2[i] <- round(D2[i], 4)
  if(length(D2) != k)
    warning("dimension is wrong!")
  addd <- 0
  if(flag == 1){
    tmp.D <- D2
    D2 <- D1
    D1 <- tmp.D
  }
  tmp.metric <- (D2 - D1)
  sorted.tmp.metric <- sort.int(tmp.metric, index.return=TRUE)
  D <- rep(0, k)

  D[sorted.tmp.metric$ix[2]] <- D2[sorted.tmp.metric$ix[2]]
  D[sorted.tmp.metric$ix[1]] <- min(D2[sorted.tmp.metric$ix[2]] - D1[sorted.tmp.metric$ix[2]] + D1[sorted.tmp.metric$ix[1]], D1[sorted.tmp.metric$ix[1]])

  distance <- max(abs(D1 - D))
  distance <- distance + max(abs(D2 - D))

  segment <- matrix(rep(0, 6), nrow=2, ncol=3)
  segment[,1] <- D1
  segment[,2] <- D
  segment[,3] <- D2

  return(list(segment, distance))
}
