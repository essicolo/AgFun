# Determine if a point is inside a given polygon or not
# Polygon is a list of (x,y) pairs. This fuction
# returns True or False.  The algorithm is called
# "Ray Casting Method".
# polygon: first point is the (1) higher - (2) left corner, then clockwise.

is.inpoly <- function (x,y,poly) {
  n <- nrow(poly)
  inside <- FALSE
  
  p1x<-poly[1,1]
  p1y<-poly[1,2]
  
  for (i in 0:n+1) {
    p2x <- poly[i%%n+1,1]
    p2y <- poly[i%%n+1,2]
    if (y > min(c(p1y,p2y))) {
      if (y <= max(c(p1y,p2y))) {
        if (x <= max(c(p1x,p2x))) {
          if (p1y != p2y) {
            xinters <- (y-p1y)*(p2x-p1x)/(p2y-p1y)+p1x
          }
          if (p1x == p2x | x <= xinters) {
            if (inside == FALSE) {inside <- TRUE}
            else if (inside == TRUE) {inside <- FALSE}
          }
        }
      }
    }
    p1x <- p2x
    p1y <- p2y
  }
  return(inside)
}