#'@name gnsConc
#'@aliases gnsConc
#'@title To compute the concentration
#'@description Function \code{gnsConc} computes the concentration on the basis of
#'formula Y = MX + C, from nromalized or standardized intensity
#'@param Y Normalized or standardized intensity
#'@param M slope
#'@param C intercept
#'@return X value, which is the concentration
#'@details Allows the user to compute concentration from intensity
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@keywords gnsdt
#'@examples
#'## m and c are the values obtained from the linear model equation
#'## y is the normalized or standardized intensity for which
#'##concentration is to be calculated
#'concentration <- gnsConc(0.45,0.0213,0.346)
#'concentration
#'@export
gnsConc <- function(y, m, c){
  x <- (y-c)/m
  x
}
