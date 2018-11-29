#'@name gnsConf
#'@aliases gnsConf
#'@title To compute the confidence interval of combined replicates of normalized and standardized
#'intensities
#'@description Function \code{gnsConf} works on the output of function CRepsd, which is an
#'object of S4 class \Code{gnsdt} and computes the confidence interval of combined replicates
#'of normalized and standardized intensities and save them in a new slot \code{confData} of
#'Class \code{gnsdt}
#'@param gnsdt an output of function CRepsd, which is an object of class \code{gnsdt}
#'@return object of Class \code{"gnsdt"} and save the results in slot confData of an object
#'of class \code{gnsdt}
#'@details Allows the user to compute confidence interval of combined replicates of normalized and
#'standardized intensities and populate an object of class \code{gnsdt}
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@keywords gnsdt
#'@examples
#'##To read the .csv file
#'file1 <- system.file("exData", "Imager.csv", package = "ReadqPCR")
#'data <- readGNS(file = file1)
#'## to combine the replicates of standardized and normalized intensities
#'crep1 <- CRepsd(data)
#'## to get confidence interval
#'conf1 <- gnsConf(crep1)
#'## to visualize all the results
#'conf1
#'## to visualize only the confidence interval
#'conf_data <- slot(conf1,"confData)
#'conf_data
#'## to visualize the confidence interval of combined replicates of normalized intensity
#'conf_data[,"NI"]
#'## to visualize the confidence interval of combined replicates of standardized intensity
#'conf_data[,"SI"]
#'@export
setMethod("gnsConf", signature = "gnsdt", definition =

            function (gnsdt){
              x.ns <- slot(gnsdt,"cData")
              x1 <- x.ns[,"NI"]
              x2 <- x.ns[,"SI"]
              calcconf <- function(x){
                x <- x[!is.na(x)]
                x.se <- sd(x)/sqrt(length(x))
                x2 <- as.numeric(x)
                x.mean <- mean(x2)
                ci.min <- (x.mean - 2*x.se)
                ci.max <- (x.mean + 2*x.se)
                ci <- as.data.frame(cbind(ci.min, ci.max))
                colnames(ci) <- c("Min.Value", "Max.Value")
                ci
              }

              conf1 <- calcconf(x1)
              conf2 <- calcconf(x2)
              confnew <- rbind(conf1,conf2)
              resdata <- new("gnsdt",confData = confnew)

              resdata

            })
