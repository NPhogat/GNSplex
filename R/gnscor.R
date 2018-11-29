#'@name gnscor
#'@aliases gnscor
#'@title To compute the correlation of normalized and standardized intensities with
#'respect to their corresponding predicted values of linearly fitted model
#'@description Function \code{gnscor} works on the output of function CRepsd, which is
#'an object of class \code{gnsdt} and computes the correlation between normalized
#'intensity with predicted values of normalized intensities from lm model as well as
#'correlation between standardized intensities with predicted values of standardized
#'intensities from lm model. It saves the final results of correlation in a new slot
#'corData of an object of class \Code{gnsdt}
#'@param gnsdt an output of function CRepsd, which is an object of the class
#'\Code{gnsdt}
#'@return object of Class \code{"gnsdt"} and save the results of correlation in
#'slot corData
#'@details Allows the user to compute the correlation and saves the results in the new
#'slot corData of an object of class \code{gnsdt}
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@keywords gnsdt
#'@examples
#'##To read the .csv file
#'file1 <- system.file("exData", "Imager.csv", package = "ReadqPCR")
#'data <- readGNS(file = file1)
#'## To combine the replicates
#'crep1 <- CRepsd(data)
#'## To compute the correlation
#'cor1 <- gnscor(crep1)
#'## to visualize all the results
#'cor1
#'## to visualize the results of correlation only
#'slot(cor1,"corData")
#'@export
setMethod("gnscor", signature = "gnsdt", definition =
            
            function (gnsdt){
              x.rep <- slot(gnsdt,"cData")
              x.ni <- x.rep[,"NI"]
              x.si <- x.rep[,"SI"]
              x.Concn <- slot(gnsdt,"conc")
              x.Concn.new <- x.Concn[(1:nrow(x.rep)),]
              x.ni <- as.vector(as.numeric(x.ni))
              x.si <- as.vector(as.numeric(x.si))
              x.Concn.new <- as.vector(as.numeric(x.Concn.new))
              model.ni <- lm(x.ni~x.Concn.new)
              model.si <- lm(x.si~x.Concn.new)
              ni.pred <- predict(model.ni)
              si.pred <- predict(model.si)
              ni.cor <- cor(x.ni,ni.pred)
              si.cor <- cor(x.si,si.pred)
              correlation <- as.data.frame(cbind(ni.cor, si.cor))
              names(correlation) <- c("NI_cor", "SI_cor")
              res <- new("gnsdt", corData = correlation)
              res
              
            })