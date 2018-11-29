#'@name gnsLOD
#'@aliases gnsLOD
#'@title To compute the LOB, LOD and LOQ of combined replicates of normalized and standardized
#'intensities
#'@description Function \code{gnsLOD} works on the output of the function CRepsd, which is an
#'object of class \code{gnsdt} and computes limit of blank (LOB), limit of detection (LOD)
#'and limit of quantification (LOQ), by two different methods and save the results in the
#'slot lodData of an object of class \code{gnsdt}
#'@param gnsdt an output of function CRepsd, which is an object of class \code{gnsdt}
#'@param LOB it can be either TRUE or FALSE. The default is FALSE. When FALSE, then returns
#'LOD and LOQ, computed by method 1 and when TRUE, then, returns the results of LOB, LOD and
#'LOQ, computed by method 2.
#'@return object of Class \code{"gnsdt"} and save the results in slot lodData of an object
#'of class \code{gnsdt}
#'@details Allows the user to compute LOB, LOD and LOQ of combined replicates of normalized and
#'standardized intensities and populate an object of class \code{gnsdt}
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@keywords gnsdt
#'@examples
#'##To read the .csv file
#'file1 <- system.file("exData", "Imager.csv", package = "ReadqPCR")
#'data <- readGNS(file = file1)
#'## to combine the replicates of standardized and normalized intensities
#'crep1 <- CRepsd(data)
#'## to get the results of LOD and LOQ by method 1
#'lod1 <- gnsLOD(crep1)
#'## to visualize the overall results
#'lod1
#'## to visualize the results of slot lodData
#'lod_new <- slot(lod1,"lodData")
#'lod_new
#'## to visualize the results of LOD and LOQ of normalized Intensity
#'lod_new[,c("lod_ni","loq_ni")]
#'## to visualize the results of LOB, LOD and LOQ of normalized Intensity
#'lod_new[,c(lod_si","loq_si")]
#'## to compute LOB, LOD and LOQ by Method 2
#'lod2 <- gnsLOD(crep1,LOB = TRUE
#'## to visualize the overall results
#'lod2
#'## to visualize the results of slot lodData
#'lod2_new <- slot(lod2,"lodData")
#'## to visualize the results of LOB, LOD and LOQ of normalized intensity
#'lod2_new[,c("lob_ni",""lod_ni","loq_ni")]
#'## to visualize the results of LOB, LOD and LOQ of standardized intensity
#'lod2_new[,c("lob_si","lod_si","loq_si")]
#'## all these calculated values are in terms of intensties (Normalized or standardized
#'##intensities, obtained after combining the replicates. These intensities can be
#'##used to calculate the respective concentration, by using the linear model equation
#'##of lm(Normalized intensity ~ Concentration) or lm(Standardized intensity ~ Concentration)
#'##to compute the respective concetration to get the lob, lod and loq in terms of
#'##concentration)
#'@export
setMethod("gnsLOD", signature = "gnsdt", definition =
            
            function (gnsdt, LOB = FALSE){
              sd <- slot(gnsdt,"sdnsData")
              mean <- slot(gnsdt,"cData")
              x_ni <- sd[1,"NI"]
              m_ni <- mean[1,"NI"]
              x_si <- sd[nrow(sd), "SI"]
              m_si <- mean[nrow(mean),"SI"]
              
              if (LOB == FALSE){
                
                lod_ni <- m_ni + 3*x_ni
                loq_ni <- m_ni + 10*x_ni
                lod_si <- m_si + 3*x_si
                loq_si <- m_si + 10*x_si
                res_data <- as.data.frame(cbind(lod_ni,loq_ni,lod_si, loq_si))
              }
              
              else if (LOB == TRUE){
                x_ni2 <- sd[2,"NI"]
                lob_ni <- m_ni+1.645*(x_ni)
                lod_ni <- lob_ni + 1.645*(x_ni2)
                loq_ni <- m_ni + 10*x_ni
                
                x_si2 <- sd[(nrow(sd)-1),"SI"]
                lob_si <- m_si+1.645*(x_si)
                lod_si <- lob_si+1.645*(x_si2)
                loq_si <- m_si + 10*x_si
                res_data <- as.data.frame(cbind(lob_ni,lod_ni,loq_ni,lob_si,lod_si,loq_si))
              }
              
              res <- new("gnsdt",lodData = res_data)
              res
            })