#'@name gnsmodel
#'@aliases gnsmodel
#'@title To generate the linear fitted plots of lm(Normalized intensity ~ Concentration)
#'and lm(Standardized intensity ~ Cocentration)
#'@description Function \code{gnsmodel} generates the plots of linear model of
#'normalized and standardized intensities vs Concentration. It works on the output
#'of CRepsd, which is an object of class \code{gnsdt} and utilized the combined
#'replicates of normalized and standardized intensities to fit linearly with
#'concentration data.
#'@param gnsdt output of function \code{CRepsd}, which is an object of class \Code{gnsdt}
#'@param Normal choose between TRUE or FALSE, where TRUE is default. When TRUE, then returns
#'the plots of lm(normalized intensity~Concentration) and when FALSE, then returns the
#'plots of lm(standardized intensity~Concentration)  
#'@return linearly fitted plots
#'@details Allows the user to get the linearly fitted plots of normalized and standardized
#'intensities after combining replicates with respect to concentration
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@keywords gnsdt
#'@examples
#'##To read the .csv file
#'file1 <- system.file("exData", "Imager.csv", package = "ReadqPCR")
#'data <- readGNS(file = file1)
#'## to combine the replicates of normalized and standardized intensities
#'crep1 <- CRepsd(data)
#'## to get the linearly fitted plot of normalized intensities vs concentration
#'gnsmodel(crep1)
#'## to get the linearly fitted plots of standardized intensities vs concentration
#'gnsmodel(crep1, Normal = FALSE)
#'@export
setMethod("gnsmodel", signature = "gnsdt", definition =
            
            function (gnsdt, Normal = TRUE){
              crep <- slot(gnsdt,"cData")
              nidata <- crep[,"NI"]
              sidata <- crep[,"SI"]
              Concn <- slot(gnsdt,"conc")
              Concn.new <- Concn[(1:nrow(crep)),]
              sd <- slot(gnsdt,"sdnsData")
              sd.ni <- sd[,"NI"]
              sd.si <- sd[,"SI"]
              ni.new <- as.data.frame(cbind(Concn.new,nidata,sd.ni))
              names(ni.new) <- c("x","y","sd")
              si.new <- as.data.frame(cbind(Concn.new,sidata,sd.si))
              names(si.new) <- c("x","y","sd")
              
              if (Normal == TRUE){
                plot1 <- ggplot(data =ni.new, aes(x,y))+ ggtitle("Intensity vs Concentration")+
                  geom_point(colour = "black", na.rm = TRUE)+
                  geom_smooth(method = "lm", formula = y~x, se = FALSE, colour = "black")+
                  geom_errorbar(aes(ymin= y-sd, ymax=y+sd), width=.2,
                                position=position_dodge(0.05))+
                  xlab("Concentration (nM)") +
                  ylab("Normalized Intensity (cl/tl) [arbitrary unit]")+
                  stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label..,
                                                                sep = "~~~")), 
                               parse = TRUE)
                
              }
              
              else if (Normal == FALSE){
                plot1 <- ggplot(data =si.new, aes(x,y))+ ggtitle("Intensity vs Concentration")+
                  geom_point(colour = "black", na.rm = TRUE)+
                  geom_smooth(method = "lm", formula = y~x, se = FALSE, colour = "black")+
                  geom_errorbar(aes(ymin= y-sd, ymax=y+sd), width=.2,
                                position=position_dodge(0.05))+
                  xlab("Concentration (nM)") +
                  ylab("Standardized Intensity (tl/cl) [arbitrary unit]")+
                  stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label..,
                                                                sep = "~~~")), 
                               parse = TRUE)
              }
              
              else{
                warning("Please mention Normal as TRUE or FALSE")
              }
              
              plot1
              
            })