#'@name CRepsd
#'@aliases CRepsd
#'@title To combine the replicates of normalised and standardized intensities and to get
#'standard deviation
#'@description Function \code{CRepsd} works on the output of the function \code{readGNS}, which is
#'an object of Class \code{gnsdt} and populate them in an object of Class \code{gnsdt}
#'@param gnsdt object of the S4 Class \code{gnsdt}
#'@return object of Class \code{"gnsdt"} and save the results in slot cData
#'@details Allows the user to compute normalization and standardization of intensites, to
#'combine the replicates of normalized  and standardized intensities,to compute the standard
#'deviation (S.D.) of normalized and standardized intensities within replicates, to compute the
#'S.D. of intensities of control and test raw data intensity within replicates and save them in
#'different slots: (1)nsData: Normalized and standardized intensities (2)cData: to save
#'combined replicates of normalized and standardized intensities (3) sdnsData: to contain the
#'standard deviation of normalized and standardized intensities within replicates (4) sdData: to
#'contain the standard deviation of control and test line raw intensity data within replicates
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@keywords gnsdt
#'@examples
#'##To read the .csv file
#'file1 <- system.file("exData", "Imager.csv", package = "ReadqPCR")
#'data <- readGNS(file = file1)
#'## to visualise all the data with all the slots
#'data
#'## to visualise the initial data
#'slot(data,"iData")
#'## to read the file
#'crep1 <- CRepsd(data)
#'## to visualize the overall results
#'crep1
#'## to visualize the normalized and standardized intensities
#'slot(crep1,"nsData")
#'## to visualize the combined replicates
#'slot(crep1,"cData")
#'## to visualize the standard deviation of combined replicates
#'slot(crep1,"sdnsData")
#'## to visualize the standard deviation of raw intensity data
#'slot(crep1,"sdData")
#'@export
setMethod("CRepsd", signature = "gnsdt", definition =

            function (gnsdt){

              ires <- slot(gnsdt,"iData")
              iconc <- ires[,"Conc"]
              ires1 <- ires[-c(1,ncol(ires))]
              ires1 <- as.matrix(ires1)

              cl <- ires[,"Control"]
              tl <- ires[,"Test"]
              normI <- cl/tl
              sI <- tl/cl
              nsdata <- as.data.frame(cbind(normI,sI))
              colnames(nsdata) <- c("NI","SI")

              nsdata1 <- as.matrix(nsdata)

              row.names(nsdata1) <- ires[,"Replicate"]

              row.names(ires1) <- ires[,"Replicate"]

              irep.row <- ires[,"Replicate"]

              irep.fac <- factor(irep.row, levels = unique(irep.row))

              irep.unique <- unique(irep.row)

              iresM <- matrix(NA,nrow=length(irep.unique),ncol(nsdata1))

              dimnames(iresM) <- list(irep.unique, colnames(nsdata1))

              for (i in 1:ncol(nsdata1))

              {

                ires.col <- (nsdata1[,i])

                ires.col.split <- lapply(split(ires.col,irep.fac), mean, na.rm = TRUE)

                iresM[,i] <- unlist(ires.col.split)

              }

              iresM_nssd <- matrix(NA,nrow=length(irep.unique),ncol(nsdata1))

              dimnames(iresM_nssd) <- list(irep.unique, colnames(nsdata1))

              for (i in 1:ncol(nsdata1))

              {

                ires.col <- (nsdata1[,i])

                ires.col.split <- lapply(split(ires.col,irep.fac), sd, na.rm = TRUE)

                iresM_nssd[,i] <- unlist(ires.col.split)

              }

              iresM1 <- matrix(NA,nrow=length(irep.unique),ncol(ires1))

              dimnames(iresM1) <- list(irep.unique, colnames(ires1))

              for (i in 1:ncol(ires1))

              {

                ires.col <- (ires1[,i])

                ires.col.split <- lapply(split(ires.col,irep.fac), sd, na.rm = TRUE)

                iresM1[,i] <- unlist(ires.col.split)

              }

              newdata <- as.data.frame(iresM)
              newdata_nssd <- as.data.frame(iresM_nssd)
              newdata1 <- as.data.frame(iresM1)
              concentration <-as.data.frame(iconc)
              colnames(concentration) <- "Conc"

              resdata <- new("gnsdt",iData = ires, nsData = nsdata, cData = newdata,
                             sdnsData = newdata_nssd, sdData = newdata1, conc = concentration)

              resdata

            })
