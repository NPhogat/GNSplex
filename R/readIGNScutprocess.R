#'@name readIGNScutprocess
#'@aliases readIGNScutprocess
#'@title To read the .jpeg image of lateral flow assay and apply cuts to cut the
#'control line and test line peaks of the signal intensity vs index
#'@description Function \code{readIGNScutprocess} will provide the values, which can be
#'used to extract the intensities of control line and test line peaks
#'@param imagefile the .jpeg file of lateral flow assay
#'@param fp either TRUE or FALSE
#'@param cut1 the first value on x-axis (index) to cut the peak
#'@param cut2 the second value on x-axis (index) to cut the peak
#'@param pi the intensity (y-axis) to get the processed signal. It's a cutoff intensity
#'to do the baseline correction
#'@return The plots of control line and test line peaks
#'@details Allows the user to read in the .jpeg image of lateral flow assay and returns
#'the cutted and baseline corrected plots of signals of control line and test line.
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@keywords gnsdt
#'@examples
#'## to read and see complete plot of signal
#'file3 <- system.file("exData", "Serum30nM_2.JPG", package = "GNSplex")
#'readIGNSplot(file3)
#'## to read and cut the first peak (control line signal) and to do baseline
#'##correction
#'file3 <- system.file("exData", "Serum30nM_2.JPG", package = "GNSplex")
#'readIGNScutprocess(imagefile = file3, fp = TRUE, cut1 = 3, cut2 = 13, pi = 2.0)
#'## To cut the second peak (test line signal) and to do baseline correction
#'readIGNScutprocess(imagefile = file3, fp = FALSE, cut1 = 17, cut2 = 30, pi = 2.0)
#'@export
readIGNScutprocess <- function(imagefile, fp = TRUE, cut1 = 2, cut2 = 18, pi = 1.56){
  image1 <- readImage(files = imagefile)
  data1 <- image1@.Data
  cd1 <- colMeans(data1)
  Intensity1 <- 1/rowMeans(cd1)
  if (fp == TRUE){
    Intensity2 <- Intensity1[cut1:cut2]
    plot(Intensity2[Intensity2>pi],xlab = "Index", ylab = "Intensity [arbitrary unit]",
         main = "Intensity vs Index")

  }

  else if (fp == FALSE){
    Intensity3 <- Intensity1[cut1:cut2]
    plot(Intensity3[Intensity3>pi],xlab = "Index", ylab = "Intensity [arbitrary unit]",
         main = "Intensity vs Index")

  }
  else {
    warning("Provide the right value of cut of the image")
  }
}
