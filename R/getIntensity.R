#'@name getIntensity
#'@aliases getIntensity
#'@title To get the final extracted intensity of control line and test line signals,
#'after baseline correction
#'@description Function \code{getIntensity} reads the .jpeg files of the lateral
#'flow assay and returns the extracted intensity of the control line and test
#'line signals
#'@param imagefile .jpeg image of the lateral flow assay
#'@param cut1 first value on X-axis (index) to cut the control line or test line
#'peak, as obtained from the function readIGNScutprocess
#'@param cut2 second value on x-axis (index) to cut the control line or test line
#'peak as obtained from the readIGNScutprocess
#'@param  pi the value of intensity to be applied as baseline correction for control
#'line or test line peaks (signals). These should be the same values, as applied in
#'the readIGNScutprocess
#'@param fp TRUE or FALSE. When TRUE, one can implement for first peak control line.
#'When FALSE; one can implement for second peak test line.
#'@return baseline corrected intensities of control line and test line
#'@details Allows the user to get the baseline corrected values of intensities of
#'control line or test line
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@keywords gnsdt
#'@examples
#'#'## to read and see complete plot of signal
#'file3 <- system.file("exData", "Serum30nM_2.JPG", package = "GNSplex")
#'readIGNSplot(file3)
#'## to read and cut the first peak (control line signal) and to do baseline
#'##correction
#'file3 <- system.file("exData", "Serum30nM_2.JPG", package = "GNSplex")
#'readIGNScutprocess(imagefile = file3, fp = TRUE, cut1 = 3, cut2 = 13, pi = 2.0)
#'## To cut the second peak (test line signal) and to do baseline correction
#'readIGNScutprocess(imagefile = file3, fp = FALSE, cut1 = 17, cut2 = 30, pi = 2.0)
#'## To get the baseline corrected intensity of control line (peak 1)
#'getIntensity(imagefile = file3, cut1 = 3, cut2 = 13, pi = 2.0)
#'## to get the baseline corrected intensity of test line (peak 2)
#'getIntensity(imagefile = file3, cut1 = 17, cut2 = 30, pi = 2.0, fp = FALSE)
#'@export
getIntensity <- function(imagefile, cut1 = 8, cut2 = 25, pi = 1.2, fp = TRUE){
  image1 <- readImage(files = imagefile)
  data1 <- image1@.Data
  cd1 <- colMeans(data1)
  Intensity1 <- 1/rowMeans(cd1)
  if (fp == TRUE){
    Intensity2 <- Intensity1[cut1:cut2]
    IP1 <- mean(Intensity2[Intensity2>pi])
  }

  else if (fp == FALSE){
    Intensity3 <- Intensity1[cut1:cut2]
    IP1 <- mean(Intensity3[Intensity3>pi])
  }
  else {
    warning("Provide the right value of cut of the image")
  }
  IP1
}
