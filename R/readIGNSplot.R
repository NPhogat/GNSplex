#'@name readIGNSplot
#'@aliases readIGNSplot
#'@title To read the image file
#'@description Function \code{readIGNSplot} reads the raw image file and generates the
#'plot of signal (intensity vs index)
#'@param Imagefile well cutted .jpeg image of lateral flow strip
#'@return plot of signal intensity vs index
#'@details Allows the user to read in the .jpeg image of lateral flow strip and
#'generates the signal in the form of the plot (intensity vs index)
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@keywords gnsdt
#'@examples
#'##To read and plot the signal of lateral flow assay strip
#'file <- system.file("exData", "Serum30nM_2.JPG", package = "GNSplex")
#'readIGNSplot(file)
#'@export
readIGNSplot <- function(Imagefile){
  image1 <- readImage(files = Imagefile)
  data1 <- image1@.Data
  cd1 <- colMeans(data1)
  Intensity1 <- 1/rowMeans(cd1)
  plot(Intensity1,xlab = "Index", ylab = "Intensity [arbitrary unit]",
       main = "Intensity vs Index")

}
