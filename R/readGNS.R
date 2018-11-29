#'@name readGNS
#'@aliases readGNS
#'@title To read the raw intensity data files
#'@description Function \code{readGNS} reads the raw intensity files (.csv or .txt) and populate
#'them in an object of Class \code{gnsdt}
#'@param file raw data file of intensity to be read in
#'@param type choose between .csv or .txt, where .csv is default
#'@return object of Class \code{"gnsdt"} and save the initial data in slot iData
#'@details Allows the user to read in the raw intensity data and save them in S4 class
#'\code{gnsdt}
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
#'## to read the .txt file
#'file2 <- system.file("exData", "Iphone_R.txt", package = "ReadqPCR")
#'data2 <- readGNS(file = file2, type = ".txt")
#'## to visualize all the data with all the slots
#'data2
#'## to visualise the initial data
#'data2(data2,"iData")
#'@export
readGNS <- function(file, type = ".csv"){

  if (type == ".csv"){

    idata <- read.csv(file, header = TRUE)
    #ipdata <- read.csv(ipfile, header = TRUE)

  }

  else if(type == ".txt"){

    idata <- read.table(file, header = TRUE, fill = TRUE,skip = 0, sep = "\t", quote = "\"",comment.char = "")
    #ipdata <- read.table(ipfile, header = TRUE, fill = TRUE,skip = 0, sep = "\t", quote = "\"",comment.char = "")

  }

  else{

    warning('Select the type of file out of ".txt" and ".csv" !' )

  }

  res <- new("gnsdt", iData = idata)

  res

}
