#'@title S4 Class gnsdt to contain the data
#'@slot iData contains the initial data, except concentration, in the form of an object
#'of Class \code{data.frame}
#'@slot nsData contains the normalized and standardized intensity data in the form of an obkect
#'of Class \code{data.frame}
#'@slot cData contains the result of the function \code{CombineReps} in the form of an object
#'of Class \code{data.frame}
#'@slot cData contains the combined replicates in the form of an object of Class \code{data.frame}
#'@slot sdnsData contains the standard deviation of normalized and standardized intensities
#'within repliactes in the for of an object of Class \code{data.frame}
#'@slot sdData contains the stadard deviation of raw intensities within replicates in the form of an
#'object of Class \code{data.frame}
#'@slot confData contains the confidence intervals in the form of an object of Class \code{data.frame}
#'@slot lodData contains the result of LOB, LOD and LOQ in the form of an object of Class
#'\code{data.frame}
#'@slot corData contains the result of correlation of normalized and standardized intensities
#'with respect to their predicted values by linear model in the form of an object of Class
#'\code{data.frame}
#'@slot conc contains the data of concentration in the form of an object of class \code{data.frame}
#'@exportClass
setClass("gnsdt", contains = "data.frame", representation(iData = "data.frame",

                                                          nsData = "data.frame",

                                                          cData = "data.frame",

                                                          sdnsData = "data.frame",

                                                          sdData = "data.frame",

                                                          confData = "data.frame",

                                                          lodData = "data.frame",

                                                          corData = "data.frame",

                                                          conc = "data.frame"))
