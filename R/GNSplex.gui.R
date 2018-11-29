#'@name GNSplex.gui
#'@aliases GNSplex.gui
#'@title Run the graphical user interface for analysis of the data of lateral
#'flow assay
#'@description Run the graphical user interface
#'@author Navneet Phogat, Matthias Kohl, \email{Matthias.Kohl@@stamats.de}
#'@examples
#'GNsplex.gui()
#'@export
GNSplex.gui <- function() {

  runApp(system.file("GNSplex.gui", package = "GNSplex"))

}
