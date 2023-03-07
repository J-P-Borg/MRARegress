#'@title "DrawScore" :   displays "rDig" and "Score" as a graph
#'
#'@description : this function represents the "Classified Connectivity Matrix" (rDig) as a graph, if "Classes = c(0,1)" only.\cr
#'	The nodes are the genes (for instance) and the edges represent the interaction between genes:\cr
#'	Black solid lines correspond to "True Positive" edges (ie, an edge between A and B is discoverd by the method, and this edge exists in the "reference"),\cr
#'	Red solid lines correspond to "False Positive" edges (ie, an edge between A and B is discoverd by the method, and this edge doesn't exist in the "reference"),\cr
#'	Green solid lines correspond to "False Negative" edges (ie, an edge between A and B is not discoverd by the method, but this edge exists in the "reference").
#'
#'@param rDig		Matrix of numbers		The "Classified Connectivity Matrix" (delivered by "Classify" -- Classes = c(0,1) only)
#'@param Ref		Matrix of numbers		The "Reference Matrix"
#'
#'@return		No return
#'		Displays the graph associated, as described in the "Description"
#'@export
#'


DrawScore <- function (rDig, Ref) {
  cat ("this method is not implemented yet !", "\n")
}		# DrawScore
