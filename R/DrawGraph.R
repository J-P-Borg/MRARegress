#'@title "DrawGraph" :   displays "r" as a graph
#'
#'@description : this function represents the Connectivity Matrix "r" as a graph where the nodes are the genes (for instance) \
#'	and an edge A $\\rightarrow$ B reflects the action of node A on node B (amplification), while a dashed line (with a "T" arrow  betwwen A and B) reflects an inhibition of node B by node A.
#'
#'@param r		Matrix of numbers		The connectivity matrix (delivered by "MRARegress")
#'
#'@return		No return
#'		Displays the graph associated with the connectivity matrix.
#'@export
#'


DrawGraph <- function (r) {
  cat ("this method is not implemented yet !", "\n")
}		# DrawGraph
