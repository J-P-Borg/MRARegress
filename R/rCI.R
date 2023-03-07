#'@title "rCI":  Computes the Confidence Intervals of the Connectivity Matrix
#'
#'@description: this function computes the Confidence Intervals for each coefficient of the Connextivity Matrix, except the diagonal
#'
#'@param nbBase			Number				Number of replicas concerning the basal state (ie. non perturbed system). nbBase >= 1.
#'@param MapExper		Matrix of {0, 1, x}	The "Experimental Map". The Nb rows represent the nodes (genes, proteins etc...) and the Np columns represent the perturbations.
#'											If a perturbation is repeated several times ("replicas"), the corresponding columns must be added and Np increased accordingly.
#'											MapExper\[i,j\] = 1 means that the perturbation j perturbs the node i,
#'											MapExper\[i,j\] = 0 means that the perturbation j does not perturb the node i,
#'											MapExper\[i,j\] = x means that we do not know if the perturbation j perturbs the node i or not.
#'@param MatExp			Matrix of numbers	The "Expression Matrix", Nb rows, Np+nbBase columns.
#'											MatExp\[i,1..nbBase\] represents the measurement of node i when no perturbation is applied to the network,
#'											MatExp\[i,j\], j>nbBase,  represents the measurement of node i when perturbation j-nbBase is applied to the network.
#'@param NodeNames		Vector of strings	The name of the nodes (use HUGO gene symbols, even for proteins). If NULL, the nodes will be named "1", "2", "3" etc...
#'@param KnlgMap		Matrix of {0, 1, x}	The "Knowledge Map". The Nb rows and Nb columns represent the nodes.
#'											KnlgMap\[i,j\] = 1 means that we know that there is an edge between node j and node i
#'											KnlgMap\[i,j\] = 0 means that we know that there is no edge between node j and node i
#'											KnlgMap\[i,j\] = x means that we do not know if there is an edge between node j and node i.
#'											KnlgMap\[i,i\] is not defined and may be set to 0.
#'											Choose NULL if you don't have a priori information about the network connectivity.
#'@param  percent		Number				The percentage of confidence required. Default = 95%.
#'
#'@return				Dataframe
#'		Returns a dataframe of Nb*(Nb-1) rows and 5 columns:
#'		- name of the origin node of the edge
#'		- name of the destination node of the edge
#'		- min. value of the Confidence Interval (CI)
#'		- max. value of the CI
#'		- mean value of the CI.
#'@export
#'


rCI <- function (nbBase, MapExper, MatExp, NodeNames = NULL, KnlgMap = NULL, percent = 0.95) {
  cat ("this method is not implemented yet !", "\n")
}		# rCI
