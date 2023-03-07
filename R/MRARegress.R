#'@title "MRAregress" :  Tools to infer connectivity in biological networks -- Main program
#'
#'@description : this function checks the input data and computes the connectivity matrix
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
#'@param Method			String				The method to use to compute the connectivity matrix. Method to choose among the available methods:
#'											- TLR: a simple Multiple Linear Regression method which minimizes the Least Square Error (default value),
#'											- LASSO: a shrinkage estimator,
#'											- RIDGE: a shrinkage estimator,
#'											- Elastic Net: a shrinkage estimator,
#'											- STEP: a variable selection scheme,
#'											- ARACNE: a method based on Mutual Information,
#'											- CLR: another method based on Mutual Information,
#'											- RMNET: another method based on Mutual Information,
#'											- Random Forest: a method used in Machine Learning.
#'
#'@return				Matrix of numbers	Returns the "Connectivity Matrix" ("r") if no error occured. This matrix has Nb rows and Nb columns.
#'@export
#'


MRARegress <- function (nbBase, MapExper, MatExp, NodeNames = NULL, KnlgMap = NULL, Method = "TLR") {
  cat ("this method is not implemented yet !", "\n")
}		# MRARegress
