#'@title "RocPR" :   displays performance curves when a reference is available.
#'
#'@description : This function displays the ROC curve, the PR curve, the areas under these curves (AuROC and AuPR), the p-value of these areas, when a reference is available.
#'				 For "Classes = c(0,1)" only.
#'
#'@param r			Matrix of numbers		The connectivity matrix (delivered by "MRARegress")
#'@param Ref		Matrix of numbers		The "Reference Matrix"
#'@param Method		String					The method to use to compute the connectivity matrix. Method to choose among the available methods:
#'					 						- TLR: a simple Multiple Linear Regression method which minimizes the Least Square Error (default value),
#'											- LASSO: a shrinkage estimator,
#'											- STEP: a variable selection scheme,
#'											- ARACNE: a method based on Mutual Information,
#'											- CLR: another method based on Mutual Information,
#'											- RMNET: another method based on Mutual Information,
#'											- Random Forest: a method used in Machine Learning.#'
#'@return			List		A list containing the ROC curve, the PR curve, the areas under these curves (AuROC and AuPR), the p-value of these areas.
#'@export
#'


RocPR <- function (r, Ref, Method = "TLR") {
  cat ("this method is not implemented yet !", "\n")
}		# RocPR
