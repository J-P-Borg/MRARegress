#'@title "Classify" :  Discretizes and classifies the connectivity coefficients
#'
#'@description : this function discretizes the connectivity coefficients, then classifies them according to a set of classes \cr
#'	For instance \{-1, 0, 1\} or \{0, 1\}. The classification method may depend on the method used to compute the connectivity coefficients.
#'
#'@param r			Matrix of numbers		The connectivity matrix (delivered by "MRARegress")
#'@param Classes	Vector					A vector showing the classes (for instance: c(0,1), default value)
#'@param Method		String					The method to use to compute the connectivity matrix. Method to choose among the available methods: \cr
#'											- TLR: a simple Multiple Linear Regression method which minimizes the Least Square Error (default value), \cr
#'											- LASSO: a shrinkage estimator, \cr
#'											- RIDGE: a shrinkage estimator, \cr
#'											- Elastic Net: a shrinkage estimator, \cr
#'											- STEP: a variable selection scheme, \cr
#'											- ARACNE: a method based on Mutual Information, \cr
#'											- CLR: another method based on Mutual Information, \cr
#'											- RMNET: another method based on Mutual Information, \cr
#'											- Random Forest: a method used in Machine Learning.
#'@param Lbda		Number					An hyper-parameter specific to the method.\cr
#'											If "NULL" (default value), Lbda will get the default value specific to the method, if this value is defined.\cr
#'											For instance, if Method = "TLR" and Lbda = "NULL", the value 0.25 will be given to Lbda.
#'
#'@return			Matrix of numbers		Returns the "Classified Connectivity Matrix" ("rDig") if no error occured. This matrix has Nb rows and Nb columns.
#'											Returns NULL in case of error or warning.
#'@export
#'


Classify <- function (r, Classes, Method = "TLR", Lbda = NULL) {
  cat ("this method is not implemented yet !", "\n")
  tryCatch (
		expr = {
  
  
  
  
		},		# expr
		
		error = function (e) {
			message ("Error detected !")
			print(e)
		},		# error
		warning = function (e) {
			message ("Warning detected !")
			print(e)
		},		# warning
		finally = {
		
		}		# finally
  )		# tryCatch  
}		# Classify
