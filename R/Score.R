#'@title "Score" :  Delivers the "Confusion Matrix" when a reference is available.
#'
#'@description : this function computes the "Confusion Matrix" (CM) by comparing the "Classified Connectivity Matrix" delivered by MRARegress::Classify with the "Reference".\cr
#'				 If Nc is the number of classes, the "Confusion Matrix" has Nc rows and Nc columns. The rows correspond to the "Reference" values and the columns, to the classified values (rDig)\cr
#'				 CM\[i, j\] = number of classified coefficients equal to "j-1", while the corresponding reference coefficients are equal to "i-1" (i, j >= 1).\cr
#'				 For instance, if "Classes = c(0, 1)", then:\cr
#'				 -  CM\[1, 1\] = TN  ("True Negative"),\cr
#'				 -  CM\[1, 2\] = FP  ("False Positive"),\cr
#'				 -  CM\[2, 1\] = FN  ("False Negative"),\cr
#'				 -  CM\[2, 2\] = TP  ("True Positive").\cr
#				This function supplies also classical coefficients, such as NRi (number of "i" in the reference), NDi (number of "i" discovered),
#'				precision (TP/ND1), sensitivity (or sensibility or recall: TP/NR1), specificity (TN/NR0), F1 score (2*precision*recall/(precision+recall)) combining precision and recall.
#'
#'@param rDig		Matrix of numbers		The "Classified Connectivity Matrix" (delivered by "Classify")
#'@param Ref		Matrix of numbers		The "Reference Matrix"
#'@param Classes	Vector					A vector showing the classes (for instance: c(0,1))
#'
#'@return			List	A list containing the "Confusion Matrix" and classical coefficients, such as NRi, NDi, precision, sensitivity, specificity, F1 score.
#'@export
#'

Score <- function (rDig, Ref, Classes) {
  cat ("this method is not implemented yet !", "\n")
}		# Score
