#'@title "Utility_Benchmark" :  For internal use only
#'
#'@name			Benchmark
#'
#'@description	This function is a utility intended to compute values in order to compare methods. 
#'				For internal use only.
#'
#' @param 	Calc 		Computed matrix (0,1), to compare with reference
#' @param 	Ref			Reference matrix (0,1)
#'
#' @details				Calc and Ref must have the same size and contain 0 or 1 only.
#'
#'@return	Vector		Returns a vector : TP, TN, FP, FN, Sensitivity, Specificity, Dist to the diagonal
#'
#'@export
#'

Benchmark <- function(Calc, Ref) {
	if (is.vector(Calc, mode="numeric"))
		Calc	<- matrix(Calc, nrow=1)		# transformed into a matrix
	if (is.vector(Ref, mode="numeric"))
		Ref	<- matrix(Ref, nrow=1)			# transformed into a matrix
		
	if ((dim(Calc)[1] != dim(Ref)[1]) || (dim(Calc)[2] != dim(Ref)[2])) {
		print("ERROR : Calc and Ref have different size !")
		return(FALSE)
	}
	
	diag(Calc)	<- 0														# Diagonal set to 0
	diag(Ref)	<- 0
	
	if (!all(Calc %in% c(0, 1)) || !all(Ref %in% c(0, 1))) {
		print("ERROR : Calc and Ref must contain 0 or 1 only !")
		return(FALSE)
	}
						
	P 	<- sum(Ref)															# True nbr of 1
	N 	<- dim(Calc)[1] * (dim(Calc)[2]-1) - P								# True nbr of 0
	
	TP	<- round(sum(ifelse(Calc & Ref, 1,0)), digits=0)					# Nbr of true 1 discovered
	TN	<- round(sum(ifelse(!Calc & !Ref, 1,0)) - dim(Calc)[1], digits=0)	# Nbr of true 0 discovered
	FP	<- round(N - TN, digits=0)											# Nbr of false 1 discovered
	FN	<- round(P - TP	, digits=0)											# Nbr of false 0 discovered
	Se 	<- round(TP/P, digits=3)											# Sensibility or sensitivity
	Sp	<- round(TN/N, digits=3)											# Specificity
	Dst	<- Se+Sp-1															# Distance to the diagonal

	#	cat ("P",P," N",N," TP",TP," TN",TN," FP",FP," FN",FN," Se",Se," Sp",Sp," Dist ",Dst, "\n")	
	return (c(TP, TN, FP, FN, Se, Sp, Dst))
}		# Benchmark
