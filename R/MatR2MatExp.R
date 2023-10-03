#'@title "Utility_MatR2MatExp" :  For internal use only
#'
#'@name			MatR2MatExp
#'
#'@description	This function is a utility intended to convert the MatR matrices, got from FRANK for example, into the matrices MatExp and Perturb,
#'				necessary to use MRARegress. For internal use only.
#'
#'@param MatR	Matrix of numbers Matrix generated according to the description in the article "Modular response analysis reformulated as a multilinear
#'								  regression problem" (Borg et al, 2023).
#'@param nbN	Number			  Number of nodes.
#'@param nbK	Number			  Number of perturbations per node (concerning the MatR matrices got from FRANK, nbK = 2 :
#'								  The first nbN rows correspond to KO perturbations and the rows nbN+1 .. 2*nbN, to KD50 pertubations).
#'
#'
#'@return		List of matrices	Returns a list with Exp=MatExp, Pert=Perturb.
#'								  To generate MatExp, we suppose Xi = 1 (all i) and Relative = TRUE (Ri,j = 2*(Xi(P+∆Pj)-Xi(P)) / (Xi(P+∆Pj)+Xi(P)))
#'
#'@export
#'

MatR2MatExp	<-	function (MatR, nbN, nbK) {
	MatExp		<- matrix(0, nrow=nbN, ncol=nbK*nbN+1)
	Perturb		<- vector(length=nbK*nbN+1)
	
	for (iRow in 1:nbN) {
		MatExp[iRow,1]	<- 1
		for (iK in 1:nbK) {
			for (iCol in (2+(iK-1)*nbN):(iK*nbN+1)) {
				MatExp[iRow, iCol]	<- (1+MatR[iRow+(iK-1)*nbN,iCol-(iK-1)*nbN-1]/nbK) / (1-MatR[iRow+(iK-1)*nbN,iCol-(iK-1)*nbN-1]/nbK)
			}	# iCol	
		}	# iK
	}		# iRow
	
	Perturb[1]	<- "Base"
	for (iRow in 1:nbN) {
		for (iK in 1:nbK) {
			Perturb[iRow+(iK-1)*nbN+1]		<- paste("P", iRow+(iK-1)*nbN, " -> N", iRow, sep="")		
		}	# iK
	}		# iRow
	
	return (list(Exp=MatExp, Pert=Perturb))
}		# MatR2MatExp
