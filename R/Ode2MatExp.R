#'@title "Utility_MExp" :  For internal use only
#'
#'@name			Ode2MatExp
#'
#'@description	This function is an utility intended to compute the matrices MatExp and Perturb,
#'				necessary to use MRARegress. For internal use only.
#'				Computation of MatExp is done directly from the equation of system dynamics.
#'
#'@param nbN		Number			Number of nodes.
#'@param Perturbs	Vector			Set of perturbations. The parameters to perturb are multiplied successively by Perturbs\[i\]
#'									There are no replicates.
#'@param P0			Vector			Initial value of the parameters to perturb (for instance : initial value of VS1,VS2,VS3 and VS4).
#'@param func		Function		Function of P (parameters) and X (expression values to compute)
#'@param st			Vector			Multiroot starts from these values, to solve the system of ODE.
#'
#'
#'@return		List of matrices	Returns a list with Exp=MatExp, Pert=Perturb.
#'
#'@export
#'


MExp	<- function (nbN, Perturbs, P0, func, st) {
	nbP			<- length(Perturbs)
	MatExp		<- matrix (0, nrow=nbN, ncol=nbN*nbP+1)	
	Nom			<- vector(length=nbN*nbP+1)

	F <- function(X) {return (func(P,X))}
	
	P		<- P0
	MatExp[ ,1]	<- (multiroot (f=F, start= st))$root

	for (iPert in 1:nbP) {
		Perturb	<- Perturbs[iPert]
		
		for (iNode in 1:nbN) {
			P			<- P0
			P[iNode]	<- P[iNode]*Perturb
			
			MatExp[ ,iNode+1+(iPert-1)*nbN]	<- (multiroot (f=F, start= st))$root
		}
	}
	
	Nom[1]	<- "Base"
	ind		<- 2
	
	for (iPert in 1:nbP) {
		for (iNode in 1:nbN) {
			val	<- paste("Q", iNode, letters[iPert], "->N", iNode, sep="")		# valid up to 26 perturbs. otherwise, use "sequence_letters" in MRARegress
			Nom[ind]	<- val
			ind	<- ind+1
		}
	}
	
	return (list(Exp=MatExp, Pert=Nom))
}		# MExp
