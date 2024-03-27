#'@title "Utility_MExp2" :  For internal use only
#'
#'@name			Ode2MatExp2
#'
#'@description	This function is an utility intended to compute the matrices MatExp and Perturb,
#'				necessary to use MRARegress. For internal use only.
#'				Computation of MatExp is done directly from the equation of system dynamics.
#'				Only one perturbation level is allowed, but perturbations may act on many parameters (H6 FALSE).
#'				This is the difference between Ode2MatExp and Ode2MatExp2.
#'
#'@param nbN		Number			Number of nodes.
#'@param MapExper	Matrix			The matrix used by MRARegress
#'									nbM rows (number of parameters), nbPc columns (number of conducted perturbations). See description in "MRARegress.R"
#'@param Perturb	Number			Amplitude of the perturbation.
#									Value of perturbed parameter = Value of unperturbed parameter * Perturb * MapExper[iPar,iPert]
#'@param P0			Vector			Initial value of the parameters to perturb (for instance : initial value of VS1,VS2,VS3 and VS4).
#'@param func		Function		Function of P (parameters) and X (expression values to compute)
#'@param st			Vector			Multiroot starts from these values, to solve the system of ODE.
#'
#'
#'@return		List of matrices	Returns a list with Exp=MatExp, Pert=Perturb.
#'
#'@export
#'


MExp2	<- function (nbN, MapExper, Perturb, P0, func, st) {
	nbP			<- dim(MapExper)[2]							# Number of perturbations
	MatExp		<- matrix (0, nrow=nbN, ncol=nbP+1)	
	Nom			<- vector(length=nbP+1)

	F <- function(X) {return (func(P,X))}
	
	P		<- P0
	MatExp[ ,1]	<- (multiroot (f=F, start= st))$root

	for (iPert in 1:nbP) {
		P	<- P0
		for (iPar in 1:nbN) {
			P[iPar]		<- P[iPar]*(1+Perturb*MapExper[iPar,iPert])
		}
		
		MatExp[ ,iPert+1]	<- (multiroot (f=F, start= st))$root
	}
	
	Nom[1]	<- "Base"
	
	for (iPert in 1:nbP) {
		Nom[iPert+1]	<- paste("Q", iPert,  sep="")		# valid up to 26 perturbs. otherwise, use "sequence_letters" in MRARegress
	}
	
	return (list(Exp=MatExp, Pert=Nom))
}		# MExp2
