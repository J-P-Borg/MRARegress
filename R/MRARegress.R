#'@title "MRARegress" :  Tools to infer connectivity in biological networks -- Main program
#'
#'@description : this function checks the input data and computes the connectivity matrix, according to the document "MaRédaction.docx".
#'
#'
#'@param MatExp		Matrix of numbers	The "Expression Matrix", nbN rows (number of nodes), nbPc + nbBase columns. No default value.
#'										(nbPc = number of conducted perturbations, nbBase = number of "non perturbed" sets of measurements, called "basal" columns).
#'										MatExp\[i,j\] represents the measurement of node i when no perturbation is applied to the network, if column "j" is a "basal" column
#'										MatExp\[i,j\] represents the measurement of node i when perturbation designated by "Perturb\[j\] is applied to the network.
#'										As the values taken into account to compute the connectivity matrix are "DeltaXij", ie the difference between 
#'										the measurement of node i when perturbation "Perturb\[j\] is applied to the network and the measurement of node i without any perturbation,
#'										we will use nbP = nbBase*nbPc as the "number of perturbations" in the package.
#'										The number of perturbations must be large enough to allow the computation of the connectivity matrix (see "Details"). MatExp\[i,j\] must be >= 0.
#'@param Perturb	Vector of strings	Refers to the name of the perturbations and the nodes they act upon. nbPc + nbBase values. Default value is NULL.
#'										If Perturb is NOT NULL, a mandatory syntax must be used. Each item is a string of one of the following values : 
#'										- "Base"	: the corresponding columns of MatExp is a "basal" column. It contains the measurements of the nodes when no perturbation 
#'													  is applied to the network. Many "basal" columns may exist, see below.
#'										- "perturbation name -> node name" : "perturbation name" is the name of the perturbation and "node name", the name of the node the perturbation acts upon.
#'										  The name of the node must be one item of the vector "NodeName" (strict equality, case sensitive). 
#'										  Many identical items may exist (designating technical replicas). Various perturbations may act upon the same node.
#'										  If "MapExper" is NULL, a perturbation must act upon one node only.
#'										  The columns of "t(Perturb)" (ie "Perturb transposed") must exactly match those of MatExp.
#'										If Perturb is NULL (default value), the number of perturbations must equal the number of nodes.There is only one basal column (the first one), 
#'										the perturbations are called "P1", "P2" ... and "P1" is supposed to act on the first node, "P2" on the second node etc...
#'@param NodeName	Vector of strings	The name of the nodes (use HUGO gene symbols, even for proteins). nbN values.  Default value is NULL.
#'										If NOT NULL, the rows of "NodeName" must exactly match those of MatExp.
#'										If NULL, the nodes will be named "N1", "N2", "N3" etc...
#'@param KnlgMap	Matrix of {0, 1, -1, x}	The "Knowledge Map". The nbN rows and nbN columns represent the nodes.  Default value is NULL.
#'										KnlgMap\[i,j\] =  1 means that we know that there is an oriented edge between node j and node i (node j ==> node i),
#'										KnlgMap\[i,j\] = -1 means that we know that there is an oriented edge between node i and node j (node i ==> node j),
#'										KnlgMap\[i,j\] =  0 means that we know that there is no edge between node j and node i,
#'										KnlgMap\[i,j\] =  x means that we do not know if there is an edge between node j and node i.
#'										KnlgMap\[i,i\] is not defined and may be set to 0.
#'										Choose NULL if you don't have a priori information about the network connectivity.
#'										If NOT NULL, the rows and the columns of "KnlgMap" must exactly match the rows of MatExp.
#'@param Method		String				The method to use to compute the connectivity matrix. Default value is "TLR".
#'										Method to choose among the available methods:
#'										- TLR: a simple Multiple Linear Regression method which minimizes the Least Square Error (default value),
#'										- LASSO: a shrinkage estimator,
#'										- RIDGE: a shrinkage estimator,
#'										- Elastic Net: a shrinkage estimator,
#'										- STEP: a variable selection scheme,
#'										- ARACNE: a method based on Mutual Information,
#'										- CLR: another method based on Mutual Information,
#'										- RMNET: another method based on Mutual Information,
#'										- Random Forest: a method used in Machine Learning.
#'@param MapExper	Matrix of numbers	The "Experience Map" matrix : nbM rows (number of parameters), nbPc columns (number of conducted perturbations). Default value is NULL.
#'										In the document "MaRédaction.docx", a strong asumption is made, called "H6" : 
#'										-  nbM = nbN  (the number of parameters equals the number of nodes),
#'										-  Every perturbation acts exactly upon one different node (ie.  ∂φ_i/∂p_j ≠ 0 if and only if i = j),
#'										-  A set of such perturbations acting upon every node is available.
#'										If "H6" is true, MapExper must be NULL. If not, MapExper must be filled, and the perturbations "proportional" to a reference pertubation,
#'										ie the coordinates of the perturbations be 0 or proportional to those of the reference.
#'										If NOT NULL, this matrix contains the coordinates of the perturbations on the base of the parameters regarding to those of a perturbation
#'										chosen as a reference. For instance, if "P0" is the reference, with coordinates (x0_1, x0_2, ... x0_M), the coordinates of perturbation "P1"
#'										must be 0 or proportional to those of P0 (ie : x1_j = 0 or x1_j = k1*x0_j). Idem for "P2".
#'										The column of MapExper corresponding to	"P0" must contain (1, 1, .. 1), the column corresponding to "P1" must contain (k1, 0, k1, .., k1) 
#'										and the column corresponding to "P2" (k2, k2, .., k2). We have assumed that x1_2 = 0 and that the other coordinates are not null.
#'@param Verbose	Logical				Default	value : FALSE.
#'										If TRUE, additional printings are made. These printings are for internal use only, so they are not documented.
#'
#'@details								nbN : number of nodes,
#'										nbP : number of perturbations, ie nbBase*nbPc (see "MaRedaction.docx" for the conditions relative to the perturbations),
#'										nbM : number of parameters.
#'										If MapExper is NULL, it is mandatory that nbP >= nbN
#'										If MapExper is NOT NULL and perturbations are proportional, it is mandatory that nbP >= nbN - 1 + nbM.
#'
#'										Imported libraries :
#'										- stringr		processing of strings
#'										- stats			lm, as.formula
#'

#'@import stringr
#'@import stats
#'


#'
#'@name			MRARegress
#'
#'@description	The function MRARegress computes the connectivity matrix, according to the document "MaRédaction.docx".
#'				The input data are described above.
#'
#'@return		Matrix of numbers	Returns the "Connectivity Matrix" ("r") if no error occured. This matrix has nbN rows and nbN columns.
#'									Returns NULL in case of error or warning.
#'
#'@export
#'

MRARegress <- function (MatExp, Perturb = NULL, NodeName = NULL, KnlgMap = NULL, Method = "TLR", MapExper = NULL, Verbose = FALSE) {
  cat ("this method is under development yet !", "\n")
  
  tryCatch (
	expr = {
		# Check input data
		err	<-  CheckInputData (MatExp, Perturb, NodeName, KnlgMap, Method, MapExper)
		if (! is.null(err)) {
			message (err)
			return (NULL)
		}

		# Variables declaration
		nbN			<- dim(MatExp)[1]										# Number of nodes
		nbBase		<- ifelse(is.null(Perturb), 1, sum(Perturb=="Base"))	# Number of basal columns
		nbPc		<- dim(MatExp)[2] - nbBase								# Number of conducted perturbations
		nbP			<- nbBase*nbPc											# Number of "perturbations" taken into account
		PerturbN	<- matrix(nrow=nbPc, ncol=2)							# Name of the perturbations and index of the node a perturbation acts upon
		
		if (is.null(MapExper)) {
			nbM		<- nbN													# Number of parameters
			H6		<- TRUE													# "H6" asumption (see "MaRedaction.docx")
			MapExper <- matrix(0, nrow=nbM, ncol=nbPc)						# To describe the links between conducted perturbations and parameters
			nbInc	<- 1													# Number of additional unknowns (see "MaRedaction.docx")
		} else {
			nbM		<- dim(MapExper)[1]										# Number of parameters
			H6		<- FALSE												# "H6" asumption (see "MaRedaction.docx")
			nbInc	<- nbM													# Number of additional unknowns (see "MaRedaction.docx")			
		}

		MatD		<- matrix(0, nrow=nbP, ncol=nbN+nbM)					# Matrix DeltaX : Xi,j - Xi and coordinates of the perturbations vs. parameters
		MatX		<- matrix(0, nrow=nbP, ncol=nbN+nbInc-1)				# Used to compute linear Regression
		MatY		<- vector(length=nbP)									# Y as a function of X
		MatrCc		<- array(dim=c(nbN,nbN))								# Computed Connectivity Matrix
		
		if (is.null(Perturb)) {
			cBase		<- c(1)												# Position of the basal columns in MatExp
			cPerturb	<- seq(2, nbPc+1, by=1)								# Position of the perturbations columns in MatExp
		} else {
			cBase		<- which(Perturb == "Base")							# Position of the basal columns in MatExp
			cPerturb	<- which(Perturb != "Base")							# Position of the perturbations columns in MatExp		
		}
		
		# Looking for node names, perturbation names and nodes perturbations act upon
		if (is.null(NodeName)) {
			NodeName	<- seq(1, nbN, by=1)
			NodeName	<- as.character(NodeName)
			NodeName	<- paste("N", NodeName, sep="")						# Name of the node
		}
		
		if (is.null(Perturb)) {
			PerturbN[ ,1]	<- seq(1, nbPc, by=1)
			PerturbN[ ,1]	<- as.character(PerturbN[ ,1])
			PerturbN[ ,1]	<- paste("P", PerturbN[ ,1], sep="")			# Name of the pertubation
			PerturbN[ ,2]	<- seq(1, nbN, by=1)							# Index of the node the perturbation acts upon (in this case, nbN = nbPc)
		} else {
			for (iPc in 1:nbPc) {
				unPert	<- Perturb[cPerturb[iPc]]
				unPert	<- str_replace_all(unPert, " ", "")
				unPert	<- strsplit(unPert, "->")							# Mandatory syntax :  "Pi  -> Nj" (as many spaces as you like)
				PerturbN[iPc,1]	<- unPert[[1]][1]							# Name of the pertubation
				PerturbN[iPc,2]	<- which(NodeName == unPert[[1]][2])		# Index of the node the perturbation acts upon
				if (length(PerturbN[iPc,2]) != 1) {
					message ("Unknown node or a perturbation acts upon many nodes !")
					return (NULL)
				}
			}
		}

		if (length(unique(PerturbN[ ,2])) != nbN) {
			message ("Some nodes are not perturbed !")
			return (NULL)
		}
		
		if (H6) {
			for (iPc in 1:nbPc) {
				MapExper[as.numeric(PerturbN[iPc,2]), iPc]	<- 1
			}
		}
				
		if (Verbose) {
			cat("nbN ", nbN, " nbPc ", nbPc, " nbBase ", nbBase, " nbInc ", nbInc, " Nodes ", NodeName, " Perturb. & nodes ", PerturbN, " MapExper ", MapExper, "\n")
		}
		
		# Construction of the "Matrix Delta" (MatD)
		for (iBase in 1:nbBase) {
			for (iPc in 1:nbPc) {
				for (iNode in 1:nbN) {
					MatD[(iBase-1)*nbPc+iPc, iNode]		<- MatExp[iNode, cPerturb[iPc]] - MatExp[iNode, cBase[iNode]]
				}
				for (iPar in 1:nbM) {
					MatD[(iBase-1)*nbPc+iPc, nbN+iPar]	<- MapExper[iPar, iPc]
				}
			}
		}
		
		if (Verbose) {
			cat("MatD ", MatD, "\n")
		}

		# Construction of matrices "X" and "Y"
		for (iNode in 1:nbN) {
			col		<- 1:nbN
			col		<- col[-iNode]
			
			MatY[ ]	<- MatD[ , iNode]

			if (H6) {
				MatX[ , nbN]	<- MatD[ , nbN+iNode]
			} else {
				for (iCol in 1:nbM) {
					MatX[ , nbN-1+iCol]	<- MatD[ , nbN-1+iCol]
				}
			}

			Form	<- "MatY[ ]~MatX[ , ]+0"
			MatrCc[iNode,col]	<- (lm(as.formula(Form)))$coefficients[1:(nbN-1)]
			MatrCc[iNode,iNode]	<- -1
			
			if (Verbose) {
				cat("iNode ", iNode, " Y ", MatY, " X ", MatX, " Form ", Form, " MatrCc ", MatrCc, "\n")
			}
		}	# for iNode
		
		cat ("DONE !", as.character(Sys.time()), "\n")
		return (MatrCc) 
	},		# expr
	
	error = function (e) {
		message ("Error detected !")
		print(e)
	},		# error
	warning = function (e) {
		message ("Warning detected !")
		print(e)
	}		# warning
  )			# tryCatch
  return (NULL)
}		# MRARegress


#' Check the input data for function MRARegress
#'
#' This function checks the input data for function MRARegress.
#' The parameters are same as those of MRARegress.
#'
#'@param MatExp		The "Expression Matrix".
#'@param Perturb	Refers to the name of the perturbations and the nodes they act upon.
#'@param NodeName	The name of the nodes.
#'@param KnlgMap	The "Knowledge Map".
#'@param Method		The method to use to compute the connectivity matrix.
#'@param MapExper	The "Experience Map" matrix

#'
#' @return 			A message if an error is detected and returns NULL otherwise.
#'

CheckInputData	<- function (MatExp, Perturb, NodeName, KnlgMap, Method, MapExper) {

	# Variables declaration
	nbN			<- dim(MatExp)[1]										# Number of nodes
	nbBase		<- ifelse(is.null(Perturb), 1, sum(Perturb=="Base"))	# Number of basal columns
	nbPc		<- dim(MatExp)[2] - nbBase								# Number of conducted perturbations
	nbP			<- nbBase*nbPc											# Number of "perturbations" taken into account	
	if (! is.null(MapExper))
		nbM		<- dim(MapExper)[1]										# Number of parameters
		
	# Test Input Variables
	if (! is.matrix(MatExp))
		return ("MatExp must be a matrix !")
	if (! is.numeric(MatExp))
		return ("MatExp must contain positive numbers only !")
	if (sum(MatExp < 0) > 0)
		return ("MatExp must contain positive numbers only !")
		
	if (is.null(Perturb) && (nbP != nbN))
		return ("Perturb is NULL and nb. perturbations != nb. nodes !")

	if (! is.null(Perturb)) {
		if (! is.vector(Perturb))
			return ("Perturb must be NULL or a vector !")
		if (length(Perturb) != dim(MatExp)[2])
			return ("Perturb must be NULL or have as many columns as MatExp !")
		if (nbBase <= 0)
			return ("Perturb must contain one basal column at least !")
	}

	if (! is.null(NodeName)) {
		if (! is.vector(NodeName))
			return ("NodeName must be NULL or a vector !")
		if (length(NodeName) != nbN)
			return ("NodeName must be NULL or have as many rows as MatExp !")
		if (! is.character(NodeName))
			return ("NodeName must be NULL or contain strings only !")
	}
	
	if (! is.null(KnlgMap)) {
		if (! is.matrix(KnlgMap))
			return ("KnlgMap must be NULL or a matrix !")
		if (dim(KnlgMap)[1] != dim(KnlgMap)[2])
			return ("KnlgMap must be NULL or a square matrix !")
		if (dim(KnlgMap)[1] != nbN)
			return ("KnlgMap must be NULL or have as many rows as MatExp !")
		if (sum(! (KnlgMap %in% c(0, 1, -1, "x"))) > 0 )
			return ("KnlgMap must contain '0', '1', '-1' or 'x' only !")
	}
	
	if (! is.character(Method))
		return ("Method must be a string !")
	if (! (Method %in% c("TLR", "LASSO", "RIDGE", "Elastic Net", "STEP", "ARACNE", "CLR", "RMNET", "Random Forest")))
		return ("Unknown method !")

	if (! is.null(MapExper)) {
		if (! is.matrix(MapExper))
			return ("MapExper must be NULL or a matrix !")
		if (dim(MapExper)[2] != nbPc)
			return ("MapExper must be NULL or have as many columns as the number of conducted perturbations !")
	}

	if (nbP < nbN)
		return ("Not enough perturbations to get result !")
	if (! is.null(MapExper) && (nbP < (nbN-1+nbM)))
		return ("Not enough perturbations to get result !")	

	return (NULL)			# No error detected
}		# CheckInputData
