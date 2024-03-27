#'@title "MRARegress" :  Tools to infer connectivity in biological networks -- Main program
#'
#'@description : this function checks the input data and computes the connectivity matrix, according to the document "MaRédaction.docx".
#'				 It has been tested with networks from 3 to 1000 nodes and 3 to 2000 perturbations.
#'
#'
#'@param MatExp		Matrix of numbers	The "Expression Matrix", nbN rows (number of nodes), nbPc + nbBase columns. No default value.
#'										(nbPc = number of conducted perturbations (including replicas), taking into account the number of nodes, ie, if we apply a perturbation,
#'										say "KO" for example, on EACH node, nbPc equals the number of nodes. nbBase = number of "non perturbed" sets of measurements, called "basal" columns).
#'										MatExp\[i,j\] represents the measurement of node i when no perturbation is applied to the network, if column "j" is a "basal" column
#'										MatExp\[i,j\] represents the measurement of node i when perturbation designated by "Perturb\[j\] is applied to the network.
#'										As the values taken into account to compute the connectivity matrix are "DeltaXij" (if "Relative" = FALSE, see below), ie the difference between 
#'										the measurement of node i when perturbation "Perturb\[j\] is applied to the network and the measurement of node i without any perturbation,
#'										we will use nbP = nbBase*nbPc as the "number of actual perturbations" in the package. Same thing if "Relative" = TRUE.
#'										The number of perturbations must be large enough to allow the computation of the connectivity matrix (see "Details").
#'@param Perturb	Vector of strings	Refers to the name of the perturbations (and the nodes they act upon, if "H6" is TRUE : see "MapExper"). nbPc + nbBase values. Default value is NULL.
#'										If Perturb is NULL, nbN = nbPc is mandatory.
#'										If Perturb is NOT NULL, a mandatory syntax must be used. Each item is a string of one of the following values : 
#'										- "Base"	: the corresponding columns of MatExp is a "basal" column. It contains the measurements of the nodes when no perturbation 
#'													  is applied to the network. Many "basal" columns may exist, see below.
#'										- if "H6" is TRUE : "perturbation name -> node name" : "perturbation name" is the name of the perturbation and "node name", the name of the node 
#'										  the perturbation acts upon (if not, an error message "Syntax Error" is displayed).
#'										  Beware, the separation mark, between "perturbation name" and "node name", is composed of two characters ("minus" : '-' and "superior" : '>'),
#'										  with no space between them. It is not the "arrow" character '→'.
#'										  The name of the node must be one item of the vector "NodeName" (strict equality, case sensitive). 
#'										  Many identical items may exist (designating technical replicas). Various perturbations may act upon the same node.
#'										  If "MapExper" is NULL ("H6" true), a perturbation must act upon one node only.
#'										  If "MapExper" is NULL, every node must be perturbed.
#'										- if "H6" is FALSE : "perturbation name" : "perturbation name" is the name of the perturbation.
#'										  "perturbation name -> node name" is allowed, but the relation "-> node name" is ignored, only the perturbation name is relevant.
#'										  "MapExper" must be NOT NULL, only one "basal" column is authorized. All other data of "Perturb" are used only to name the perturbations. 
#'										  The relations "perturbations/parameters" are indicated with MapExper.
#'										The columns of "t(Perturb)" (ie "Perturb transposed") must exactly match those of MatExp (number and position).
#'										If Perturb is NULL (default value), the number of perturbations must equal the number of nodes if "H6" is true.There is only one basal column (the first one), 
#'										the perturbations are called "Q1", "Q2" ... and, if "H6" is true, "Q1" is supposed to act on the first node, "Q2" on the second node etc...
#'										If several measurements correspond to the same perturbation ("replicas"), then the corresponding lines of "Perturb"	must be identical.
#'@param NodeName	Vector of strings	The name of the nodes (use HUGO gene symbols, even for proteins). nbN values.  Default value is NULL.
#'										If NOT NULL, the rows of "NodeName" must exactly match those of MatExp (number and position).
#'										If NULL, the nodes will be named "N1", "N2", "N3" etc...
#'@param KnlgMap	Matrix of {"0", "1", "-1", "x"}	The "Knowledge Map". The nbN rows and nbN columns represent the nodes.  Default value is NULL.
#'										KnlgMap\[i,j\] =  "1" or KnlgMap\[i,j\] = "-1". We know that there is an oriented edge between node j and node i (node j ==> node i),
#'										and node j amplifies the action of node i ("1") or inhibits this action ("-1").
#'										KnlgMap\[i,j\] =  "0" means that we know that there is no edge between node j and node i,
#'										KnlgMap\[i,j\] =  "x" means that we do not know if there is an edge between node j and node i.
#'										Choose NULL if you don't have a priori information about the network connectivity.
#'										If NOT NULL, the rows and the columns of "KnlgMap" must exactly match the rows of MatExp (number and position).
#'										If NOT NULL, the parameter "Method" (see below) must equal "TLR" and "H6" must be TRUE (see param. MapExper).
#'@param Method		String				The method used to compute the connectivity matrix. Default value is "TLR".
#'										Method to choose among the available methods:
#'										- TLR: a simple Multiple Linear Regression method which minimizes the Least Square Error (default value),
#'										- LASSO: a shrinkage estimator (library glmnet),
#'										- RIDGE: a shrinkage estimator (library glmnet),
#'										- Elastic Net: a shrinkage estimator (library glmnet),
#'										- STEP: a variable selection scheme,
#'										- ARACNE: a method based on Mutual Information (ref : "Package 'minet', 'Mutual Information NETworks', version 3.59.0, date 2014-07,
#'										  by Patrick E. Meyer, Frederic Lafitte, Gianluca Bontempi)
#'										- CLR: another method based on Mutual Information (ref : "Package 'minet' : see above),
#'										- MRNET: another method based on Mutual Information (ref : "Package 'minet' : see above),
#'										- Random Forest: a method used in Machine Learning (ref : "Package 'randomForest', 'Breiman and Cutler's Random Forests for Classification 
#'										 and Regression', version 4.7-1.1, date 2022-01-24, by Leo Breiman, Adele Cutler, Andy Liaw, Matthew Wiener)
#'										- Order2 : this method uses a polynomial (order 2) regression approach to compute the rij. "H6" myst be TRUE to use this method.
#'										 The number of perturbations must be sufficient to use this method, ie nbP >= nbN^2.(nbN-1).
#'@param Hyp_Lbda	Number (>= 0)		Hyper parameter needed by the LASSO method. Default value is NULL.
#'										If NULL, the LASSO method computes the best value for this parameter, according an algorithm of glmnet.
#'@param Hyp_Mu		Number (0 <= Hyp_Mu <= 1) Hyper parameter μ of the method ElasticNet -- Hyp_Mu = 0 : RIDGE, Hyp_Mu = 1 : LASSO (default value), else (between 0 and 1) : Elastic Net. 
#'@param Hyp_Step	String				Mandatory if Method = "STEP" (default = "Fo")
#'										- "Fo"		: "STEP Forward"
#'										- "Ba"		: "STEP Backward"
#'										- "Bo"		: "STEP Both"
#'@param Hyp_Eps	Number (>= 0)		Hyper parameter needed by the ARACNE method. Default value is 0.
#'										Numeric value indicating the threshold used when removing an edge : for each triplet of nodes (i,j,k), 
#'										the weakest edge, say (ij), is removed if its weight is below min{(ik),(jk)}- Hyp_Eps
#'@param Hyp_Cvx	Number (>= 0)		Hyper parameter used by the KnlgMap. Default value is 0.
#'										KnlgMap\[i,j\] =  "1" means r\[i,j\] >=  Hyp_Cvx
#'										KnlgMap\[i,j\] = "-1" means r\[i,j\] <= -Hyp_Cvx
#'@param MapExper	Matrix of numbers	The "Experience Map" matrix : nbM rows (number of parameters), nbPc columns (number of conducted perturbations). Default value is NULL.
#'										In the document "Projet de mémoire de thèse - 1° partie", a strong asumption is made, called "H6" : 
#'										-  nbM = nbN  (the number of parameters equals the number of nodes),
#'										-  Every perturbation acts exactly upon one different node (ie.  ∂φ_i/∂p_j ≠ 0 if and only if i = j),
#'										-  A set of such perturbations acting upon every node is available.
#'										If "H6" is true, MapExper must be NULL. If "H6" is false, MapExper must be filled, and the perturbations "proportional" to a reference pertubation,
#'										ie the coordinates of the perturbations (vs. parameters) be 0 or proportional to those of the reference.
#'										If NOT NULL, this matrix contains the coordinates of the perturbations on the base of the parameters regarding those of a perturbation
#'										chosen as a reference. For instance, if "Q0" is the reference, with coordinates (x0_1, x0_2, ... x0_M), the coordinates of perturbation "Q1"
#'										must be 0 or proportional to those of Q0 (ie : x1_j = 0 or x1_j = k1*x0_j). Idem for "Q2" etc.
#'										The column of MapExper corresponding to	"Q0" must contain (1, 1, .. 1), the column corresponding to "Q1" must contain (k1, 0, k1, .., k1) if
#'										we assume that x1_2 equals 0 and the other coordinates are not null etc...
#'@param ParNode	Matrix of numbers	The "Parameters / Nodes relation" matrix : nbN rows (number of nodes) and nbM columns (number of parameters). Default value is Identity matrix.
#'										If NOT NULL, the only values allowed are 0 and 1.
#'										- ParNode\[i,j\] =  0 means  (∂φ_i)/(∂p_j ) = 0
#'										- ParNode\[i,j\] =  1 means  (∂φ_i)/(∂p_j ) ≠ 0
#'@param Relative	Logical				"Relative or absolute" values for the Global Response Matrix ("R"). Default value is TRUE.
#'										If TRUE (default value), Ri,j = 2*(Xi(P+∆Pj)-Xi(P)) / (Xi(P+∆Pj)+Xi(P)).
#'										If FALSE, Ri,j = Xi(P+∆Pj)-Xi(P). See the document ""Projet de mémoire de thèse - 1° partie".
#'@param Verbose	Logical				Default	value : FALSE.
#'										If TRUE, additional printings are made. These printings are for internal use only, so they are not documented.
#'@param NoPrint	Logical				Default	value : FALSE.
#'										If TRUE, no printings are made. Useful for tests including calls to 'MRARegress' in a loop.
#'
#'@details			
#'		nbN : number of nodes,
#'		nbP : number of perturbations, ie nbBase*nbPc, calles éQ" in the document "Projet de mémoire de thèse - 1° partie"
#'		nbM : number of parameters.
#'			If MapExper is NULL, it is mandatory that nbP >= nbN
#'			If MapExper is NOT NULL and perturbations are proportional, it is mandatory that nbP >= nbN - 1 + nbM.
#'			The rank of the system must be equal to the number of the unknowns (nbN or nbN-1+nbM) : the nbP perturbations must be "independant". 
#'		See "Projet de mémoire de thèse - 1° partie".
#'		This document provides many examples to explain these parameters.
#'		
#'		Imported libraries :
#'			- stringr		processing of strings, str_replace_all
#'			- stats			lm, as.formula
#'			- glmnet		cv.glmnet
#'			- dplyr			rename
#'			- BiocManager	may be necessary to import minet
#'			- minet			aracne, clr, mrnet, build.mim
#'			- randomForest	randomForest, importance
#'			- CVXR			convex optimization
#'			- magrittr		pipes
#'			- rootSolve		solve for the roots of n nonlinear equations (Order2)
#'			- broom			broom::tidy
#'
#'	OUTPUT:
#'
#'		   r		Matrix of numbers	Returns the "Connectivity Matrix" ("r") if no error occured. This matrix has nbN rows and nbN columns.
#'		Order2		Matrix of numbers	Returns the "Order2" coefficients (nbN rows, (nbN-1)*(2+(nbN-2)/2) columns), if Method = "Order2", and NULL otherwise.
#'						The "Order2" coefficients are defined like this, for each i in 1:nbN (i is the row number and nbN, the number of nodes) :
#'							- the first nbN-1 coefficients correspond to the "linear part" (ie. ri,j) : ri,j * DeltaXi,j with j != i.
#'							- the next nbN-1 coefficients correspond to the "quadratic part" (ie. si,j,j) : 0.5*si,j,j * (DeltaXi,j)^2 with j != i.
#'							- the last (nbN-2)*(nbN-1)/2 coefficients correspond to the "product part" (ie. si,j,k) : 
#'							  si,j,k * (DeltaXi,j)*DeltaXi,k) with j : 1 .. (nbN-1), j != i and k : (j+1) .. nbN, k!= i.
#'							  For example, if nbN=4 and i=1, this part is : s1,2,3*D1,2*D1,3, s1,2,4*D1,2*D1,4, s1,3,4*D1,3*D1,4 (since s1,2,3 = s1,3,3 etc..).
#'							  where we note Di,j the term DeltaXi,j.
#'							  If nbN=4 and i=2, this part is : s2,1,3*D2,1*D2,3, s2,1,4*D2,1*D2,4, s2,3,4*D2,3*D2,4 (idem, s2,1,3 = s2,3,1 etc...).
#'		ANOVA		Matrix of numbers	Returns the "ANOVA values", as described in "Statistical Design and Analysis of Experiments" (R.L. Mason et al, 2003), for the
#'							various methods, except "ARACNE", "CLR", "MRNET".		
#'							- A matrix
#'								This matrix has 5 rows ("SSR", "SSE", "LOF", "Pure", "TSS") 
#'								and 11 columns ("df/m", "df/M", "Sum/m", "Sum/M", "Mean/m", "Mean/M", "F/m", "F/M," "pVal/m", "pVal/M", "nbrNdes")
#'								where : SSR (or SCE) represents the variance explained by the regression, SSE (or SCR) is the estimation error, 
#'								LOF ("Lack Of Fit") is the error due to maladjustement of the model (linear, polynomial...), Pure is due to measurement noise, 
#'								TSS (or SCT) is the total variation of error, and
#'								df stands for degrees of freedom, /m and /M : minimum and maximum value for the different nodes, Sum is the sum designed by the row,
#'								Mean equals Sum/df, F is a Fisher variable used to compute the p Value of the result (see "Projet de mémoire de thèse - 1° partie", § 4.4.3).
#'								nbrNdes is the number of nodes whose p Value exceeds a threshold (0.25 for F-SSR and 0.05 for F-LOF).
#'							- Two sentences explaining the results F-SSR and 0.05 for F-LOF.
#'		PValN		Vector	A vector giving the coded p values (SSR and LOF) regarding each node.
#'		Input		List	A list composed of two lists : "Variables" and "InputPar", for internal use and to communicate with the other modules of the package MRARegress.
#'							"Variables" is a list of important variables used by the program (nbN, nbM, H6, nbBase, nbPc, nbP, PerturbN, PerturbR1, PerturbR2, MatD, 
#'								MatInc, FirstTrial, Keys, PValN).
#'							"InputPar" : list of the input parameters values. NULL values are set to their default values (except for "KnlgMap" and "Hyp_Lbda").
#'		IC95		List	A list composed of three square matrices (nbN, nbN) : ValMean, ValMin and ValMax.
#'							If Method = "TLR" (without 'a priori' knowledge), ValMean, ValMin and ValMax provide the mean value and the confidence interval (95%) of each connectivity coefficient r\[i,j\],
#'							else, these matrices are set to 0 and are not usable.
#'	

#'@import stringr
#'@import stats
#'@import glmnet
#'@importFrom dplyr rename
#'@import BiocManager
#'@import minet
#'@import randomForest
#'@import CVXR
#'@description Warnings may be generated during the CVXR library import.
#'@import magrittr
#'@import rootSolve
#'@import broom
#'

#'
#'@name			MRARegress
#'
#'@description	The function MRARegress computes the connectivity matrix, according to the document "MaRédaction.docx".
#'				The input data are described above and the outputs below.
#'
#'@return		List		NULL in case of error or a list of informations ("r", "Order2", "ANOVA","pValN", "Input", "IC95") whose content depends on the chosen method.

#'@export
#'

MRARegress <- function (MatExp, Perturb = NULL, NodeName = NULL, KnlgMap = NULL, Method = "TLR", Hyp_Lbda = NULL, Hyp_Mu = 1, Hyp_Step = "Fo", 
						Hyp_Eps = 0, Hyp_Cvx = 0, MapExper = NULL, ParNode = NULL, Relative = TRUE, Verbose = FALSE, NoPrint = FALSE) {
  tryCatch (
	expr = {
		if (! NoPrint)
			cat ("START MRARegress !", as.character(Sys.time()), "\n")
		
		toReturn				<- list()			# Return values
		toReturn[["r"]]			<- NULL
		toReturn[["Order2"]]	<- NULL
		toReturn[["ANOVA"]]		<- NULL
		toReturn[["PValN"]]		<- NULL
		toReturn[["Input"]]		<- NULL
		toReturn[["IC95"]]		<- NULL

		# Check input data
		err	<-  CheckInputData (MatExp, Perturb, NodeName, KnlgMap, Method, Hyp_Lbda, Hyp_Mu, Hyp_Step, Hyp_Eps, Hyp_Cvx, MapExper, ParNode, Relative, Verbose, NoPrint)
		if (! is.null(err)) {
			message (err)
			return (NULL)
		}

		# Variables declaration
		nbN			<- dim(MatExp)[1]										# Number of nodes (nb. of rows)
		nbBase		<- ifelse(is.null(Perturb), 1, sum(Perturb=="Base"))	# Number of basal columns
		nbPc		<- dim(MatExp)[2] - nbBase								# Number of conducted perturbations (nb. of columns)
		nbP			<- nbBase*nbPc											# Number of "perturbations" taken into account
		PerturbN	<- matrix(0, nrow=nbPc, ncol=2)							# Name of the perturbations and index of the node a perturbation acts upon
		PerturbR1	<- list()												# Name of the perturbations
		PerturbR2	<- list(list())											# Index of the replicates
		PerturbRMoy	<- list(vector(length=nbN))								# Average values of the replicates
		avg.tmp		<- vector(length=nbN)									# to compute the average
		nbPm		<- 0													# Number of perturbations (exclusive of "basal state" and "replicates")

		colAnovas	<- c("df", "Sum", "Mean", "F", "pVal")					# Column names for Anovas :
																			# Degrees of freedom, Sum of squares, Mean of the sum of squares, F, p-value
		rowAnova	<- c("SSR", "SSE", "LOF", "Pure", "TSS")				# Raw names for Anovas and Anova :
																			# Variance explained by regression, Residual variance, Lack of fit, Pure error
		Anovas		<- array(0, dim=c(length(rowAnova), length(colAnovas), nbN)) # Anova computations for each node
		PValN		<- vector(length=nbN)									# For each node : 
																			#	A	: pVal[SSR] or pVal[LOF] cannot be computed.
																			#	B	: Measures are NOT explained by the model : pVal[SSR] >= tlrSSR 
																			#	C	: Measures are explained by the model, residual errors do not correspond to the noise level :
																			#				pVal[SSR] < tlrSSR AND  pVal[LOF] < tlrLOF, 
																			#	D	: Measures are explained by the model, residual errors correspond to the noise level :
																			#				pVal[SSR] < tlrSSR AND  pVal[LOF] >= tlrLOF. 
		dimnames(Anovas)	<- list(rowAnova, colAnovas, NULL)

		bSSR		<- TRUE													# TRUE if we can compute SSR F & pValue for each node
		bLOF		<- TRUE													# TRUE if we can compute LOF F & pValue for each node
		tlrSSR		<- 0.25													# Threshold concerning SSR
		tlrLOF		<- 0.05													# Threshold concerning LOF
		colAnova	<- c("df/m", "df/M", "Sum/m", "Sum/M", "Mean/m", "Mean/M",
						"F/m", "F/M", "pVal/m", "pVal/M","nbrNdes")			# Column names for Anova (all nodes) :
																			# Degrees of freedom (min;max), Sum of squares (min;max), Mean of the sum of squares (min;max), 
																			# F (min;max), p-value (min;max), nbr of nodes exceeding the threshold.
		Anova		<- array(0, dim=c(length(rowAnova), length(colAnova))) # Anova computations
		dimnames(Anova)	<- list(rowAnova, colAnova)
		AnovaOut	<- list()												# Results of ANOVA
		
		Input		<- list()												# Input data and variables
		IC95		<- list()												# Confidence intervals
		
		Keys		<- vector(length=4)										# Keys used to check the connection between the modules of the package.

		if (is.null(MapExper)) {
			nbM		<- nbN													# Number of parameters
			H6		<- TRUE													# "H6" asumption (see "MaRedaction.docx")
			MapExper <- matrix(0, nrow=nbM, ncol=nbPc)						# To describe the links between conducted perturbations and parameters
			ParNode	 <- matrix(0, nrow=nbN, ncol=nbM)						# To describe the links between parameters and nodes
			diag(ParNode)	<- 1
		} else {
			nbM		<- dim(MapExper)[1]										# Number of parameters
			H6		<- FALSE												# "H6" asumption (see "MaRedaction.docx")
			if (is.null(ParNode)) {
				if (nbN != nbM) {
					message ("If ParNode is NULL, the number of nodes and parameters must be equal !")
					return (NULL)
				}		
				ParNode	 <- matrix(0, nrow=nbN, ncol=nbM)					# To describe the links between parameters and nodes
				diag(ParNode)	<- 1
			}
		}

		MatD		<- matrix(0, nrow=nbP, ncol=nbN)						# Matrix (2*(Xi,j-Xi) / (Xi,j+Xi) or Xi,j-Xi) transposed and coordinates of the perturbations vs. parameters.		
																			# Depends on "Relative".
																			# MatD[i, j]	: here, i represents the perturbation acting on the node j
		MatInc		<- matrix(0, nrow=nbP, ncol=nbM)						# Used to compute the coordinates of (∂φi)/(∂pj), if H6 is false

		# Results
		MatrCc		<- array(dim=c(nbN,nbN))								# Computed Connectivity Matrix (toReturn "r")
		ValMean		<- array(0, dim=c(nbN,nbN))								# Mean value of rij
		ValMin		<- array(0, dim=c(nbN,nbN))								# Min value at IC95 of rij
		ValMax		<- array(0, dim=c(nbN,nbN))								# Max value at IC95 of rij
		FirstTrial	<- TRUE													# If TRUE, results have been got without use of (∂φi)/(∂pj)
		MatrCcCol	<- vector(length=nbN)									# Columns' names of the connectivity matrix (MatrCc)
		
		rL 			<- vector(length=nbN)									# Result of Lasso method (ie sol. of Yi = Ai * Ui)		
		MatV		<- matrix(0, nrow=nbP, ncol=(nbN-1)*(2+(nbN-2)/2))		# Used by the method "Order2" : Y as a function of V			
		MatO2		<- matrix(0, nrow=nbN, ncol=(nbN-1)*(2+(nbN-2)/2))		# Coefficients to return (toReturn "Order2")

		# We analyze the input data
		if (is.null(Perturb)) {
			cBase		<- c(1)												# Position of the basal columns in MatExp
			cPerturb	<- seq(2, nbPc+1, by=1)								# Position of the perturbations columns in MatExp
		} else {
			cBase		<- which(Perturb == "Base")							# Position of the basal columns in MatExp
			cPerturb	<- which(Perturb != "Base")							# Position of the perturbations columns in MatExp		
		}

		# Looking for node names
		if (is.null(NodeName)) {
			NodeName	<- as.character(seq(1, nbN, by=1))
			NodeName	<- paste("N", NodeName, sep="")						# Name of the node
		}

		# Looking for perturbation names
		if (is.null(Perturb)) {
			if (H6 && (nbN != nbPc)) {
				message ("if Perturb is NULL and H6 TRUE, the number of nodes must be equal to the number of perturbations !")
				return (NULL)
			}			
			PerturbN[ ,1]	<- as.character(seq(1, nbPc, by=1))
			PerturbN[ ,1]	<- paste("Q", PerturbN[ ,1], sep="")			# Name of the pertubation
			PerturbN[ ,2]	<- seq(1, nbN, by=1)							# Index of the node the perturbation acts upon (in this case, nbN = nbPc is mandatory)
			
			# Construction of Perturb		
			Perturb			<- "Base"
			for (iPc in 1:nbPc) {
				idx		<- as.integer(PerturbN[iPc,2])
				Perturb	<- cbind(Perturb, paste(PerturbN[iPc,1], "->", NodeName[idx], sep=""))
			}
		} else {
			for (iPc in 1:nbPc) {
				unPert	<- Perturb[cPerturb[iPc]]							# One perturbation
				unPert	<- stringr::str_replace_all(unPert, " ", "")
				unPert	<- strsplit(unPert, "->")							# Mandatory syntax if H6 TRUE :  "Qi  -> Nj" (as many spaces as you like)

				PertName	<- unPert[[1]][1]								# Name of the perturbation
				PerturbN[iPc,1]	<- PertName									# saved here

				if (H6) {
					if (is.na(unPert[[1]][2])) {
						message ("Syntax Error")
						return (NULL)
					}
					idx		<- which(NodeName == unPert[[1]][2])			# Index of the node the perturbation acts upon
	
					if (length(idx) != 1) {
						message ("Unknown node !")
						return (NULL)
					} else {
						idx	<- as.integer(idx)
						PerturbN[iPc,2]	<- idx
					}
				} 	# if H6
			}		# for iPc
		}			# Perturb not NULL

		# Looking for replicates
		for (iPc in 1:nbPc) {		
			PertName	<- PerturbN[iPc,1]
			idx			<- PerturbN[iPc,2]

			if (PertName %in% PerturbR1) {
				iOld	<- match(PertName, PerturbN[1:(iPc-1), 1])			# Perturbation found
				if (H6 && idx != PerturbN[iOld, 2]) {
					message ("A perturbation acts upon many nodes (forbidden if H6 TRUE) !")
					return (NULL)
				}
				iOld	<- which(PerturbR1 == PertName)
				PerturbR2[[iOld]]	<- c(PerturbR2[[iOld]], iPc)
			} else {
				PerturbR1			<- c(PerturbR1, PertName)				# New perturbation					
				nbPm	<- nbPm +1
				PerturbR2[[nbPm]]	<- iPc
			}
		}

		# Construction of matrix MapExper
		if (H6) {															# if H6 FALSE, MaxExper is filled yet
			for (iPc in 1:nbPc) {
				idx	<- as.integer(PerturbN[iPc,2])
				MapExper[idx,iPc]	<- 1
			}	# for iPc
		}

		for (iPar in 1:nbM) {
			if (sum(MapExper[iPar, ]) == 0) {
				message ("A parameter or a node is not perturbed !")
				return (NULL)
			}
		}

		for (iNode in 1:nbN) {
			if (sum(ParNode[iNode, ]) == 0) {
				message ("A node is not perturbed !")
				return (NULL)
			}
		}

###				# Looking for non perturbed nodes
###				if (length (setdiff (seq(1,nbN), unique(as.integer(PerturbN[ ,2])))) > 0) {
###		cat("nbN", nbN, "PerturbN \n")
###		print(PerturbN)
###					message ("Some nodes are not perturbed !")
###					return (toReturn)
###				}

		if (Verbose) {
			cat("H6 ", H6, "nbN ", nbN, " nbPc ", nbPc, " nbBase ", nbBase, " Nodes ", NodeName, "\n")
			cat(" Perturb. & nodes \n")
			print(PerturbN)
			cat("PerturbR1 \n")
			print(PerturbR1)
			cat("PerturbR2 \n")
			print(PerturbR2)			
			cat(" MapExper \n")
			print(MapExper)
			cat("ParNode \n")
			print(ParNode)
		}

		# Columns' names of the connectivity matrix (MatrCc)
		# Association between a node and the pertubation(s) acting on it
		
		for (iNode in 1:nbN) {
			cc	<- ""
			for (j in 1:nbPc) {
				if (PerturbN[j,2] == iNode) {
					cc	<- paste(cc, PerturbN[j,1], "; ", sep="")
				}
			}
			cc	<- stringr::str_sub(cc, start=1, end=stringr::str_length(cc)-2)	# removes the final '; ' 
			MatrCcCol[iNode]	<- cc
			
			if (Verbose) {
				cat("MatrCcCol \n")
				print(MatrCcCol)
			}
		}	
		
		# Construction of the "Matrix Delta" (MatD)
		for (iBase in 1:nbBase) {
			for (iPc in 1:nbPc) {
				for (iNode in 1:nbN) {
					if (Relative)
						MatD[(iBase-1)*nbPc+iPc, iNode]		<- 2*(MatExp[iNode, cPerturb[iPc]] - MatExp[iNode, cBase[iBase]]) / (MatExp[iNode, cPerturb[iPc]] + MatExp[iNode, cBase[iBase]])
					else
						MatD[(iBase-1)*nbPc+iPc, iNode]		<-   MatExp[iNode, cPerturb[iPc]] - MatExp[iNode, cBase[iBase]]
				}
			}
		}
		
		if (Verbose) {
			cat("MatD \n")
			print(MatD)
		}

		# Calculation of the keys
		Keys[1]		<-	 3*nbN +  7*(nbPc+nbBase)
		Keys[2]		<-	19*nbN + 37*(nbPc+nbBase)
		Keys[3]		<-	sum(abs(MatD[ , ]))
		Keys[4]		<-	sum(abs(MatExp[1, ])) + sum(abs(MatExp[ ,1]))
		
		# Calculation of the averages of the replicates
		for (iQ in 1:nbPm) {
			raws	<- PerturbR2[[iQ]]
			for (iN in 1:nbN) {
				avg.tmp[iN]	 <- mean(MatD[raws,iN])
			}
			PerturbRMoy[[iQ]]	<- avg.tmp
		}
		
		# Construction of matrix MatInc
		for (iBase in 1:nbBase) {
			MatInc[((iBase-1)*nbPc+1):(iBase*nbPc), ]	<- t(MapExper)
		}
		if (Verbose) {
			cat("MatInc \n")
			print(MatInc)
		}

		# Returning input values, used by other modules of the package
		Variables	<- list(nbN=nbN, nbM=nbM, H6=H6, nbBase=nbBase, nbPc=nbPc, nbP=nbP, PerturbN=PerturbN, PerturbR1=PerturbR1, PerturbR2=PerturbR2, 
							MatD=MatD, MatInc=MatInc, FirstTrial=FirstTrial, Keys=Keys)
		InputPar	<- list(MatExp=MatExp, Perturb=Perturb, NodeName=NodeName, KnlgMap=KnlgMap, Method=Method, 
							Hyp_Lbda=Hyp_Lbda, Hyp_Mu=Hyp_Mu, Hyp_Step=Hyp_Step, Hyp_Eps=Hyp_Eps, Hyp_Cvx=Hyp_Cvx, 
							MapExper=MapExper, ParNode=ParNode, Relative=Relative, Verbose=Verbose)
		Input		<- list(InputPar=InputPar, Variables=Variables)

		# Computation of "Connection Matrix" : MatrCc
		if (Verbose) {
			cat("Method ", Method, "\n")
		}
		MatrCc[ , ] <- 0
		
		if (Method == "Random Forest") {
			if (H6)
				seqLetters	<- sequence_letters(nbN -1)		# a sequence of letters : "a", "b", "c", ... to name the columns with the Random Forest method
			else
				seqLetters	<- sequence_letters(nbN)	
		}

		if (Method %in% c("ARACNE", "CLR", "MRNET")) {
			MI <- build.mim(dataset = MatD[ ,1:nbN])		# Mutual Information matrix
			
			if (Method == "ARACNE") {		
				MatrCc  <- aracne(MI, Hyp_Eps)
			} else if (Method == "CLR") {
				MatrCc  <- clr(MI)
			} else if (Method == "MRNET") {
				MatrCc  <- mrnet(MI)
			}
			
			dimnames(MatrCc)	<- NULL
			diag(MatrCc)		<- -1
		} else {
			for (iNode in 1:nbN) {
				col		<- 1:nbN
				col		<- col[-iNode]

				# Construction of matrices "U", "Y", "U0", "Y0"
				MatY	<- MatD[ ,iNode]
				
				MatU	<- MatD[ ,col]
				nbInc	<- 0								# Number of coordinates ∆(q,j) of (∂φi)/(∂pj) to take into account
				Mat0	<- rep(0, nbP)
				for (iPar in 1:nbM) {
					if (ParNode[iNode,iPar] != 0) {
						nbInc	<- nbInc +1
						MatU	<- cbind(MatU, MatInc[ ,iPar])
						Mat0	<- Mat0 + abs(MatInc[ ,iPar])
					}
				}
				
				if (Verbose) {
					cat("\n\n\niNode", iNode, "nbInc", nbInc, "col", col, "\n")
					cat("MatY \n")
					print(MatY)
					cat("MatU \n")
					print(MatU)
				}

				MatY0	<- MatY
				MatU0	<- MatD[ ,col]
				
				# First trial : we don't use the rows where (∂φi)/(∂pj) * ∆(q,j) != 0
				FirstTrial	<- TRUE
				row		<- which(Mat0 != 0)
				MatY0	<- MatY0[-row]
				if (is.matrix(MatU0)) {
					MatU0	<- MatU0[-row, ]
				} else {
					MatU0	<- MatU0[-row]
				}

				if (Verbose) {
					cat("row", row, "Mat0 \n")
					print(Mat0)
					cat("MatY0 \n")
					print(MatY0)
					cat("MatU0 \n")
					print(MatU0)
				}
				
				# Test of the rank of the system
				if (qr(MatU0)$rank < nbN-1) {
					# Second trial : we use all the rows
					MatY0	<- MatY
					MatU0	<- MatU
					
					if (qr(MatU0)$rank < nbN-1+nbInc) {
						message ("The rank of the system is not sufficient !")
						return (NULL)
					}
					FirstTrial	<- FALSE
				}			

				if (Verbose) {
					cat ("FirstTrial", FirstTrial, "iNode ", iNode, "Rang", qr(MatU0)$rank, "MatY0 \n")
					print (MatY0)
					cat ("MatU0 \n")
					print (MatU0)
				}

				if (Method == "TLR") {
					if (is.null(KnlgMap)) {
						Form	<- "MatY0~MatU0+0"
						MatrCc[iNode,col]	<- ((lm(as.formula(Form)))$coefficients)[1:(length(col))]
						pQ <- lm(as.formula(Form))
						ValMean	[iNode,col]  	<- broom::tidy(pQ)$estimate
						ValMin  [iNode,col]  	<- broom::tidy(pQ)$estimate - 1.96*broom::tidy(pQ)$std.error	# 1.96 to get an IC 95%
						ValMax  [iNode,col]  	<- broom::tidy(pQ)$estimate + 1.96*broom::tidy(pQ)$std.error
					} else {												# We have some informations about the solution
						constraints	 <- list()
						beta		 <- CVXR::Variable(nbN-1)				# ri,j to compute according to this method

						for (iCol in 1:(nbN-1)) {
							cc	<- col[iCol]
							if (KnlgMap[iNode, cc] != "x") {	 			# There is an edge between nodes 'iNode' and 'iCol'
								nomMatr <- paste0("Matrix", iCol)
								Matr	<- matrix(0, nrow=1, ncol=nbN-1)
								Matr[1,iCol]	<- 1									

								if (KnlgMap[iNode, cc] == -1) {				# We know that node 'cc' inhibits the action of node 'iNode'
									Cond	<- Matr %*% beta <= -Hyp_Cvx
								} else if (KnlgMap[iNode, cc] == 0) {		# We know that nodes 'cc' and 'iNode' don't interact
									Cond	<- Matr %*% beta == 0
								} else {									# We know that node 'cc' amplifies the action of node 'iNode'
									Cond	<- Matr %*% beta >= Hyp_Cvx
								}

								constraints[[nomMatr]]	<- Cond							
							}
						}	# for iCol

						objective			<- CVXR::Minimize(sum((MatY0 - MatU0 %*% beta)^2))
						problem				<- CVXR::Problem(objective, constraints)
						result				<- CVXR::solve(problem)
#						MatrCc[iNode,col]	<- result$getValue(beta) %>% round(8)
						MatrCc[iNode,col]	<- round (result$getValue(beta), 8)
					}		# KnlgMap not NULL
					
				} else if (Method == "LASSO") {
					MatrCc[iNode, col]	<- (fELASTIC(MatU0, MatY0, Hyp_Lbda, 1, nbN))[2:nbN]
				}
				else if (Method == "RIDGE") {
					MatrCc[iNode, col]	<- (fELASTIC(MatU0, MatY0, Hyp_Lbda, 0, nbN))[2:nbN]
				}
				else if (Method == "Elastic Net") {
					MatrCc[iNode, col]	<- (fELASTIC(MatU0, MatY0, Hyp_Lbda, Hyp_Mu, nbN))[2:nbN]
				}
				else if (Method == "STEP") {
					MatrCc[iNode, col]	<- fSTEP(MatU0, MatY0, Hyp_Step, nbN, Verbose)
				}
				else if (Method == "Random Forest") {
					MatrCc[iNode, col]	<- fRForest(MatU0, MatY0, seqLetters, nbN-1)
				}
				else if (Method == "Order2") {
					for (iCol2 in 1:(nbN-1)) {
						MatV[ ,iCol2]		<- MatU[ ,iCol2]				# Linear terms			vs MatU0
						MatV[ ,iCol2+nbN-1]	<- MatU[ ,iCol2]^2				# Quadratic terms
					}	# iCol2
					ind	<- 2*(nbN-1) +1
					for (ind1 in 1:(nbN-2)) {
						for (ind2 in (ind1+1):(nbN-1)) {
							MatV[ ,ind]		<- MatU[ ,ind1]*MatU[ ,ind2]	# Product terms
							ind	<- ind+1
						}
					}
					MatV0	<- MatV
					MatV0	<- MatV0[-row, ]
												
					if (qr(MatV0)$rank < (nbN-1)*(2+(nbN-2)/2)) {
						message ("The rank of the system is not sufficient with this method !")
						cat ("Rank : ", qr(MatV0)$rank, " Needs :", (nbN-1)*(2+(nbN-2)/2), "\n")
						return (NULL)
					}
					
					if (Verbose) {
						cat ("iNode ", iNode, "MatY0 \n")
						print (MatY0)
						cat ("MatV0 \n")
						print (MatV0)
					}
					
					Form	<- "MatY0~MatV0+0"
					MatrCc[iNode,col]	<- ((lm(as.formula(Form)))$coefficients)[1:(nbN-1)]
					MatO2[iNode, ]		<- (lm(as.formula(Form)))$coefficients
					MatO2[iNode,nbN:(2*(nbN-1))]	<-	2*MatO2[iNode,nbN:(2*(nbN-1))]		# To be consistent with Taylor's development
																							# (quadratic terms are multiplied by 0.5)

					if (Verbose) {
						cat("iNode ", iNode, " Coefficients ", 	(lm(as.formula(Form)))$coefficients, "\n")
					}	
				}		# Order2

				diag(MatrCc)	<- -1
				if (Verbose) {
					cat ("MatrCc iNode", iNode, "\n")
					print (MatrCc[iNode, ])
				}

				# ANOVA
				# 1/ Degrees of freedom
				Anovas["SSR","df",iNode]	<- nbN-1			# Mason notation : p
				idPerts	<- which(PerturbN[ ,2] == iNode)		# Id of the perturbations acting on this node, including replicates
				nbPerts	<- length(idPerts)						# Nbr. of perturbations acting on this node. 
				nbRowsY0<- length(MatY0)						# Nbr. of equations for this node. Mason notation : n
				Anovas["SSE","df",iNode]	<- nbRowsY0-nbN+1	# Mason notation : n-p
				Meas	<- setdiff(PerturbR1, PerturbN[idPerts, 1]) # Equations acting on this node, excluding replicates.
				nbMeas	<- length(Meas)							# Nbr. of perturbations acting on this node, excluding replicates. Mason notation : m
				Anovas["Pure","df",iNode]	<- nbRowsY0-nbMeas 	# Mason notation : fp = n-m
				Anovas["LOF","df",iNode]	<- nbMeas-nbN+1		# Mason notation :n-p-fp = m-p
				Anovas["TSS","df",iNode]	<- nbRowsY0			# Mason notation : n

				if ((Anovas["SSR","df",iNode]==0) || (Anovas["SSE","df",iNode]==0))
					bSSR	<- FALSE
				if ((Anovas["LOF","df",iNode]==0) || (Anovas["Pure","df",iNode]==0))
					bLOF	<- FALSE
			
				# 2/ Sums
				ZMean	<- mean(MatY0)							# Mason notation : Zmean
				
				ZEst	<- MatY0								# To define this vector. Mason notation : Zhat
				ZEst[ ]	<- 0
				for (j in 1:(nbN-1)) {
					if (is.matrix(MatU0)) {
						ZEst[ ]	<- ZEst[ ] + MatrCc[iNode,col[j]]*MatU0[ ,j]
					} else {
						ZEst[ ]	<- ZEst[ ] + MatrCc[iNode,col[j]]*MatU0[ ]
					}
				}

				Anovas["SSR","Sum",iNode]	<- sum(sapply(ZEst-ZMean, fcarre))		# df=p
				Anovas["SSE","Sum",iNode]	<- sum(sapply(MatY0-ZEst, fcarre))		# df=n-p

				ZIMes	<- vector(length=nbMeas)				# Sum (yij - yi moy)^2
				names(ZIMes)	<- Meas
				for (Mes in Meas) {								# Name of one perturbation acting on this node, excluding replicas
					iPert	<- which(PerturbR1 == Mes)
					idPert	<- PerturbR2[[iPert]]				# Id of these perturbations in MatD
					ZMes	<- MatD[idPert,iNode]				# Measures of these perturbations. Mason notation : yij
					ZMesMean	<- mean(ZMes)
					ZIMes[Mes]	<- sum(sapply(ZMes-ZMesMean, fcarre))
				}

				Anovas["Pure","Sum",iNode]	<- sum(ZIMes[ ])
				Anovas["LOF","Sum",iNode]	<- Anovas["SSE","Sum",iNode] - Anovas["Pure","Sum",iNode]
				Anovas["TSS","Sum",iNode]	<- sum(sapply(MatY0-ZMean, fcarre))		# df=n

				# 3/ Means
				Anovas[ ,"Mean",iNode]	<- Anovas[ ,"Sum",iNode] / Anovas[ ,"df",iNode]
				
				# 4/ F  (Fisher/Snedecor) and pVal
				Anovas["SSR","F",iNode]	<- Anovas["SSR","Mean",iNode] / Anovas["SSE","Mean",iNode]
				if ((Anovas["SSR","df",iNode]>0) && (Anovas["SSE","df",iNode]>0))
					Anovas["SSR","pVal",iNode]	<- 	pf(Anovas["SSR","F",iNode], Anovas["SSR","df",iNode], Anovas["SSE","df",iNode], lower.tail=FALSE)
				else
					bSSR	<- FALSE

				Anovas["LOF","F",iNode]	<- Anovas["LOF","Mean",iNode] / Anovas["Pure","Mean",iNode]
				if ((Anovas["LOF","df",iNode]>0) && (Anovas["Pure","df",iNode]>0))
					Anovas["LOF","pVal",iNode]	<- 	pf(Anovas["LOF","F",iNode], Anovas["LOF","df",iNode], Anovas["Pure","df",iNode], lower.tail=FALSE)
				else
					bLOF	<- FALSE

				# 5/ Results
				if (Verbose) {
					cat ("iNode", iNode, "p", nbN-1, "n", nbRowsY0, "m", nbMeas, "ZMean", ZMean,"ZEst\n")
					print (ZEst)
					cat ("Anovas\n")
					print (Anovas[ , ,iNode])
				}

				if (!bSSR || !bLOF)
					PValN[iNode] = "A"				
				else if (Anovas["SSR", "pVal", iNode] >= tlrSSR)
					PValN[iNode] = "B"
				else if (Anovas["LOF", "pVal", iNode] < tlrLOF)
					PValN[iNode] = "C"
				else
					PValN[iNode] = "D"
			}		# for iNode
		}			# else ARACNE ...

		# Rows and columns name
		rownames(MatrCc)	<- NodeName
		colnames(MatrCc)	<- MatrCcCol

		# IC95
		IC95	<- list (ValMean=ValMean, ValMin=ValMin, ValMax=ValMax)
		
		# Results of ANOVA
		Anova["SSR", "df/m"]	<-	min(Anovas["SSR", "df", ])
		Anova["SSR", "df/M"]	<-	max(Anovas["SSR", "df", ])
		Anova["SSR", "Sum/m"]	<-	min(Anovas["SSR", "Sum", ])
		Anova["SSR", "Sum/M"]	<-	max(Anovas["SSR", "Sum", ])
		Anova["SSR", "Mean/m"]	<-	min(Anovas["SSR", "Mean", ])
		Anova["SSR", "Mean/M"]	<-	max(Anovas["SSR", "Mean", ])
		Anova["SSR", "F/m"]		<-	min(Anovas["SSR", "F", ])
		Anova["SSR", "F/M"]		<-	max(Anovas["SSR", "F", ])
		Anova["SSR", "pVal/m"]	<-	min(Anovas["SSR", "pVal", ])
		Anova["SSR", "pVal/M"]	<-	max(Anovas["SSR", "pVal", ])
		Anova["SSR", "nbrNdes"]	<-	length(which(Anovas["SSR", "pVal", ] >= tlrSSR))			# length(which(Anova["SSR", "pVal/M"] > tlrSSR))

		Anova["SSE", "df/m"]	<-	min(Anovas["SSE", "df", ])
		Anova["SSE", "df/M"]	<-	max(Anovas["SSE", "df", ])
		Anova["SSE", "Sum/m"]	<-	min(Anovas["SSE", "Sum", ])
		Anova["SSE", "Sum/M"]	<-	max(Anovas["SSE", "Sum", ])
		Anova["SSE", "Mean/m"]	<-	min(Anovas["SSE", "Mean", ])
		Anova["SSE", "Mean/M"]	<-	max(Anovas["SSE", "Mean", ])
		Anova["SSE", "F/m"]		<-	min(Anovas["SSE", "F", ])
		Anova["SSE", "F/M"]		<-	max(Anovas["SSE", "F", ])
		Anova["SSE", "pVal/m"]	<-	min(Anovas["SSE", "pVal", ])
		Anova["SSE", "pVal/M"]	<-	max(Anovas["SSE", "pVal", ])
		
		Anova["LOF", "df/m"]	<-	min(Anovas["LOF", "df", ])
		Anova["LOF", "df/M"]	<-	max(Anovas["LOF", "df", ])
		Anova["LOF", "Sum/m"]	<-	min(Anovas["LOF", "Sum", ])
		Anova["LOF", "Sum/M"]	<-	max(Anovas["LOF", "Sum", ])
		Anova["LOF", "Mean/m"]	<-	min(Anovas["LOF", "Mean", ])
		Anova["LOF", "Mean/M"]	<-	max(Anovas["LOF", "Mean", ])
		Anova["LOF", "F/m"]		<-	min(Anovas["LOF", "F", ])
		Anova["LOF", "F/M"]		<-	max(Anovas["LOF", "F", ])
		Anova["LOF", "pVal/m"]	<-	min(Anovas["LOF", "pVal", ], na.rm=TRUE)
		Anova["LOF", "pVal/M"]	<-	max(Anovas["LOF", "pVal", ], na.rm=TRUE)
		Anova["LOF", "nbrNdes"]	<-	length(which(Anovas["LOF", "pVal", ] < tlrLOF))				# length(which(Anova["LOF", "pVal/M"] > tlrLOF))
		
		Anova["Pure", "df/m"]	<-	min(Anovas["Pure", "df", ])
		Anova["Pure", "df/M"]	<-	max(Anovas["Pure", "df", ])
		Anova["Pure", "Sum/m"]	<-	min(Anovas["Pure", "Sum", ])
		Anova["Pure", "Sum/M"]	<-	max(Anovas["Pure", "Sum", ])
		Anova["Pure", "Mean/m"]	<-	min(Anovas["Pure", "Mean", ])
		Anova["Pure", "Mean/M"]	<-	max(Anovas["Pure", "Mean", ])
		Anova["Pure", "F/m"]	<-	min(Anovas["Pure", "F", ])
		Anova["Pure", "F/M"]	<-	max(Anovas["Pure", "F", ])
		Anova["Pure", "pVal/m"]	<-	min(Anovas["Pure", "pVal", ], na.rm=TRUE)
		Anova["Pure", "pVal/M"]	<-	max(Anovas["Pure", "pVal", ], na.rm=TRUE)	

		Anova["TSS", "df/m"]	<-	min(Anovas["TSS", "df", ])
		Anova["TSS", "df/M"]	<-	max(Anovas["TSS", "df", ])
		Anova["TSS", "Sum/m"]	<-	min(Anovas["TSS", "Sum", ])
		Anova["TSS", "Sum/M"]	<-	max(Anovas["TSS", "Sum", ])

		if (! bSSR) {
			strSSR	<-	"Unable to compute SSR\n"
		} else if (Anova["SSR", "nbrNdes"] > 0) {
			strSSR	<- paste("The error is NOT explained by the regression used for ", Anova["SSR", "nbrNdes"], " nodes out of ", nbN, "!\n")
		} else {
			strSSR	<-	"Measures are explained by the model, at least partially !\n"
		}

		if (! bLOF) {
			strLOF	<-	"Unable to compute LOF\n"
		} else if (Anova["LOF", "nbrNdes"] > 0) {
			strLOF	<- paste("Residual errors do not correspond to the noise level of the measurements for ", Anova["LOF", "nbrNdes"], " nodes out of ", nbN, "!\n")
		} else {
			strLOF	<-	"Residual errors are explained by the noise level !\n"
		}

		if (Verbose) {
			cat ("\n\nSSR \n")
			for (iNode in 1:nbN) {
				cat ("iNode : ", iNode, " Sum : ",  Anovas["SSR", "Sum", iNode],  " F : ",  Anovas["SSR", "F", iNode], " pVal : ", Anovas["SSR", "pVal", iNode], " Name : ", NodeName[iNode], "\n")
			}
			cat ("\nLOF \n")
			for (iNode in 1:nbN) {
				cat ("iNode : ", iNode, " Sum : ",  Anovas["LOF", "Sum", iNode],  " F : ",  Anovas["LOF", "F", iNode], " pVal : ", Anovas["LOF", "pVal", iNode], " PValN ", PValN[iNode], "\n")
			}
		}

		AnovaOut	<- list(Anova, strSSR, strLOF)
		
		if (! NoPrint)
			cat ("DONE !", as.character(Sys.time()), "\n")

		toReturn$r		<- MatrCc	
		toReturn$Order2	<- MatO2
		toReturn$ANOVA	<- AnovaOut
		toReturn$PValN	<- PValN
		toReturn$Input	<- Input
		toReturn$IC95	<- IC95

		return (toReturn)
	},		# expr
	
	warning = function (e) {
		message ("Warning detected !")
		print(e)	
		toReturn$r		<- MatrCc
		toReturn$Order2	<- MatO2
		toReturn$ANOVA	<- AnovaOut
		toReturn$PValN	<- PValN
		toReturn$Input	<- Input
		toReturn$IC95	<- IC95
		
		return (toReturn)
	},		# warning

	error = function (e) {
		message ("Error detected !")
		print(e)
	}		# error
  )			# tryCatch

  return (NULL)
}		# MRARegress


#' Checks the input data for function MRARegress
#'
#' This function checks the input data for function MRARegress.
#' The parameters are the same as those of MRARegress.
#'
#'@param MatExp		The "Expression Matrix".
#'@param Perturb	Refers to the name of the perturbations and the nodes they act upon.
#'@param NodeName	The name of the nodes.
#'@param KnlgMap	The "Knowledge Map".
#'@param Method		The method used to compute the connectivity matrix.
#'@param Hyp_Lbda	Hyper parameter needed by the LASSO, RIDGE, Elastic Net methods.
#'@param Hyp_Mu		Hyper parameter μ of the method 'Elastic Net'
#'@param Hyp_Step	Hyper parameter μ of the method 'Step'
#'@param Hyp_Eps	Hyper parameter ε of the method 'aracne'
#'@param Hyp_Cvx	Hyper parameter used by KnlgMap
#'@param MapExper	The "Experience Map" matrix
#'@param ParNode	The "Parameters /Nodes relation" matrix
#'@param Relative	"Relative or absolute" values for the Global Response Matrix ("R")
#'@param Verbose	If TRUE, additional printings are made. These printings are for internal use only, so they are not documented
#'@param NoPrint	If TRUE, no printings are made. Useful for tests including calls to 'MRARegress' in a loop.
#'

#'
#' @return 			A message if an error is detected and returns NULL otherwise.
#'

CheckInputData	<- function (MatExp, Perturb, NodeName, KnlgMap, Method, Hyp_Lbda, Hyp_Mu, Hyp_Step, Hyp_Eps, Hyp_Cvx, MapExper, ParNode, Relative, Verbose, NoPrint) {

	# Variables declaration
	nbN			<- dim(MatExp)[1]										# Number of nodes
	nbBase		<- ifelse(is.null(Perturb), 1, sum(Perturb=="Base"))	# Number of basal columns
	nbPc		<- dim(MatExp)[2] - nbBase								# Number of conducted perturbations
	nbP			<- nbBase*nbPc											# Number of "perturbations" taken into account	

	IdentM		<- matrix(0, nrow=nbN, ncol=nbN)						# Identity matrix
	diag(IdentM)	<- 1
	
	# Test Input Variables
	if (is.null(MapExper)) {
		H6		<- TRUE
		nbM		<- nbN													# Number of parameters
	} else {
		H6		<- FALSE
		nbM		<- dim(MapExper)[1]										# Number of parameters
	}
	
	# nbN, nbBase, nbPc, nbM
	if (nbN < 2)
		return ("Insufficient number of nodes !")
	if (nbBase < 1)
		return ("Insufficient number of basal columns !")
	if (nbPc < 1)
		return ("Insufficient number of perturbations !")
	if (nbM < 1)
		return ("Insufficient number of parameters (rows of MapExper) !")
		
	# MatExp
	if (! is.matrix(MatExp))
		return ("MatExp must be a matrix !")
	if (! is.numeric(MatExp))
		return ("MatExp must contain numbers only !")

	# Perturb
	if (is.null(Perturb) && (nbP != nbN) && is.null(MapExper))
		return ("Perturb is NULL and nb. perturbations != nb. nodes !")

	if (! is.null(Perturb)) {
		if (! is.vector(Perturb))
			return ("Perturb must be NULL or a vector !")
		if (length(Perturb) != dim(MatExp)[2])
			return ("Perturb must be NULL or have as many columns as MatExp !")
		if (nbBase <= 0)
			return ("Perturb must contain one basal column at least !")
		if (! is.null(MapExper) && (nbBase > 1))
			return ("Perturb must contain exactly one basal column if MapExper is NOT NULL !")
	}

	# NodeName
	if (! is.null(NodeName)) {
		if (! is.vector(NodeName))
			return ("NodeName must be NULL or a vector !")
		if (length(NodeName) != nbN)
			return ("NodeName must be NULL or have as many rows as MatExp !")
		if (! is.character(NodeName))
			return ("NodeName must be NULL or contain strings only !")
		if (any(duplicated(NodeName)))
			return ("Many nodes have the same name !")
	}

	# KnlgMap
	if (! is.null(KnlgMap)) {
		if (! is.matrix(KnlgMap))
			return ("KnlgMap must be NULL or a matrix !")
		if (dim(KnlgMap)[1] != dim(KnlgMap)[2])
			return ("KnlgMap must be NULL or a square matrix !")
		if (dim(KnlgMap)[1] != nbN)
			return ("KnlgMap must be NULL or have as many rows as MatExp !")
		if (sum(! (KnlgMap %in% c(0, 1, -1, "0", "1", "-1", "x"))) > 0 )
			return ("KnlgMap must contain '0', '1', '-1' or 'x' only !")
		if (Method != "TLR")
			return ("KnlgMap works only with 'TLR' method !")
	}

	# Method
	if (! is.character(Method))
		return ("Method must be a string !")
	if (! (Method %in% c("TLR", "LASSO", "RIDGE", "Elastic Net", "STEP", "ARACNE", "CLR", "MRNET", "Random Forest", "Order2")))
		return ("Unknown method !")

	# Hyper parameters
	if (! is.null(Hyp_Lbda)) {	
		if (! is.numeric(Hyp_Lbda))
			return ("Hyper parameter Hyp_Lbda must be NULL or a number !")
		if (Hyp_Lbda < 0)
			return ("Hyper parameter Hyp_Lbda must be NULL or a positive number !")
	}
	
	if (! is.numeric(Hyp_Mu))
		return ("Hyper parameter Hyp_Mu must be NULL or a number !")
	if (Hyp_Mu < 0)
		return ("Hyper parameter Hyp_Mu must be NULL or a number between 0 and 1 !")
	if (Hyp_Mu > 1)
		return ("Hyper parameter Hyp_Mu must be NULL or a number between 0 and 1 !")

	if (! (Hyp_Step %in% c("Fo", "Ba", "Bo")))
		return ("Unknown Hyper parameter Hyp_Step !")

	if (! is.numeric(Hyp_Eps))
		return ("Hyper parameter Hyp_Eps must be NULL or a number !")
	if (Hyp_Eps < 0)
		return ("Hyper parameter Hyp_Eps must be NULL or a positive number !")

	if (! is.numeric(Hyp_Cvx))
		return ("Hyper parameter Hyp_Cvx must be NULL or a number !")
	if (Hyp_Cvx < 0)
		return ("Hyper parameter Hyp_Cvx must be NULL or a positive number !")
		
	# MapExper
	if (! is.null(MapExper)) {
		if (! is.matrix(MapExper))
			return ("MapExper must be NULL or a matrix !")
		if (dim(MapExper)[2] != nbPc)
			return ("MapExper must be NULL or have as many columns as the number of conducted perturbations !")
	}

	# ParNode
	if (! is.null(ParNode)) {
		if (! is.matrix(ParNode))
			return ("ParNode must be NULL or a matrix !")
		if (dim(ParNode)[1] != nbN || dim(ParNode)[2] != nbM)
			return ("ParNode must be NULL or have as many rows as the number of nodes and as many columns as the number parameters !")
	}
	
	# Relative
	if (! (Relative %in% c("TRUE", "FALSE"))) {
		return ("Relative must be TRUE or FALSE !")
	}

	# Verbose
	if (! (Verbose %in% c("TRUE", "FALSE"))) {
		return ("Verbose must be TRUE or FALSE !")
	}

	# NoPrint
	if (! (NoPrint %in% c("TRUE", "FALSE"))) {
		return ("NoPrint must be TRUE or FALSE !")
	}
	
	# Number of perturbations
	if (nbP < nbN)
		return ("Not enough perturbations to get result !")

	if (! is.null(MapExper) && ! (dim(MapExper)[1] == nbN && dim(MapExper)[2] == nbN && all(MapExper == IdentM)) 	&& (nbP < (nbN-1+nbM)))
		return ("Not enough perturbations to get result !")

	return (NULL)			# No error detected
}		# CheckInputData


#' Computes the "Connectivity Matrix" using ELASTICNET, RIDGE or LASSO Method (library glmnet)
#'
#'@param MatU		Used to compute linear Regression
#'@param MatY		Y as a function of U
#'@param Lbda		Hyper parameter λ of the method  -- If Lbda = NULL, a "best value" of Lbda is computed directly
#'@param Mu			Hyper parameter μ of the method  -- Mu = 0 : RIDGE, Mu = 1 : LASSO, else (between 0 and 1) : Elastic Net. 
#'@param nbN		Number of nodes
#'

#'
#' @return 			NULL if an error is detected or the result ("r[iNode, ]") otherwise (vector, length = nbN)
#'

fELASTIC	<- function (MatU, MatY, Lbda, Mu, nbN) {
  tryCatch (
	expr = {
		rL 			<- vector(length=nbN)													# Result of Lasso method (ie sol. of Yi = Ai * Ui)
		
		if (is.null(Lbda)) {
			cv_model 	<- cv.glmnet(MatU, MatY, alpha = Mu, grouped = FALSE)				# Fits lasso regression model using k-fold cross-validation
			Lbda		<- cv_model$lambda.min
		}
		
		best_model 	<- glmnet(MatU, MatY, alpha = Mu, lambda = Lbda, grouped = FALSE) 		# View coefficients of best model
		rL			<-	coef(best_model)
	
		return (rL)
	},		# expr

	error = function (e) {
		message ("Error detected in ELASTIC method !")
		print(e)
	}		# error
  )			# tryCatch
  return (NULL)	
}		# fELASTIC


#' Computes the "Connectivity Matrix" using STEP Method
#'
#'@param MatU		Used to compute linear Regression
#'@param MatY		Y as a function of U
#'@param Meth		Hyper parameter of the method  -- "Fo" => "STEP-Fo", "Ba" => "STEP-Ba", "Bo" => "STEP-Bo"
#'@param nbN		Number of nodes
#'@param Verbose	Logical. If TRUE, additional printings are made. These printings are for internal use only, so they are not documented.
#'

#'
#' @return 			NULL if an error is detected or the result ("r[iNode, ]") otherwise  (vector, length = nbN-1)
#'

fSTEP	<- function (MatU, MatY, Meth, nbN, Verbose) {
  tryCatch (
	expr = {
		rij 		<- matrix(nrow=1, ncol=nbN-1)			# Intermediate calculation of rij - Use a matrix instead of a vector to use 'colnames'
		
		Donnees  <- data.frame(MatY, MatU)
		if (Verbose) {
			cat(" Data ", colnames(Donnees), "\n")
		}
		Donnees	 <- rename(Donnees, "Y" ="MatY")			# The follow-up is clearer like this
		cc 		 <- colnames(Donnees)						# Name of the columns "Data". The first one is "Y"
		cc		 <- cc[-1]									# It remains the name of the coefficients ("X1", "X2", ... "Xn"  -- n = nbN-1)
		colnames(rij) <- cc									# STEP gives the column names corresponding to the coefficients that are kept
				
		switch(Meth, 
			"Fo" =
			{intercept_only <- lm(Y ~ 1, data=Donnees)
				all 	<- lm(Y ~ ., data=Donnees)
				forward <- step(intercept_only, direction='forward', scope=formula(all), trace=0)},	# Forward
			"Ba" = 
			{intercept_only <- lm(Y ~ 1, data=Donnees)
				all 	<- lm(Y ~ ., data=Donnees)
				forward <- step(all, direction='backward', scope=formula(all), trace=0)},				# Backward
			"Bo" =
			{intercept_only <- lm(Y ~ 1, data=Donnees)
				all 	<- lm(Y ~ ., data=Donnees)
				forward <- step(intercept_only, direction='both', scope=formula(all), trace=0)},		# Both
			)
		
		ll  	<- length(forward$coefficients)				# Number of coefficients kept by step
		nn		<- names(forward$coefficients)				# Name of these coefficients

		rij[1, ]	<- 0
		if (ll >= 2) {
			for (i in 2:ll) {								# nn[1] = "(Intercept)"
				rij[1, nn[i]] <- forward$coefficients[nn[i]]
			}
		}

		if (Verbose) {
			cat (" ll ", ll, " nn ", nn, " rij ", rij, "\n")
		}
		return(rij[1, ])
	},		# expr

	error = function (e) {
		message ("Error detected in STEP method !")
		print(e)
	}		# error
  )			# tryCatch
  return (NULL)	
}		# fSTEP


#' Computes the "Connectivity Matrix" using Random Forest Method (library randomForest)
#'
#'@param MatU		Used to compute linear Regression
#'@param MatY		Y as a function of U
#'@param seqLetters	A sequence of lower case letters "a", "b" .... used to name the columns
#'@param nbN		Number of nodes
#'

#'
#' @return 			NULL if an error is detected or the result ("r[iNode, ]") otherwise  (vector, length = nbN-1)
#'

fRForest	<- function (MatU, MatY, seqLetters, nbN) {
  tryCatch (
	expr = {
		rL 			<- vector(length=nbN-1)					# Result of Random Forest method
		nbLmin		<- 80									# minimum number of rows to use Random Forest method
		
		MatA		<- cbind(MatY, MatU)
		nr			<- nrow(MatA)
		
		nrep		<- round(nbLmin/nr)
		MatB		<- MatA
		if (nrep > 1) {
			for (i in 1:nrep) {									# to have enough rows
				MatB	<- rbind(MatB, MatA)
			}
		}
		colnames(MatB)	<- c("Y", seqLetters)
		
		rf			<- randomForest(Y ~ ., data=MatB, importance=TRUE)
		rL			<- importance(rf)[,1]/sum(abs(importance(rf)[,1]))
				
		return (rL)
	},		# expr

	error = function (e) {
		message ("Error detected in Random Forest method !")
		print(e)
	}		# error
  )			# tryCatch
  return (NULL)	
}		# fRForest


#' Computes a sequence of letters : "a", "b", "c", ...., "z", "aa", "ab", .... , "az", "ba", "bb" etc ....
#'
#'@param	n		The number of elements of the sequence
#'

#'
#' @return			A vector, length n, containing the sequence of letters
#'

sequence_letters <- function(n) {
  tryCatch (
	expr = {
		seq <- character(n)
		letters <- c(letters, "")
		
		for (i in 1:n) {
			indices <- c()
			k <- i - 1
			while (k >= 0) {
			indices <- c((k %% 26) + 1, indices)
			k <- k %/% 26 - 1
			}
			seq[i] <- paste0(letters[indices], collapse = "")
		}
  
		return(seq)
	},		# expr

	error = function (e) {
		message ("Error detected in sequence_letters function!")
		print(e)
	}		# error
  )			# tryCatch
  return (NULL)			
}		# sequence_letters


#' Computes a squared value
#'
#'@param	x		The value to square (may be a vector)
#'

#'
#' @return			A vector, with the values squared
#'
fcarre	= function(x) {x^2}
