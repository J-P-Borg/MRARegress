#'@title "rCI":  Computes the Confidence Intervals of the Connectivity Matrix
#'
#'@description: this function computes the Confidence Intervals for each coefficient of the Connextivity Matrix, except the diagonal
#'
#' 	This function applies only to networks that can be simulated from ODE (that one integrates from a starting point 'St').
#'	For other cases, you can use the return value of MRARegress (Ret$IC95).
#'	This function carries out 'nbD' random draws. For each of them, it realizes the planned perturbations (vector 'Perturbs') and the 'nbRep' desired replicates, by simulating a noise N (0, k*XMoy), 
#'	where Xmoy is the mean of the undisturbed data expressions (positive numbers).
#'	For each trial, 'rCI' computes the connection matrix 'r' (dimension nbN*nbN) using 'MRARegress', with all parameters provided on call ('KnlgMap', 'Method', hyperparameters, 'MapExper', 'Relative').
#'	These parameters are directly transmitted to 'MRARegress', and therefore have the same meaning as for this program.
#'	From the nbD matrices 'r' obtained, 'rCI' calculates the '95% confidence interval' (min, mean and max), for every coefficient (nbN*(nbN-1) : the diagonal, the value of which is fixed to -1, is excluded).
#'	These values are returned as three square matrices (nbN, nbN), called 'ValMean', 'ValMin' and 'ValMax', whose diagonals are set to 0.
#'	In addition, if the exact solution is provided as a '.rda' file (parameter 'Solution' not NULL : Solution = '***.rda'), the program stores in a file called '***.pdf', located in the directory indicated 'Root'
#'	(which must be writable), a graph showing the 95% confidence intervals (dark segment) and the exact value (green dot) for each connectivity coefficient. The abscissa of this graph consists of two lines.
#'	The first one represents the destination nodes numbers and the last one, the original node names. This last line may have to be modified by hand, to adapt to the resolution of the screen or the figure to print.
#'
#'
#'@param	Ret		List			list of informations delivered by "MRARegress", corresponding to a call from 'MRARegress'. This call is an example of the desired process (Method, hyperparameters etc...).
#'									The various parameters of this call of 'MRARegress' (number of nodes, names, 'KnlgMap', 'Method', hyperparameters, 'MapExper', 'Relative' etc...) are automatically 
#'									deducted from 'Ret'.
#'@param 	Func	Function		Function of P (parameters) and X (expression values to compute).
#'@param 	St		Vector			Multiroot starts from these values, to solve the system of ODE.
#'@param 	P0		Vector			Initial value of the parameters to perturb (for example, this value can represent the amount of protein, associated with a gene, present in the cell 
#'									at the beginning of transcription of this gene).
#'@param	Perturbs  Vector		Set of perturbations. The parameters to perturb are multiplied successively by Perturbs\[i\].
#'									There are no replicates in this list. The replicates will be generated automatically by 'rCI'.
#'@param	nbRep	Int (>0) 		Number of replicates to generate for every perturbation, including the initial value. 
#' 									For example, suppose Perturbs = c(-0.1, 0.3) and nbRep=3. Suppose also that each node Ni has exactly one parameter Pi associated, whose initial value is P0i. 
#'									rCI will generate 6 expression values for node Ni : 3 draws corresponding to 0.9*P0i and 3 draws corresponding to 1.3*P0i.
#'@param	nbD		Int (>0)		Number of random draws to compute the "Confidence Intervals" ('CI').
#'@param	K		Number (>=0)	Used to compute the simulated noise (independant gaussian variables N (0, k*XMoy) added to every measure, perturbed or not. 
#'									Xmoy is the mean of the undisturbed data expressions, computed automatically by 'rCI'.
#'@param	Solution   String		If NOT NULL, name of a '.rda' file ('***/xxx.rda'), containing the exact values of the connectivity coefficients. 
#'									These coefficients must be in a square matrix (nbN, nbN), named 'xxx' (same as 'xxx.rda'), and stored with the R command : "save ('xxx', file='xxx.rda')".
#'									Must be NOT NULL if Root is NOT NULL.
#'									Default value is NULL.
#'@param	Root      String		Name of a writable directory ('yyyy'). Must be NOT NULL if Solution is NOT NULL.
#'									If 'Solution' and 'Root' are not null, 'rCI' generates a graph showing the IC and exact values of the connectivity coefficients and stores it as a '.pdf' file, 
#'									called 'yyyy/xxx.pdf'. Default value is NULL.
#'@param 	Verbose	  Logical		Default	value : FALSE.
#'									If TRUE, additional printings are made. These printings are for internal use only, so they are not documented.
#'	
#'@details	
#'		Imported libraries :
#'		- 'ggplot2'		To draw the graph
#'		- 'stringr'		To split names
#'		- 'grDevices'	To create pdf files
#'
#'	OUTPUT:
#'
#'  	IC95	Dataframe containing three square matrices (nbN, nbN), called 'ValMean', 'ValMin' and 'ValMax', whose diagonals are set to 0.
#'				ValMean\[i,j\] : mean value of the r\[i,j\] got by the 'nbD' draws
#'				ValMin\[i,j\]  : min. value of the 2.5% - 97.5% quantile of the r\[i,j\] got by the 'nbD' draws (95% Confidence Interval)
#'				ValMax\[i,j\]  : max. value of the 2.5% - 97.5% quantile of the r\[i,j\] got by the 'nbD' draws (95% Confidence Interval)
#'		Input	A list containing the Input Parameters, for internal use only.
#'		file	A graph showing the IC and exact values of the connectivity coefficients, saved as a '.pdf' file, if Solution and Root are NOT NULL.


#'@import ggplot2
#'@import stringr
#'@importFrom  grDevices pdf
#'
#'
#'@name			rCI
#'
#'@description	Computes the Confidence Intervals for each coefficient of the Connextivity Matrix.
#'				The input data are described above and the outputs below.
#'
#'@return		List		NULL in case of error or a list of informations ("IC95", "Input").

#'@export
#'


rCI <- function (Ret, Func, St, P0, Perturbs, nbRep, nbD, K, Solution=NULL, Root=NULL, Verbose=FALSE) {
  tryCatch (
	expr = {
		cat ("START rCI !", as.character(Sys.time()), "\n")

		toReturn			<- list()			# Return values
		toReturn[["IC95"]]	<- NULL
		toReturn[["Input"]]	<- NULL

		# Check input data
		err	<-  CheckInputDataCI (Ret, Func, St, P0, Perturbs, nbRep, nbD, K, Solution, Root, Verbose)
		if (! is.null(err)) {
			message (err)
			return (NULL)
		}

		toReturn$Input	<- list (Ret, Func, St, P0, Perturbs, nbRep, nbD, K, Solution, Root, Verbose)
		
		# Variables declaration
		#	Variables deducted from 'Ret'. See 'MRARegress' for an explanation of these parameters.
		nbN			<- Ret$Input$Variables$nbN						# Number of nodes (nb. of rows)
		nbBase		<- Ret$Input$Variables$nbBase					# Number of "basal columns"
		nodesName	<- Ret$Input$InputPar$NodeName					# Name of the nodes
		KnlgMap		<- Ret$Input$InputPar$KnlgMap					# The "Knowledge Map"
		Method		<- Ret$Input$InputPar$Method					# Data processing method
		Hyp_Lbda	<- Ret$Input$InputPar$Hyp_Lbda					# Hyper-parameter
		Hyp_Mu		<- Ret$Input$InputPar$Hyp_Mu					# Hyper-parameter
		Hyp_Step	<- Ret$Input$InputPar$Hyp_Step					# Hyper-parameter
		Hyp_Eps		<- Ret$Input$InputPar$Hyp_Eps					# Hyper-parameter
		Hyp_Cvx		<- Ret$Input$InputPar$Hyp_Cvx					# Hyper-parameter
		MapExper	<- Ret$Input$InputPar$MapExper					# The "Experience Map" matrix 
		ParNode		<- Ret$Input$InputPar$ParNode					# The "Parameters /Nodes relation" matrix
		Relative	<- Ret$Input$InputPar$Relative					# "Relative or absolute" values for the Global Response Matrix ("R")

		nbPert		<- length(Perturbs)*nbN							# Number of perturbations (we assume that the number of perturbations per node is the same)
		
		if (Verbose) {
			cat ("nbN ", nbN, " nbBase ", nbBase, " Method ", Method, " Relative ", Relative, "\n")
			cat ("KnlgMap ");	print(KnlgMap)
			cat ("MapExper ");	print(MapExper)
			cat ("ParNode ");	print(ParNode)
			cat ("St "); 		print (St)
			cat ("P0 "); 		print (P0)
			cat ("nbPert ", nbPert, "Perturbs "); print (Perturbs)
		}

		#	Other variables
		MatrCc		<- array (0, dim=c(nbN,nbN,nbD))									# Connectivity matrix ('r')
		ValMean		<- matrix (0, nrow=nbN, ncol=nbN)									# mean value of the r[i,j] got by the 'nbD' draws
		ValMin		<- matrix (0, nrow=nbN, ncol=nbN)									# min. value of the 2.5% - 97.5% quantile of the r[i,j] got by the 'nbD' draws (95% Confidence Interval)
		ValMax		<- matrix (0, nrow=nbN, ncol=nbN)									# max. value of the 2.5% - 97.5% quantile of the r[i,j] got by the 'nbD' draws (95% Confidence Interval)
		MatExp		<- matrix (0, nrow=nbN, ncol=nbBase + nbRep*nbPert)				# Expression Matrix (see 'MRARegress')
		MapExper1	<- matrix (0, nrow=dim(MapExper)[1], ncol=nbRep*nbPert)				# New "Experience Map" matrix, to take replicates into account (see 'MRARegress')
		PertName	<- vector (length=nbRep*nbPert)										# Perturbations name

		#	Computation of the initial "Expression Matrix", nbN rows (number of nodes), nbPc + one basal columns.
		#	To get the "Expression Matrix", we replicate the first column (non perturbed, ie 'basal' column) nbBase times and every pertubed column, nbRep times.
		Exp_		<-	MExp (nbN, Perturbs, P0, Func, St)			# Initial Expression  -- only one basal column, number of perturbations per node identical with this version of MExp
		
		NPCol		<-	Exp_$Exp[ ,1]								# Non perturbed expression ('basal' column)
		XMoy		<- mean(abs(NPCol))								# Mean value of the non perturbed expression values
		for (iRep in 1:nbBase) {
			MatExp[ ,iRep]	<- NPCol
			PertName[iRep]	<- "Base"
		}

		for (iPert in 1:(nbPert)) {
			PCol	<- Exp_$Exp[ ,1+iPert]							# Perturbed expressions
			name	<- Exp_$Pert[1+iPert]
			Pert1	<- str_trim((str_split(name, "->", simplify=TRUE))[1])		# Perturbation name
			Pert2	<- str_trim((str_split(name, "->", simplify=TRUE))[2])		# Node name

			for (iRep in 1:nbRep) {
				MatExp[ ,nbBase+(iRep-1)*nbPert+iPert]	<- PCol
				PertName[nbBase+(iRep-1)*nbPert+iPert]	<- paste(Pert1, "_Rep_", iRep, "->", Pert2, sep="")
			}	#iRep
		}		# iPert

		#	To get the new "Experience Map" matrix, we replicate every column of MapExper, nRep times
		for (iPert in 1:(nbPert)) {
			EMCol	<- MapExper[ ,iPert]
			
			for (iRep in 1:nbRep) {
				MapExper1[ ,(iRep-1)*nbPert+iPert]	<- EMCol
			}
		}

		if (Verbose) {
			cat ("XMoy : ", XMoy, "\n")
			cat ("Exp_ \n"); 		print (Exp_)
			cat ("MatExp \n"); 		print (MatExp)
			cat ("PertName\n");		print (PertName)
			cat ("MapExper1\n");	print (MapExper1)
		}

		#	Noisy "Expression Matrix"
		Sd	<- K*XMoy					# Standard deviation of the noise
		set.seed(12345)					# So as to generate the same seqences of noise

		for (iDraw in 1:nbD) {
			MatExpN	<- MatExp + rnorm(nbN*(nbBase+nbRep*nbPert), mean=0, sd=Sd)				# Noisy expression matrix
			Res		<-	MRARegress(MatExpN, PertName, nodesName, KnlgMap, Method, Hyp_Lbda, Hyp_Mu, Hyp_Step, Hyp_Eps, Hyp_Cvx, MapExper1, ParNode, Relative, NoPrint=TRUE)
			if (is.null (Res$r)) {
				message ("Data non valid from MRARegress !")
				return (NULL)
			}		
			MatrCc[ , ,iDraw] <- Res$r
		}

		if (Verbose) {
			cat ("MatrCc \n"); 	print (MatrCc)
		}

		#	Statistical computation
		for (iRow in 1:nbN) {
			col <- 1:nbN
			col <- col[-iRow]
			
			for (iCol in col) {	
				ValMean[iRow,iCol] 	<- mean(MatrCc[iRow,iCol, ])
				qq	<- quantile(MatrCc[iRow,iCol, ], probs=c(0.025,0.975))		# IC 95%
				ValMin [iRow,iCol] 	<- qq[1]
				ValMax [iRow,iCol] 	<- qq[2]
			}	# Loop on iCol
		}		# Loop on iRow

		toReturn$IC95	<- list (Mean=ValMean, Min=ValMin, Max=ValMax)

		if (! is.null(Solution)) {
			ind			<- 0					# Index of the object to plot
			val			<- 0					# Value of the object to plot
			lower		<- 0					# Min. value of the objects to plot
			upper		<- 0					# Max. value of the objects to plot
			
			load (Solution)						# Solution must be the full name of a '.rda' file  (ie, created with 'save')
			Solution	<- str_replace_all (Solution, "\\\\", "/")										# "C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Solution_6K.rda"
			NomSol		<- (strsplit(basename (Solution), ".", fixed=TRUE))[[1]][1]						# "Solution_6K"
			MatrSol		<- get(NomSol)
			nbCols		<- ifelse (Method == "Order2", nbN-1, nbN)

			VMin		<- min(ValMin[ , ], na.rm=TRUE)		# Minimum on the Y axis
			VMin		<- min(VMin, MatrSol[ ,1:nbCols], na.rm=TRUE)
			VMax		<- max(ValMax[ , ], na.rm=TRUE)		# Maximum on the Y axis
			VMax		<- max(VMax, MatrSol[ ,1:nbCols], na.rm=TRUE)
			
			if (Verbose) {
				cat ("MatrSol \n"); 	print (MatrSol)
				cat ("VMin ", VMin, " VMax ", VMax, "\n")
			}				

			Matr.df		<- data.frame(mean=NULL, lower=NULL, upper=NULL)		# To draw the figure
			MatrSol.df	<- data.frame(val=NULL)
			for (iMat in 1:nbN) {
				Matr.tmp.df		<- data.frame(name=1:nbN, mean=as.vector(ValMean[ ,iMat]), lower=as.vector(ValMin[ ,iMat]), upper=as.vector(ValMax[ ,iMat]))
				Matr.tmp.df		<- Matr.tmp.df[-iMat, ]
				Matr.df			<- rbind(Matr.df, Matr.tmp.df)
				Matr.tmp.df		<- data.frame(name=" ", mean=VMin, lower=VMin, upper=VMin)
				Matr.df			<- rbind(Matr.df, Matr.tmp.df)

				MatrSol.tmp.df	<- data.frame(name=1:nbN, val=as.vector(MatrSol[1:nbN,iMat]))
				MatrSol.tmp.df	<- MatrSol.tmp.df[-iMat, ]
				MatrSol.df		<- rbind(MatrSol.df, MatrSol.tmp.df)
				MatrSol.tmp.df	<- data.frame(name=" ", val=VMin-1.0)
				MatrSol.df		<- rbind(MatrSol.df, MatrSol.tmp.df)
			}
			Matr.df		<- cbind(ind=1:(nbN*nbN), Matr.df)
			MatrSol.df	<- cbind(ind=1:(nbN*nbN), MatrSol.df)

			OName	<- ""
			for (iNode in 1:nbN) {
				OName	<- paste (OName, nodesName[iNode], " (", iNode, ")              ", sep="")			# Names of the origins of the edges (the columns of the connectivity matrix)
			}		
			Breaks	<-  c(1:(nbN*nbN))

			Root	<- str_trim(Root)								# Clean repertory name
			Root	<- str_replace_all (Root, "\\\\", "/")	
			lRoot	<- str_length(Root)
			if (str_sub(Root, lRoot, lRoot) != "/")
				Root	<- paste(Root, "/", sep="")

			NomFic	<- paste (Root, NomSol, ".pdf", sep="")
			pdf(file=NomFic, height=10, width=10) 					# Sizes default to 7
			pp	<- ggplot()+ 
				geom_linerange(data=Matr.df, 	aes(x=ind, ymin=lower, ymax=upper),  linewidth=0.9, color="black") +
				geom_hline(yintercept=0, linewidth=1, color="orange") +
				geom_point(data=MatrSol.df, aes(x=ind, y=val), size=1.8, color="green") +		# vs 0.9
				ylim(VMin-0.5, VMax+0.5) +														# remove the points corresponding to NA (NA replaced by VMin for computations)
				labs(title=NomSol, x="", y="") + scale_x_continuous(breaks=Breaks, labels=Matr.df$name) +
				xlab(OName) +
				theme(axis.text=element_text(size=12))
			print (pp)
			dev.off()
		}

		cat ("DONE !", as.character(Sys.time()), "\n")
		return (toReturn)
	},		# expr

##		warning = function (e) {
##			message ("Warning detected !")
##			print(e)
##	
##			if (str_detect(as_string(e[[1]]), pattern="geom")) {
##				print (pp)
##				dev.off()
##			}
##			return (toReturn)
##		},		# warning
	
	error = function (e) {
		message ("Error detected !")
		print(e)
	}		# error
  )			# tryCatch

  return (NULL)
}		# rCI


#' Checks the input data for function rCI
#'
#' This function checks the input data for function rCI.
#' The parameters are the same as those of rCI.
#'
#'@param 	Ret		  list of informations delivered by "MRARegress".
#'@param 	Func	  Function of P (parameters) and X (expression values to compute).
#'@param 	St		  Multiroot starts from these values, to solve the system of ODE.
#'@param 	P0		  Initial value of the parameters to perturb.
#'@param	Perturbs  Set of perturbations.
#'@param	nbRep	  Number of replicates to generate for every perturbation, including the initial value. 
#'@param	nbD		  Number of random draws to compute the "Confidence Intervals" ('CI').
#'@param	K		  Used to compute the simulated noise.
#'@param	Solution  Name of a '.rda' file ('***/xxx.rda'), containing the exact values of the connectivity coefficients. 
#'@param	Root      Name of a writable directory ('yyyy').
#'@param 	Verbose	  If TRUE, additional printings are made. These printings are for internal use only, so they are not documented.

#'
#' @return 			A message if an error is detected and returns NULL otherwise.
#'

CheckInputDataCI 	<- function (Ret, Func, St, P0, Perturbs, nbRep, nbD, K, Solution, Root, Verbose) {
  tryCatch (
	expr = {
		# 	Test	Ret 
		KeysR		<- Ret$Input$Variables$Keys						# Keys read
		KeysC		<- vector(length=4)								# Keys computed from data
	
		nbN			<- Ret$Input$Variables$nbN						# Number of nodes
		nbPc		<- Ret$Input$Variables$nbPc                   # Number of conducted perturbations
		nbBase		<- Ret$Input$Variables$nbBase                 # Number of basal columns
		MatD		<- Ret$Input$Variables$MatD                   # Matrix (2*(Xi,j-Xi) / (Xi,j+Xi) or Xi,j-Xi) transposed and coordinates of the perturbations vs. parameters.
		MatExp		<- Ret$Input$InputPar$MatExp					# "Expression Matrix", see the document "Projet de mémoire de thèse - 1° partie"
		
		# Calculation of the keys
		KeysC[1]	<-	 3*nbN +  7*(nbPc+nbBase)
		KeysC[2]	<-	19*nbN + 37*(nbPc+nbBase)
		KeysC[3]	<-	sum(abs(MatD[ , ]))
		KeysC[4]	<-	sum(abs(MatExp[1, ])) + sum(abs(MatExp[ ,1]))
	
		if (KeysC[1] != KeysR[1] || KeysC[2] != KeysR[2] || KeysC[3] != KeysR[3] || KeysC[4] != KeysR[4])
			return ("Ret is NOT a valid return from MRARegress !")

		# 	Test	Func
		if (! is.function(Func))
			return ("Func must be a function of P (parameters) and X (expression values) !")

		#	Test	St
		if (! is.numeric(St))
			return ("St must contain numbers only !")
		if (length(St) != nbN)
			return ("St must contain as many numbers as tne number of nodes !")

		#	Test	P0
		if (! is.numeric(P0))
			return ("P0 must contain numbers only !")
		if (length(P0) != nbN)
			return ("P0 must contain as many numbers as tne number of nodes !")

		#	Test	Perturbs
		if (! is.numeric(Perturbs))
			return ("Perturbs must contain numbers only !")

		#	Test	nbRep
		if (! is.numeric(nbRep))
			return ("nbRep must be a positive integer !")
		if (nbRep <= 0)
			return ("nbRep must be a positive integer !")
		if (nbRep != round(nbRep))
			return ("nbRep must be a positive integer !")

		#	Test	nbD
		if (! is.numeric(nbD))
			return ("nbD must be a positive integer !")
		if (nbD <= 0)
			return ("nbD must be a positive integer !")
		if (nbD != round(nbD))
			return ("nbD must be a positive integer !")

		#	Test	K
		if (! is.numeric(K))
			return ("K must be a positive or zero number !")
		if (K < 0)
			return ("K must be a positive or zero number !")

		#	Test	Solution
		if (! is.null(Solution)) {
			if (! is.character(Solution))
				return ("Solution must be NULL or be a string !")
			if (! file.exists(Solution))
				return ("Solution must be the name of an existing '.rda' file !")
			ExtSol	<- (strsplit(basename (Solution), ".", fixed=TRUE))[[1]][2]				# Extension of the file name
			if (ExtSol != "rda")
				return ("Solution must be the name of an existing '.rda' file !")
			if (is.null(Root))
				return ("Root must be the name of a folder !")
		}

		#	Test	Root
		if (! is.null(Root)) {
			if (! is.character(Root))
				return ("Root must be NULL or be a string !")
			if (! file.exists(Root))
				return ("Root must be the name of an existing folder !")
			if (file.access(Root, mode=2) < 0)
				return ("Root must be the name of a writable folder !")
			if (is.null(Solution))
				return ("Solution must be the name of an existing '.rda' file !")
		}

		# 	Test	Verbose
		if (! (Verbose %in% c("TRUE", "FALSE"))) {
			return ("Verbose must be TRUE or FALSE !")
		}
			
		return (NULL)			# No error detected
	},		# expr
	
	warning = function (e) {
		message ("rCI : check input parameters : Warning detected !")
		print(e)	
		return ("Warning")
	},		# warning
	
	error = function (e) {
		message ("rCI : check input parameters : Error detected !")
		print(e)
	}		# error
  )			# tryCatch
  
  return ("Error")
}		# CheckInputDataCI
