#'@title "Classify" :  Classifies the connectivity coefficients
#'
#'@description : this function discretizes the connectivity coefficients, then classifies them according to a set of classes
#'	For instance \{-1, 0, 1\} or \{0, 1\}. The classification method may depend on the method used to compute the connectivity coefficients.
#'
#'@param  Ret	List or Matrix			list of informations delivered by "MRARegress" or matrix of numbers.
#'										If 'Ret' is a list of informations delivered by "MRARegress", Classify uses 'Ret$r' (connectivity matrix).
#'@param Classes	Vector				A vector showing the classes to define (for instance, c(0,1) : default value, or c(-1, 0, 1)). Values must be increasing.
#'@param MethDiscr	String				The method to use to classify the matrix. Default is NULL.
#'										If Ret is a list of informations delivered by "MRARegress" AND MethDiscr is NULL AND there are two classes (Classes = c(0,1)), 
#'											this method will depend on the method used to compute the connectivity matrix (Ret$Input$InputPar$Method), namely :
#'											- TLR: 	rDig\[i,j\] = 1 if abs(Ret$r\[i,j\]) > Lbda * max(abs(Ret$r\[i,j\])) else rDig\[i,j\] = 0 
#'											  (rDig is the result of "Classify" and Lbda an hyper-parameter. See below)
#'											- LASSO: 			rDig\[i,j\] = 1 if abs(Ret$r\[i,j\]) > Lbda  else rDig\[i,j\] = 0
#'											- RIDGE: 			rDig\[i,j\] = 1 if abs(Ret$r\[i,j\]) > Lbda  else rDig\[i,j\] = 0
#'											- Elastic Net:		rDig\[i,j\] = 1 if abs(Ret$r\[i,j\]) > Lbda  else rDig\[i,j\] = 0
#'											- STEP: 			rDig\[i,j\] = 1 if abs(Ret$r\[i,j\]) > Lbda  else rDig\[i,j\] = 0
#'											- ARACNE:			rDig\[i,j\] = 1 if abs(Ret$r\[i,j\]) is in the  Lbda % top values  else rDig\[i,j\] = 0 
#'											- CLR: 				rDig\[i,j\] = 1 if abs(Ret$r\[i,j\]) is in the  Lbda % top values  else rDig\[i,j\] = 0 
#'											- RMNET: 			rDig\[i,j\] = 1 if abs(Ret$r\[i,j\]) is in the  Lbda % top values  else rDig\[i,j\] = 0 
#'											- Random Forest: 	rDig\[i,j\] = 1 if abs(Ret$r\[i,j\]) is in the  Lbda % top values  else rDig\[i,j\] = 0
#'										Else,
#'											MethDiscr must be defined among the following methods :
#'											- Threshold		rDig\[i,j\] will be put into a class, comparing Ret$r\[i,j\] (or Ret\[i,j\]) to a threshold (see "Details" below)
#'											- Top			rDig\[i,j\] will be put into a class, comparing Ret$r\[i,j\] (or Ret\[i,j\]) to a percentage of the assigned values (see "Details" below)
#'@param Lbda		Vector or Number	An hyper-parameter specific to the method. It's a number if there are two classes and a vector if there are more. Default is NULL.
#'										If Ret is a list of informations delivered by "MRARegress" AND Lbda is NULL AND Classes = c(0,1),
#'											Lbda default value is specific to the method used to compute the connectivity matrix, namely :
#'											- TLR: 				Lbda = 0.25
#'											- LASSO: 			Lbda = 0
#'											- RIDGE: 			Lbda = 0
#'											- Elastic Net:		Lbda = 0
#'											- STEP: 			Lbda = 0
#'											- ARACNE:			Lbda = 20
#'											- CLR: 				Lbda = 20 
#'											- RMNET: 			Lbda = 20 
#'											- Random Forest: 	Lbda = 20
#'										Else, Lbda must be defined.
#'										If there are more than two classes, Lbda\[1\] determines the first class, Lbda\[2\] determines the second class, and so on (see "Details" below).
#'										The length of the vector Lbda needs to equal the number of classes minus one.
#'										If MethDiscr = "Threshold", Lbda values must be in increasing order.
#'										If MethDiscr = "Top", Lbda values must be in decreasing order.
#'@param Verbose	Logical				Default	value : FALSE.
#'										If TRUE, additional printings are made. These printings are for internal use only, so they are not documented.
#'
#'@details								Examples : let  Res = MRARegress(MatExp)  (Method = "TLR" : default value)  and  Sqr be a matrix
#'										- A = Classify(Res) means Classes = c(0,1), A$rDig\[i,j\] = 1 if abs(Res$r\[i,j\] > 0.25 * max(abs(Res$r\[i,j\])) else A$rDig\[i,j\] = 0
#'										- A = Classify(Res, Lbda=0.5) means Classes = c(0,1), A$rDig\[i,j\] = 1 if abs(Res$r\[i,j\] > 0.5 * max(abs(Res$r\[i,j\])) else A$rDig\[i,j\] = 0
#'										- A = Classify(Res, MethDiscr="Top", Lbda=10) means Classes = c(0,1), A$rDig\[i,j\] = 1 if Res$r\[i,j\] belongs to the top 10% values, else 0
#'										(note that, when MethDiscr is NOT NULL, like here, we test Res$r and not abs(Res$r))
#'										- A = Classify(Res, Classes=c(-1,0,1), Lbda=10)  : incorrect. If there are more than two classes, MethDiscr and Lbda must be defined.
#'										- A = Classify(Res, Classes=c(-1,0,1), MethDiscr="Threshold", Lbda=0.3)  : incorrect. The length of the vector Lbda needs to equal 2.
#'										- A = Classify(Res, classes=c(-1,0,1), MethDiscr="Threshold", Lbda=c(-0.3,0.25)) means :
#'										A$rDig\[i,j\] = -1 if Res$r\[i,j\] <= -0.3  * max(abs(Res$r\[i,j\]))
#'										A$rDig\[i,j\] =  0 if Res$r\[i,j\] >  -0.3  * max(abs(Res$r\[i,j\])) AND Res$r\[i,j\] <= 0.25 * max(abs(Res$r\[i,j\]))
#'										A$rDig\[i,j\] =  1 if Res$r\[i,j\] >   0.25 * max(abs(Res$r\[i,j\]))
#'										- B = Classify(Sqr)  : incorrect. If the parameter "Ret" is a matrix, MethDiscr and Lbda must be specified.
#'										- B = Classify(Sqr, MethDiscr="Threshold", Lbda=0.3) means Classes = c(0,1), B$rDig\[i,j\] = 1 if Sqr\[i,j\] > 0.3 * max(abs(Sqr\[i,j\])) else 0
#'										- B = Classify(Sqr, classes=c(-1,0,1), MethDiscr="Threshold", Lbda=c(-0.3,0.25)) means :
#'										B$rDig\[i,j\] = -1 if Sqr\[i,j\] <= -0.3  * max(abs(Sqr\[i,j\]))
#'										B$rDig\[i,j\] =  0 if Sqr\[i,j\] >  -0.3  * max(abs(Sqr\[i,j\])) AND Sqr\[i,j\] <= 0.25 * max(abs(Sqr\[i,j\]))
#'										B$rDig\[i,j\] =  1 if Sqr\[i,j\] >   0.25 * max(abs(Sqr\[i,j\]))
#'										- B = Classify(Sqr, classes=c(-1,0,1), MethDiscr="Top", Lbda=c(50,10)) means :
#'										B$rDig\[i,j\] =  1 if Sqr\[i,j\] belongs to the top 10%
#'										B$rDig\[i,j\] =  0 if Sqr\[i,j\] belongs to the interval \[top 10%, top 50%\[
#'										otherwise, B$rDig\[i,j\] = -1 

#'
#'@name			Classify
#'
#'@description	Classifies the connectivity coefficients.
#'				The input data are described above and the outputs below.
#'
#'@return		List					NULL in case of error or a list of informations ("rDig", "Input").
#'	rDig		Matrix of numbers		Returns the input matrix classified if no error occured, ie, each element of the input matrix is replaced by the class to which
#'										this item belongs. This matrix has the same number of rows and columns than the input matrix.
#'  Input		List					list of the input parameters values + "Method" and "ThresholdPar" (used to classify). NULL values are not replaced.
#'

#'@export
#'


Classify <- function (Ret, Classes = c(0,1), MethDiscr = NULL, Lbda = NULL, Verbose = FALSE) {
  tryCatch (
		expr = {
		cat ("START Classify !", as.character(Sys.time()), "\n")

		toReturn	<- list()			# Return values
		toReturn[["rDig"]]	<- NULL
		toReturn[["Input"]]	<- NULL

		if (is.vector(Ret, mode="numeric"))
			Ret	<- matrix(Ret, nrow=1)		# transformed into a matrix
	
		# Check input data
		err	<-  CheckInputDataCY (Ret, Classes, MethDiscr, Lbda, Verbose)
		if (! is.null(err)) {
			message (err)
			return (NULL)
		}
  
 		# Variables declaration
		nbClasses		<- length(Classes)								# Number of classes

		if (is.matrix(Ret)) {
			nbN			<- dim(Ret)[1]									# Number of nodes (nb. of rows)
			nbCol		<- dim(Ret)[2]									# Number of columns
			MatrCc		<- Ret											# Connectivity matrix to classify
		} else {
			nbN			<- Ret$Input$Variables$nbN						# Number of nodes (nb. of rows)
			nbCol		<- nbN											# Number of columns
			MatrCc		<- Ret$r										# Connectivity matrix to classify
			diag(MatrCc) <- 0
		}

		Mx	<- max(abs(MatrCc))
		if (is.null(Lbda)) {
			if (Ret$Input$InputPar$Method %in% c("TLR")) {
				ThresholdPar	<- 0.25									# Threshold used to classify
			} else if (Ret$Input$InputPar$Method %in% c("LASSO", "RIDGE", "Elastic Net", "STEP")) {			
				ThresholdPar	<- 0									# Threshold used to classify
			} else {
				ThresholdPar	<- 20									# Threshold used to classify
			}		
		} else {
			ThresholdPar		<- Lbda									# Threshold used to classify
		}		

		if (! is.matrix(Ret) && Ret$Input$InputPar$Method %in% c("TLR")) {
			ThresholdPar	<- ThresholdPar*Mx							# Threshold used to classify
		}
			
		if (is.null(MethDiscr)) {
			if (Ret$Input$InputPar$Method %in% c("TLR", "LASSO", "RIDGE", "Elastic Net", "STEP")) {
				Method	<- "Threshold"									# Method used to classify
			} else {
				Method	<- "Top"										# Method used to classify
			}
			if (nbClasses == 2) {
				MatrCc	<- abs(MatrCc)									# Comparison concerns absolute values
			}
		} else {
			Method	<- MethDiscr										# Method used to classify
		}

		if (Method == "Threshold") {
			if (! all(ThresholdPar == cummax(ThresholdPar))) {
				message ("Method = 'Threshold' -- Lbda must be in increasing order !")
				print(message)
				return (NULL)
			}
		} else {
			if (! all(ThresholdPar == cummin(ThresholdPar))) {
				message ("Method = 'Top' -- Lbda must be in decreasing order !")
				print(message)
				return (NULL)
			}
		}		

		InputPar	<- list (Ret=Ret, Classes=Classes, MethDiscr=MethDiscr, Lbda=Lbda, Verbose=Verbose)

		if (Method == "Threshold") {
			fThresh <- function (x) {Thresh (x, ThresholdPar)}
			Input		<- list (InputPar, Method=Method, Thresholds=ThresholdPar)
			if (Verbose) {
				cat ("Method :", Method, " Thresholds :", ThresholdPar, "\n")
			}
		} else {
			QSeuils	<- quantile(MatrCc, probs=(1-ThresholdPar/100), names=FALSE)		# Thresholds corresponding to quantiles
			Input		<- list (InputPar, Method=Method, Thresholds=QSeuils)
			if (Verbose) {
				cat ("Method :", Method, " Thresholds :", QSeuils, "\n")
			}
			fThresh <- function (x) {Thresh (x, QSeuils)}
		}
		
		Result	<- Classes [sapply(MatrCc, fThresh)]
		rDig	<- matrix(Result, nrow=nbN, ncol=nbCol)					# Classified connectivity matrix
		rownames(rDig)	<- rownames(MatrCc)
		colnames(rDig)	<- colnames(MatrCc)

		toReturn$rDig		<- rDig
		toReturn$Input		<- Input

		cat ("DONE !", as.character(Sys.time()), "\n")
		return (toReturn)
	},		# expr
		
	warning = function (e) {
		message ("Warning detected !")
		print(e)
		return (toReturn)
	},		# warning
	
	error = function (e) {
		message ("Error detected !")
		print(e)
	}		# error
  )			# tryCatch
  
  return (NULL)
}		# Classify


#' Checks the input data for function Classify
#'
#' This function checks the input data for function Classify.
#' The parameters are same as those of Classify.
#'
#'@param Ret		list of informations delivered by "MRARegress" or a matrix.
#'@param Classes	A vector showing the classes to define.
#'@param MethDiscr	The method to use to classify the matrix. 
#'@param Lbda		An hyper-parameter specific to the method.
#'@param Verbose	If TRUE, additional printings are made. These printings are for internal use only, so they are not documented.
#'

#'
#' @return 			A message if an error is detected and returns NULL otherwise.
#'

CheckInputDataCY	<- function (Ret, Classes, MethDiscr, Lbda, Verbose) {
  tryCatch (
	expr = {
		# Test input data : Ret or Matr
		if (is.matrix(Ret)) {
			if (! is.numeric(Ret))
				return ("If 'Ret' is a matrix, it must contain numbers only !")
		} else {
			# Variables declaration
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
		}
		
		# Test 'Classes'
		if (! is.numeric(Classes))
			return ("Classes must be NULL or contain numbers only !")
		if (length(Classes) < 2)
			return ("Two classes at least !")
		if (! all(Classes == cummax(Classes)))
			return ("Classes must be in ascending order !") 

		# MethDiscr
		if (! is.null(MethDiscr)) {
			if (! is.character(MethDiscr))
				return ("MethDiscr must be NULL or a string !")
			if (! (MethDiscr %in% c("Threshold", "Top")))
				return ("Unknown method !")
		} else {
			if (is.matrix(Ret) || length(Classes) > 2)
				return ("If Ret is a matrix or nb. classes > 2, MethDiscr must not be NULL !")
		}

		# Lbda
		if (! is.null(Lbda)) {
			if (! is.numeric(Lbda))
				return ("Lbda must be NULL or numeric !")
			if (length(Lbda) != (length(Classes) -1))
				return ("Length (Lbda) must equal length (Classes) - 1 !")
		} else {
			if (is.matrix(Ret) || length(Classes) > 2)
				return ("If Ret is a matrix or nb. classes > 2, Lbda must not be NULL !")
		}

		return (NULL)			# No error detected
	},		# expr
	
	warning = function (e) {
		message ("DrawGraph : check input parameters : Warning detected !")
		print(e)	
		return ("Warning")
	},		# warning
	
	error = function (e) {
		message ("DrawGraph : check input parameters : Error detected !")
		print(e)
	}		# error
  )			# tryCatch
  
  return ("Error")
}		# CheckInputDataCY


#' Finds the class of a value, according thresholds
#'
#'@param	x		The value to classify (may be a vector)
#'@param	Seuils	We compare x with these values
#'

#'
#' @return			A vector, with the class of the value
#'
Thresh	<- function(x, Seuils) {
	i <- 1
	while (i <= length(Seuils)) {
		if (x <= Seuils[i]) {
			return (i)
		}
		i	<- i+1
	}
	return (i)
}
