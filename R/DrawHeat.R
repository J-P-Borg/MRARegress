#'@title "DrawHeat" :   represents "r" as a "matrix heat chart"
#'
#'@description : this function represents the Connectivity Matrix "r" as a "matrix heat chart", where the colours are a function of the (absolute) value of r\[i,j\]
#'
#'@param  Ret		List or Matrix		list of informations delivered by "MRARegress" or square matrix of numbers.
#'										If 'Ret' is a list of informations delivered by "MRARegress", DrawHeat uses 'Ret$r' (connectivity matrix) 
#'										and 'Ret$InputPar$NodeName' (name of the nodes).
#'										If 'Ret' is a square matrix, it corresponds to a connectivity matrix, and the names of the nodes are the names of the columns if defined,
#'										or a sequence of numbers if not.
#'@param  Absolute	Logical				If TRUE, the heatmap relates to the absolute value of the data (ie : we are interested only in the force of action from node A to node B, 
#'										regardless of amplification or inhibition).		Default value is FALSE.
#'@param  Title		String				Title of the plot. Default value is NULL.
#'@param  FontSize	Number 				Base fontsize for the plot. Default value is 10
#'@param  Colour	Vector of 100 RGB values	Defines the colour palette. Default is 'RdYlBu'. Possible values : 'Spectral', 'RdYlGn', 'RdYlBu', 'RdGy', 'RdBu', 'PuOr', 'PRGn', 'PiYG', 'BrBG'
#'										See : https://www.datanovia.com/en/blog/the-a-z-of-rcolorbrewer-palette/    "Diverging palettes" 
#'@param  Legend	Logical				To determine if legend should be drawn or not.		Default value is TRUE
#'@param  DisplayNumbers	Logical 	If TRUE, the numeric values are printed in the cells.	Default value is TRUE
#'@param  DecNumbers Number 			The number of decimal places of the numbers displayed in the cells (if any).	Default value is 2
#'@param  FileName	String				Default value is NULL
#'										If Not NULL, the plot is saved in a .pdf file, whose name is "FileName.pdf"
#'@param  NoPrint	Logical				Default	value : FALSE.
#'										If TRUE, no printings are made. Useful for tests including calls to 'DrawHeat' in a loop.

#'		
#'@details	
#'		Imported libraries :
#'		- pheatmap						Drawing of a heatmap
#'		- RColorBrewer					Used by colour palette
#'		- stringr						str_replace_all
#'		- grDevices						colorRampPalette, dev.cur, dev.off
#'
#'	OUTPUT:
#'
#'		Variables	list of the input parameters values. NULL values are set to their default values

#'@import pheatmap
#'@import RColorBrewer
#'@import stringr
#'@importFrom  grDevices colorRampPalette
#'@importFrom  grDevices dev.cur
#'@importFrom  grDevices dev.off
#'
#'
#'@name			DrawHeat
#'
#'@description	Displays the "matrix heat chart" associated with the connectivity matrix.
#'				The input data are described above.
#'
#'@return		List			NULL in case of error or a list of the input parameters values. For internal use only.
#'
#'@export
#'


DrawHeat <- function (Ret, Absolute=FALSE, Title=NULL, FontSize=10, Colour="RdYlBu", Legend=TRUE, DisplayNumbers=TRUE, DecNumbers=2, FileName=NULL, NoPrint=FALSE) {
  tryCatch (
	expr = {
		if (! NoPrint)
			cat ("START DrawHeat !", as.character(Sys.time()), "\n")

		toReturn	<- list()			# Return values
		toReturn[["Variables"]]	<- NULL
		
		# Check input data
		err	<-  CheckInputDataDH (Ret, Absolute, Title, FontSize, Colour, Legend, DisplayNumbers, DecNumbers, FileName, NoPrint)
		if (! is.null(err)) {
			message (err)
			return (NULL)
		}

		# Variables declaration
		if (is.matrix(Ret)) {
			nbN			<- dim(Ret)[1]									# Number of nodes (nb. of rows)
			if (is.null (rownames(Ret))) {
				nodesName	<- as.character(seq(1:nbN))					# Name of the nodes
			} else {
				nodesName	<- rownames(Ret)							# Name of the nodes
			}
			MatrCc		<- Ret											# Connectivity matrix
		} else {
			nbN			<- Ret$Input$Variables$nbN						# Number of nodes (nb. of rows)
			nodesName	<- Ret$Input$InputPar$NodeName					# Name of the nodes
			MatrCc		<- Ret$r										# Connectivity matrix
		}

		if (Absolute)
			MatrCc		<- abs(MatrCc)

		rownames(MatrCc)	<- nodesName
		colnames(MatrCc)	<- nodesName

		if (is.null(FileName))
			filename	<- NA
		else
			filename	<- paste(FileName, ".pdf", sep="")
		Color 	<-	colorRampPalette(rev(brewer.pal(n = 11, name = Colour)))(100)
		NumberF	<- 	paste("%.", DecNumbers, "f", sep="")
		if (is.null(Title))
			main	<- NA
		else
			main	<- Title

		if (dev.cur() != 1)
			dev.off()						# Device may be blocked
			
		pheatmap(MatrCc, color=Color, cluster_rows=FALSE, cluster_cols=FALSE, legend=Legend, main=main, fontsize=FontSize, angle_col=45, display_numbers=DisplayNumbers, number_format=NumberF, filename=filename)
		toReturn$Variables	<- list (Ret, Absolute, Title, FontSize, Colour, Legend, DisplayNumbers, DecNumbers, FileName, NoPrint)
			
		if (! NoPrint)
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
}		# DrawHeat



#' Checks the input data for function DrawHeat
#'
#' This function checks the input data for function DrawHeat.
#' The parameters are the same as those of DrawHeat.
#'
#'@param  Ret				list of informations delivered by "MRARegress" or square matrix of numbers.
#'@param  Absolute			If TRUE, the heatmap relates to the absolute value of the data.
#'@param  Title				Title of the plot.
#'@param  FontSize			Base fontsize for the plot.
#'@param  Colour			Defines the colour palette.
#'@param  Legend			To determine if legend should be drawn or not.
#'@param  DisplayNumbers	If TRUE, the numeric values are printed in the cells.
#'@param  DecNumbers 		The number of decimal places of the numbers displayed in the cells (if any).
#'@param  FileName			If Not NULL, the plot is saved in a .pdf file, whose name is "FileName.pdf"
#'@param  NoPrint			If TRUE, no printings are made. Useful for tests including calls to 'DrawHeat' in a loop.
#'

#'
#' @return 			A message if an error is detected and returns NULL otherwise.
#'

CheckInputDataDH 	<- function (Ret, Absolute, Title, FontSize, Colour, Legend, DisplayNumbers, DecNumbers, FileName, NoPrint) {
  tryCatch (
	expr = {
		# Test input data : Ret or Matr
		if (is.vector(Ret, mode="numeric")) {
			return ("Ret must not be a vector !")
		}
		
		if (is.matrix(Ret)) {
			if (dim(Ret)[1] != dim(Ret)[2])
				return ("If 'Ret' is a matrix, it must be a square matrix !")			
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

		# Absolute
		if (! (Absolute %in% c("TRUE", "FALSE"))) {
			return ("Absolute must be TRUE or FALSE !")
		}
	
		# Test Title
		if (! is.null(Title) && ! is.character(Title))
			return ("Title must be NULL or a string !")

		# Test FontSize
		if (! is.numeric(FontSize))
			return ("FontSize must be a number !")
		if (FontSize <= 0)
			return ("FontSize must be a strictly positive number !")

		# Colour
		if (! (Colour %in% c('Spectral', 'RdYlGn', 'RdYlBu', 'RdGy', 'RdBu', 'PuOr', 'PRGn', 'PiYG', 'BrBG'))) {
			return ("Colour must be one of these values : 'Spectral', 'RdYlGn', 'RdYlBu', 'RdGy', 'RdBu', 'PuOr', 'PRGn', 'PiYG', 'BrBG' !")
		}

		# Legend
		if (! (Legend %in% c("TRUE", "FALSE"))) {
			return ("Legend must be TRUE or FALSE !")
		}

		# DisplayNumbers
		if (! (DisplayNumbers %in% c("TRUE", "FALSE"))) {
			return ("DisplayNumbers must be TRUE or FALSE !")
		}

		# Test DecNumbers
		if (! is.numeric(DecNumbers))
			return ("DecNumbers must be a number !")
		if (DecNumbers < 0)
			return ("DecNumbers must be a  positive or nul number !")

		# Test FileName
		if (! is.null(FileName) && ! is.character(FileName))
			return ("FileName must be NULL or a valid file name (without extension) !")

		# NoPrint
		if (! (NoPrint %in% c("TRUE", "FALSE"))) {
			return ("NoPrint must be TRUE or FALSE !")
		}
			
		return (NULL)			# No error detected
	},		# expr
	
	warning = function (e) {
		message ("DrawHeat : check input parameters : Warning detected !")
		print(e)	
		return ("Warning")
	},		# warning
	
	error = function (e) {
		message ("DrawHeat : check input parameters : Error detected !")
		print(e)
	}		# error
  )			# tryCatch
  
  return ("Error")
}		# CheckInputDataDH
