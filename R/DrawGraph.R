#'@title "DrawGraph" :   Displays "r" as a graph
#'
#'@description : this function represents the Connectivity Matrix "r" as a graph where the nodes are the genes (for instance)
#'	and an edge A $\\rightarrow$ B reflects the action of node A on node B (amplification), while a dashed line (with a "T" arrow  betwwen A and B) reflects an inhibition 
#'	of node B by node A.
#'	The graph is a "valued oriented graph", and values, next to edges, mean the "strength" of this action.
#'
#'@param  Ret	List or Matrix		list of informations delivered by "MRARegress" or square matrix of numbers.
#'									If 'Ret' is a list of informations delivered by "MRARegress", DrawGraph uses 'Ret$r' (connectivity matrix) 
#'									and 'Ret$InputPar$NodeName' (name of the nodes).
#'									If 'Ret' is a square matrix, it corresponds to a connectivity matrix, and the names of the nodes are the names of the columns if defined,
#'									or a sequence of numbers if not.
#'@param  Title	String				Title of the graph. Default value is NULL.
#'@param  Thr	Number (>= 0)		A threshold to determine edge existence. Default value is 0.1.
#'									An edge is drawn between nodes 'i' and 'j' iff connectivity coefficient is >= Thr (ie \|MatrCc\[i,j\]\| >= Thr)
#'	
#'@details	
#'		Imported libraries :
#'		- BiocManager (version 1.30.22). To install this version, R version >= 4.3 is mandatory
#'		- RCy3							Communication with Cytoscape
#'		- utils	compareVersion			To check Cytoscape version
#'		
#'		Cytoscape (version >= 3.6.1) must be installed and running.
#'
#'	OUTPUT:
#'
#'  	theGraph	List of dataframes
#'					nodes (list of nodes : name, PValN), edges (list of edges : source, target, interaction, value).
#'		Variables	list of the input parameters values. NULL values are set to their default values.
#'					list of important variables used by the program (Thr, nbN, MatrCc, nodesName).

#'@import BiocManager
#'@import RCy3
#'@importFrom utils   compareVersion
#'
#'
#'@name			DrawGraph
#'
#'@description	Displays the graph associated with the connectivity matrix.
#'				The input data are described above and the outputs below.
#'
#'@return		List			NULL in case of error or a list of informations ("theGraph", "Variables"). For internal use only.

#'@export
#'


DrawGraph <- function (Ret, Title=NULL, Thr=0.1) {
  tryCatch (
	expr = {
		cat ("START DrawGraph !", as.character(Sys.time()), "\n")

		toReturn	<- list()			# Return values
		toReturn[["theGraph"]]	<- NULL
		toReturn[["Variables"]]	<- NULL
		
		# Check Cytoscape connection and version
		cytoscapePing()
		Vers	<- cytoscapeVersionInfo ()
		if (compareVersion(Vers[2], "3.6.1") < 0)	{
			message ("Cytoscape too old. Download the latest version !")
			return (NULL)
		}		
		
		# Check input data
		err	<-  CheckInputDataDG (Ret, Title, Thr)
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
			PValN		<- vector(length=nbN)							#  pVal[SSR] or pVal[LOF] cannot be computed for the nodes
			PValN[]		<- -1
		} else {
			nbN			<- Ret$Input$Variables$nbN						# Number of nodes (nb. of rows)
			nodesName	<- Ret$Input$InputPar$NodeName					# Name of the nodes
			MatrCc		<- Ret$r										# Connectivity matrix
			PValN		<- Ret$PValN									# pVal (SSR and LOF) regarding the nodes
																		#	A	: pVal[SSR] or pVal[LOF] cannot be computed.
																		#	B	: Measures are NOT explained by the model : pVal[SSR] >= tlrSSR 
																		#	C	: Measures are explained by the model, residual errors do not correspond to the noise level : pVal[SSR] < tlrSSR AND  pVal[LOF] < tlrLOF, 
																		#	D	: Measures are explained by the model, residual errors correspond to the noise level : pVal[SSR] < tlrSSR AND  pVal[LOF] >= tlrLOF
		}

		Id			<- NULL												# Name of the nodes
		pVal		<- NULL												# pVal regarding the nodes	
		
		source		<- NULL												# Source of the edges
		target		<- NULL												# Target of the edges
		interaction	<- NULL												# Source action on target : "d" : amplifies, "r" : inhibits
		value		<- NULL												# Action magnitude

		# Network construction
		for (iNode in 1:nbN) {
			Id		<- c(Id, nodesName[iNode])
			pVal	<- c(pVal, PValN[iNode])
		}

		nodes	<- data.frame(id=Id, group=pVal, stringsAsFactors=FALSE)														# List of the nodes

		diag(MatrCc)	<- 0
		for (jCol in 1:nbN) {
			for (iRow in 1:nbN) {
				if (abs(MatrCc[iRow,jCol]) < Thr)
					next
				source	<- c(source, nodesName[jCol])
				target	<- c(target, nodesName[iRow])
				value	<- c(value,  MatrCc[iRow,jCol] %>% round(3))
				if (MatrCc[iRow,jCol] > 0)
					interaction	<- c(interaction, "d")
				else
					interaction	<- c(interaction, "r")
			}
		}

		edges	<- data.frame(source=source, target=target, interaction=interaction, value=value, stringsAsFactors=FALSE)		# List of the edges
	
		createNetworkFromDataFrames (nodes,edges, title=Title, collection="MRARegress_tests")
		
		toReturn$theGraph	<- list (nodes, edges)
		toReturn$Variables	<- list (Thr, nbN, MatrCc, nodesName)

		# Style definition
		style.name	<- "MRARegress_style"
	
		defaults	<- list(
#						NODE_FILL_COLOR = "#89D0F5",
						NODE_SIZE		= 35,
						EDGE_SOURCE_ARROW_SIZE	= 6.0,
						EDGE_TARGET_ARROW_SIZE	= 6.0,
						EDGE_WIDTH				= 2.0,
						EDGE_LABEL_AUTOROTATE	= TRUE,					# The edge label will be automatically rotated so that the label aligns with the edge
						EDGE_LABEL_POSITION		= "C,C,c,0.00,-8.00")	# The position of the edge label relative to the edge. Centered, vertical offset -8 px
		nodeLabels 		<- mapVisualProperty('node label','id','p')		# Nodes name comes from "id" and "passthrough"
		nodeFills		<- mapVisualProperty('node fill color', 'group', 'd', c("A","B","C","D"), c("#FC4E2A","#FC4E2A","#FEB24C","#9ECAE1"))	# Node colour according to PValN
		arrowShapes		<- mapVisualProperty('Edge Target Arrow Shape','interaction','d',c("d","r"),c("Arrow","T"))
									# Shape of the arrow, according to discrete values ('d'). Possible values are "d" or "r". Corresponding extremity shape "Arrow" or "T"
		edgeLabels		<- mapVisualProperty('edge label','value','p','float')
		edgeLineType	<- mapVisualProperty('edge line type','interaction','d',c("d","r"),c("SOLID","EQUAL_DASH"))
		
		createVisualStyle (style.name, defaults, list(nodeLabels,nodeFills,arrowShapes,edgeLabels,edgeLineType))
		setVisualStyle (style.name)

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
}		# DrawGraph


#' Checks the input data for function DrawGraph
#'
#' This function checks the input data for function DrawGraph.
#' The parameters are the same as those of DrawGraph.
#'
#'@param Ret		list of informations delivered by "MRARegress" or a square matrix.
#'@param Title		Title of the graph.
#'@param Thr		Threshold to determine edge existence
#'

#'
#' @return 			A message if an error is detected and returns NULL otherwise.
#'

CheckInputDataDG	<- function (Ret, Title, Thr) {
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

		# Test 'Title'
		if (! is.null(Title) && ! is.character(Title))
			return ("Title must be NULL or a string !")
		
		# Test threshold value
		if (! is.numeric(Thr))
			return ("Thr must be a number !")
		if (Thr <= 0)
			return ("Thr must be a strictly positive number !")
			
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
}		# CheckInputDataDG
