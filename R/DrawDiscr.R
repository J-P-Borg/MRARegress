#'@title "DrawDiscr" :   displays "rDiscr" and "Score" as a graph
#'
#'@description : this function represents the "Discretized Connectivity Matrix" (rDiscr) as a graph, if "Classes = c(0,1)" only, compared to a reference \cr
#'	The nodes are the genes (for instance) and the edges represent the interaction between genes:\cr
#'	Black solid lines correspond to "True Positive" edges (ie, an edge between A and B is discoverd by the method, and this edge exists in the "reference"),\cr
#'	Red solid lines correspond to "False Positive" edges (ie, an edge between A and B is discoverd by the method, and this edge doesn't exist in the "reference"),\cr
#'	Green solid lines correspond to "False Negative" edges (ie, an edge between A and B is not discoverd by the method, but this edge exists in the "reference").
#'
#'@param  rDiscr		Matrix of numbers		The "Discretized Connectivity Matrix" (delivered by "Classify" -- Classes = c(0,1) only)
#'@param  Ref			Matrix of numbers		The "Reference Matrix"
#'@param  Title			String					Title of the graph. Default value is NULL.
#'

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
#'					nodes (list of nodes : name, PValN), edges (list of edges : source, target, value, interaction, edgeType).
#'		Variables	list of the input parameters values. NULL values are set to their default values

#'@import BiocManager
#'@import RCy3
#'@importFrom utils   compareVersion
#'
#'
#'@name			DrawDiscr
#'
#'@description	Displays the "Discretized Connectivity Matrix" (rDiscr) as a graph, compared to a reference.
#'				The input data are described above.
#'
#'@return		List			NULL in case of error or a list of the input parameters values. For internal use only.
#'
#'@export
#'


DrawDiscr <- function (rDiscr, Ref, Title=NULL) {
  tryCatch (
	expr = {
		cat ("START DrawDiscr !", as.character(Sys.time()), "\n")

		toReturn	<- list()			# Return values
		toReturn[["theGraph"]]	<- NULL
		toReturn[["Variables"]]	<- NULL

		Thr			<- 0.5				# Possible classes : (0, 1)

		# Check Cytoscape connection and version
		cytoscapePing()
		Vers	<- cytoscapeVersionInfo ()
		if (compareVersion(Vers[2], "3.6.1") < 0)	{
			message ("Cytoscape too old. Download the latest version !")
			return (NULL)
		}

		# Check input data
		err	<-  CheckInputDataDD (rDiscr, Ref, Title)
		if (! is.null(err)) {
			message (err)
			return (NULL)
		}

		# Variables declaration
		nodesName	<- rDiscr$Input$InputPar$NodeName					# Name of the nodes
		MatrCc		<- rDiscr$rDig										# Connectivity matrix
		nbN			<-  dim(MatrCc)[1]									# Number of nodes

		Id			<- NULL												# Name of the nodes
		source		<- NULL												# Source of the edges
		target		<- NULL												# Target of the edges
		edgeType	<- NULL												# Edge type : "TP" (True Positive), "FP" (False Positive), "FN" (False Negative)

		# Network construction
		for (iNode in 1:nbN) {
			Id		<- c(Id, nodesName[iNode])
		}

		nodes	<- data.frame(id=Id, stringsAsFactors=FALSE)			# List of the nodes

		diag(MatrCc)	<- 0
		for (jCol in 1:nbN) {
			for (iRow in 1:nbN) {
				if (abs(MatrCc[iRow,jCol]) < Thr) {						#  No edge discovered
					if (abs(Ref[iRow,jCol]) < Thr) {
						next											#  and no edge in Ref : good, nothing to draw.
					} else {
						source	<- c(source, nodesName[jCol])			#  and edge in Ref : False Negative
						target	<- c(target, nodesName[iRow])
						edgeType	<- c(edgeType, "FN")
					}
				} else {												# Edge discovered
					if (abs(Ref[iRow,jCol]) < Thr) {
						source	<- c(source, nodesName[jCol])			#  and no edge in Ref : False Positive
						target	<- c(target, nodesName[iRow])
						edgeType	<- c(edgeType, "FP")
					} else {
						source	<- c(source, nodesName[jCol]			)#  and edge in Ref : good, True Positive
						target	<- c(target, nodesName[iRow])
						edgeType	<- c(edgeType, "TP")
					}				
				}
			}			# for iRow
		}				# for iCol

		edges	<- data.frame(source=source, target=target, edgeType=edgeType, stringsAsFactors=FALSE)		# List of the edges

		createNetworkFromDataFrames (nodes, edges, title=Title, collection="MRARegress_tests")
		
		toReturn$theGraph	<- list (nodes, edges)
		toReturn$Variables	<- list (nbN, MatrCc, Ref, nodesName)

		# Style definition
		style.name	<- "MRARegress_style2"
	
		defaults	<- list(
						NODE_FILL_COLOR = "#89D0F5",
						NODE_SIZE		= 35,
						EDGE_LINE_TYPE	= "SOLID",
						EDGE_SOURCE_ARROW_SIZE	= 6.0,
						EDGE_TARGET_ARROW_SIZE	= 6.0,
						EDGE_TARGET_ARROW_SHAPE	= "Arrow",
						EDGE_WIDTH				= 2.0,
						EDGE_LABEL_AUTOROTATE	= TRUE,					# The edge label will be automatically rotated so that the label aligns with the edge
						EDGE_LABEL_POSITION		= "C,C,c,0.00,-8.00")	# The position of the edge label relative to the edge. Centered, vertical offset -8 px
		nodeLabels 		<- mapVisualProperty('node label','id','p')		# Nodes name comes from "id" and "passthrough"
		edgeLineColor	<- mapVisualProperty('edge unselected paint','edgeType','d',c("TP","FP", "FN"),c("black","red", "green"))
		
		createVisualStyle (style.name, defaults, list(nodeLabels,edgeLineColor))
		setVisualStyle (style.name)
		matchArrowColorToEdge(TRUE, style.name = "MRARegress_style2")

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
}		# DrawDiscr


#' Checks the input data for function DrawDiscr
#'
#' This function checks the input data for function DrawDiscr.
#' The parameters are the same as those of DrawDiscr.
#'
#'@param  rDiscr	The "Discretized Connectivity Matrix" (delivered by "Classify" -- Classes = c(0,1) only)
#'@param  Ref		The "Reference Matrix"
#'@param  Title		Title of the graph.
#'

#'
#' @return 			A message if an error is detected and returns NULL otherwise.
#'

CheckInputDataDD	<- function (rDiscr, Ref, Title) {
  tryCatch (
	expr = {
		# Test input data : rDiscr is delivered by "Classify" -- Classes = c(0,1) only
		Cl		<- c(0, 1)						# Authorized classes
		if (! all(rDiscr$Input$InputPar$Classes == Cl))	
			return ("rDiscr must be returned by 'Classify', Classes = c(0,1) !")
		nbN		<- dim(rDiscr$rDig)[1]		# Number of nodes

		if ((! is.matrix(Ref)) | (dim(Ref)[1] != dim(Ref)[2]))
			return ("Ref must be a squarematrix !")
		if (! all(Ref %in% Cl))
			return ("Ref must contain 0 and 1 only !")
		if (dim(Ref)[1] != nbN)
			return ("Ref must have as many rows and columns as the number of nodes !")

		# Test 'Title'
		if (! is.null(Title) && ! is.character(Title))
			return ("Title must be NULL or a string !")

		return (NULL)			# No error detected
	},		# expr
	
	warning = function (e) {
		message ("DrawDiscr : check input parameters : Warning detected !")
		print(e)	
		return ("Warning")
	},		# warning
	
	error = function (e) {
		message ("DrawGDiscr : check input parameters : Error detected !")
		print(e)
	}		# error
  )			# tryCatch
  
  return ("Error")
}		# CheckInputDataDG


		
# CYTOSCAPE : Run getVisualPropertyNames() to retrieve property names

###		  [1] "COMPOUND_NODE_PADDING"              "COMPOUND_NODE_SHAPE"               
###		  [3] "DING_RENDERING_ENGINE_ROOT"         "EDGE"                              
###		  [5] "EDGE_BEND"                          "EDGE_CURVED"                       
###		  [7] "EDGE_LABEL"                         "EDGE_LABEL_AUTOROTATE"             
###		  [9] "EDGE_LABEL_BACKGROUND_COLOR"        "EDGE_LABEL_BACKGROUND_SHAPE"       
###		 [11] "EDGE_LABEL_BACKGROUND_TRANSPARENCY" "EDGE_LABEL_COLOR"                  
###		 [13] "EDGE_LABEL_FONT_FACE"               "EDGE_LABEL_FONT_SIZE"              
###		 [15] "EDGE_LABEL_POSITION"                "EDGE_LABEL_ROTATION"               
###		 [17] "EDGE_LABEL_TRANSPARENCY"            "EDGE_LABEL_WIDTH"                  
###		 [19] "EDGE_LINE_TYPE"                     "EDGE_PAINT"                        
###		 [21] "EDGE_SELECTED"                      "EDGE_SELECTED_PAINT"               
###		 [23] "EDGE_SOURCE_ARROW_SELECTED_PAINT"   "EDGE_SOURCE_ARROW_SHAPE"           
###		 [25] "EDGE_SOURCE_ARROW_SIZE"             "EDGE_SOURCE_ARROW_UNSELECTED_PAINT"
###		 [27] "EDGE_STACKING"                      "EDGE_STACKING_DENSITY"             
###		 [29] "EDGE_STROKE_SELECTED_PAINT"         "EDGE_STROKE_UNSELECTED_PAINT"      
###		 [31] "EDGE_TARGET_ARROW_SELECTED_PAINT"   "EDGE_TARGET_ARROW_SHAPE"           
###		 [33] "EDGE_TARGET_ARROW_SIZE"             "EDGE_TARGET_ARROW_UNSELECTED_PAINT"
###		 [35] "EDGE_TOOLTIP"                       "EDGE_TRANSPARENCY"                 
###		 [37] "EDGE_UNSELECTED_PAINT"              "EDGE_VISIBLE"                      
###		 [39] "EDGE_WIDTH"                         "EDGE_Z_ORDER"                      
###		 [41] "NETWORK"                            "NETWORK_ANNOTATION_SELECTION"      
###		 [43] "NETWORK_BACKGROUND_PAINT"           "NETWORK_CENTER_X_LOCATION"         
###		 [45] "NETWORK_CENTER_Y_LOCATION"          "NETWORK_CENTER_Z_LOCATION"         
###		 [47] "NETWORK_DEPTH"                      "NETWORK_EDGE_SELECTION"            
###		 [49] "NETWORK_FORCE_HIGH_DETAIL"          "NETWORK_HEIGHT"                    
###		 [51] "NETWORK_NODE_LABEL_SELECTION"       "NETWORK_NODE_SELECTION"            
###		 [53] "NETWORK_SCALE_FACTOR"               "NETWORK_SIZE"                      
###		 [55] "NETWORK_TITLE"                      "NETWORK_WIDTH"                     
###		 [57] "NODE"                               "NODE_BORDER_PAINT"                 
###		 [59] "NODE_BORDER_STROKE"                 "NODE_BORDER_TRANSPARENCY"          
###		 [61] "NODE_BORDER_WIDTH"                  "NODE_CUSTOMGRAPHICS_1"             
###		 [63] "NODE_CUSTOMGRAPHICS_2"              "NODE_CUSTOMGRAPHICS_3"             
###		 [65] "NODE_CUSTOMGRAPHICS_4"              "NODE_CUSTOMGRAPHICS_5"             
###		 [67] "NODE_CUSTOMGRAPHICS_6"              "NODE_CUSTOMGRAPHICS_7"             
###		 [69] "NODE_CUSTOMGRAPHICS_8"              "NODE_CUSTOMGRAPHICS_9"             
###		 [71] "NODE_CUSTOMGRAPHICS_POSITION_1"     "NODE_CUSTOMGRAPHICS_POSITION_2"    
###		 [73] "NODE_CUSTOMGRAPHICS_POSITION_3"     "NODE_CUSTOMGRAPHICS_POSITION_4"    
###		 [75] "NODE_CUSTOMGRAPHICS_POSITION_5"     "NODE_CUSTOMGRAPHICS_POSITION_6"    
###		 [77] "NODE_CUSTOMGRAPHICS_POSITION_7"     "NODE_CUSTOMGRAPHICS_POSITION_8"    
###		 [79] "NODE_CUSTOMGRAPHICS_POSITION_9"     "NODE_CUSTOMGRAPHICS_SIZE_1"        
###		 [81] "NODE_CUSTOMGRAPHICS_SIZE_2"         "NODE_CUSTOMGRAPHICS_SIZE_3"        
###		 [83] "NODE_CUSTOMGRAPHICS_SIZE_4"         "NODE_CUSTOMGRAPHICS_SIZE_5"        
###		 [85] "NODE_CUSTOMGRAPHICS_SIZE_6"         "NODE_CUSTOMGRAPHICS_SIZE_7"        
###		 [87] "NODE_CUSTOMGRAPHICS_SIZE_8"         "NODE_CUSTOMGRAPHICS_SIZE_9"        
###		 [89] "NODE_CUSTOMPAINT_1"                 "NODE_CUSTOMPAINT_2"                
###		 [91] "NODE_CUSTOMPAINT_3"                 "NODE_CUSTOMPAINT_4"                
###		 [93] "NODE_CUSTOMPAINT_5"                 "NODE_CUSTOMPAINT_6"                
###		 [95] "NODE_CUSTOMPAINT_7"                 "NODE_CUSTOMPAINT_8"                
###		 [97] "NODE_CUSTOMPAINT_9"                 "NODE_DEPTH"                        
###		 [99] "NODE_FILL_COLOR"                    "NODE_HEIGHT"                       
###		[101] "NODE_LABEL"                         "NODE_LABEL_BACKGROUND_COLOR"       
###		[103] "NODE_LABEL_BACKGROUND_SHAPE"        "NODE_LABEL_BACKGROUND_TRANSPARENCY"
###		[105] "NODE_LABEL_COLOR"                   "NODE_LABEL_FONT_FACE"              
###		[107] "NODE_LABEL_FONT_SIZE"               "NODE_LABEL_POSITION"               
###		[109] "NODE_LABEL_ROTATION"                "NODE_LABEL_TRANSPARENCY"           
###		[111] "NODE_LABEL_WIDTH"                   "NODE_NESTED_NETWORK_IMAGE_VISIBLE" 
###		[113] "NODE_PAINT"                         "NODE_SELECTED"                     
###		[115] "NODE_SELECTED_PAINT"                "NODE_SHAPE"                        
###		[117] "NODE_SIZE"                          "NODE_TOOLTIP"                      
###		[119] "NODE_TRANSPARENCY"                  "NODE_VISIBLE"
###		[121] "NODE_WIDTH"                         "NODE_X_LOCATION" 
###		[123] "NODE_Y_LOCATION"                    "NODE_Z_LOCATION" 
