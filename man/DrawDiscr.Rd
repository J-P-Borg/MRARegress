% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DrawDiscr.R
\name{DrawDiscr}
\alias{DrawDiscr}
\title{"DrawDiscr" :   displays "rDiscr" and "Score" as a graph}
\usage{
DrawDiscr(rDiscr, Ref, Title = NULL)
}
\arguments{
\item{rDiscr}{Matrix of numbers        The "Discretized Connectivity Matrix" (delivered by "Classify" -- Classes = c(0,1) only)}

\item{Ref}{Matrix of numbers        The "Reference Matrix"}

\item{Title}{String                    Title of the graph. Default value is NULL.}
}
\value{
\if{html}{\out{<div class="sourceCode">}}\preformatted{List			NULL in case of error or a list of the input parameters values. For internal use only.
}\if{html}{\out{</div>}}
}
\description{
: this function represents the "Discretized Connectivity Matrix" (rDiscr) as a graph, if "Classes = c(0,1)" only, compared to a reference \cr
The nodes are the genes (for instance) and the edges represent the interaction between genes:\cr
Black solid lines correspond to "True Positive" edges (ie, an edge between A and B is discoverd by the method, and this edge exists in the "reference"),\cr
Red solid lines correspond to "False Positive" edges (ie, an edge between A and B is discoverd by the method, and this edge doesn't exist in the "reference"),\cr
Green solid lines correspond to "False Negative" edges (ie, an edge between A and B is not discoverd by the method, but this edge exists in the "reference").

Displays the "Discretized Connectivity Matrix" (rDiscr) as a graph, compared to a reference.
The input data are described above.
}
\details{
\if{html}{\out{<div class="sourceCode">}}\preformatted{Imported libraries :
- BiocManager (version 1.30.22). To install this version, R version >= 4.3 is mandatory
- RCy3							Communication with Cytoscape
- utils	compareVersion			To check Cytoscape version

Cytoscape (version >= 3.6.1) must be installed and running.
}\if{html}{\out{</div>}}

OUTPUT:

\if{html}{\out{<div class="sourceCode">}}\preformatted{theGraph	List of dataframes
			nodes (list of nodes : name, PValN), edges (list of edges : source, target, value, interaction, edgeType).
Variables	list of the input parameters values. NULL values are set to their default values
}\if{html}{\out{</div>}}
}
