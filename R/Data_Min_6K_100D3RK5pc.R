#' Institut de Recherche en Cancérologie de Montpellier (IRCM)
#' Cancer Bioinformatics and Systems Biology
#'
#' A subset of data used by the unit tests of rCI
#'
#' @format ## 'Min_6K_100D3RK5pc'
#' A matrix with 6 rows and 6 columns : the min of the "95% Confidence Intervals" of the "Exact Connectivity Matrix".
#' Six nodes network "Kinases MKK, MKKK et MAPK + bi-phosphorylated forms", described in the articles "Inferring dynamic architecture of cellular 
#' networks using time series of gene expression, protein and metabolite data" Eduardo Sontag et al, 2004 and "Untangling the wires: 
#' A strategy to trace functional interactions in signaling and gene networks" Boris N. Kholodenko et al, PNAS 2002 
#'		Solution_6K\[i,j\] = the exact coefficient ri,j.
#'
#'	These Confidence intervals are obtained by making 100 random draws (100D in the name), adding gaussian independant variables N(0, sd) to the expression matrix values
#	sd = 0.05*mean value (5pc in the name) of the non perturbed expression values (see doc.). 3 replicates (3R in the name) are used for each measurement.

"Min_6K_100D3RK5pc"