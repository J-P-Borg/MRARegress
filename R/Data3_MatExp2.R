#' Institut de Recherche en Cancérologie de Montpellier (IRCM)
#' Cancer Bioinformatics and Systems Biology
#'
#' A subset of data used by the second example of the vignette
#'
#' @format ## `MatExp2`
#' A matrix with 10 rows and 21 columns (the "Expression Matrix"):
#'		From Dream Challenge 4 (InSilico_10_1 : "basal" values + 2 perturbations : Knock Down and Knock Out).
#'		MatExp\[i,1\] represents the measurement of node i when no perturbation is applied to the network (here, as input matrix MapExer is NULL, there is one basal column, the first one.
#'		MatExp\[i,j\], j>1,  represents the measurement of node i when perturbation j-1 is applied to the network.

#'	As we have two sets of perturbation ("KO" and "KD"), we have twice as many columns (plus one: the "basal" column) than rows.

#' @source <"C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data-raw\\DATASET.R">
"MatExp2"
