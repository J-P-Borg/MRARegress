#' Institut de Recherche en Cancérologie de Montpellier (IRCM)
#' Cancer Bioinformatics and Systems Biology
#'
#' A subset of data used by the second example of the vignette
#'
#' @format ## `MatExp2`
#' A matrix with 10 rows and 21 columns (the "Expression Matrix"):
#'		MatExp\[i,1..nbBase\] represents the measurement of node i when no perturbation is applied to the network (here, nbBase = 1),
#'		MatExp\[i,j\], j>nbBase,  represents the measurement of node i when perturbation j-nbBase is applied to the network.

#'	As we have two sets of perturbation ("KO" and "KD"), we have twice as many columns (plus one: nbBase = 1) than rows.

#' @source <"C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data-raw\\DATASET.R">
"MatExp2"
