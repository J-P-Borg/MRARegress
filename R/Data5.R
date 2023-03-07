#' Institut de Recherche en Cancérologie de Montpellier (IRCM)
#' Cancer Bioinformatics and Systems Biology
#'
#' A subset of data used by the first example of the vignette
#'
#' @format ## `MapExper2`
#' A matrix with 10 rows and 20 columns (the "Experimental Map"):
#'		MapExper\[i,j\] = 1 means that the perturbation j perturbs the node i,
#'		MapExper\[i,j\] = 0 means that the perturbation j does not perturb the node i,
#'		MapExper\[i,j\] = x means that we do not know if the perturbation j perturbs the node i or not.

#'	As we have two sets of perturbation ("KO" and "KD"), we have twice as many columns than rows.

#' @source <"C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data-raw\\DATASET.R">
"MapExper2"
