#' Institut de Recherche en Cancérologie de Montpellier (IRCM)
#' Cancer Bioinformatics and Systems Biology
#'
#' A subset of data used by the unit tests of MRARegress
#'
#' @format ## `MatExp`
#' A matrix with 10 (or 100 rows) and 21 (or 201) columns (the "Expression Matrix"), to generate unit tests aiming at testing the different resolution methods.
#' Dream Challenge 4, size=10 (or 100), Fic.1 .. 5  ===   The network has 10 (or 100) nodes, perturbations : KD50 (Knock Down 50%), KO (Knock Out)
#'		MatExp\[i,1\] 	   = measurement of node i, without any perturbation ("basal" values).

#'		MatExp\[i,2:11\]  = measurement of node i, perturbed by "Perturbation 1" (col. 2),  ..., "Perturbation 10" (col. 11). Perturbation = KD50
#'		MatExp\[i,12:21\] = measurement of node i, perturbed by "Perturbation 1" (col. 12), ..., "Perturbation 10" (col. 21). Perturbation = KO     OR

#'		MatExp\[i,2:101\]   = measurement of node i, perturbed by "Perturbation 1" (col. 2),  ..., "Perturbation 100" (col. 101). Perturbation = KD50
#'		MatExp\[i,102:201\] = measurement of node i, perturbed by "Perturbation 1" (col. 102), ..., "Perturbation 100" (col. 201). Perturbation = KO 

"MatExp"
