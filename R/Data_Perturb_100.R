#' Institut de Recherche en Cancérologie de Montpellier (IRCM)
#' Cancer Bioinformatics and Systems Biology
#'
#' A subset of data used by the unit tests of MRARegress
#'
#' @format ## `Perturb_100`
#' A vector with and 201 (values (the "Perturbation Vector"), to generate unit tests aiming at testing the different resolution methods.
#' Dream Challenge 4, size=100   ===   The network has 100 nodes, perturbations : KD50 (Knock Down 50%), KO (Knock Out)
#' This vector identifies the "basal" and "perturbations" columns. It designates the nodes on which the perturbations act. A specific syntax is mandatory (see doc.)
#'		Perturb_10\[1\]   	   = "Base"		===  First column ("basal" values).
#'		Perturb_100\[2:101\]   = "Qi -> Ni"	===  Perturbation Qi acts on node Ni (KD50)
#'		Perturb_100\[102:201\] = "Qi -> Ni"	===  Perturbation Qi acts on node Ni (KO)

"Perturb_100"
