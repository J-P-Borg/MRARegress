#' Institut de Recherche en Cancérologie de Montpellier (IRCM)
#' Cancer Bioinformatics and Systems Biology
#'
#' A subset of data used by the unit tests of MRARegress
#'
#' @format ## `Pat3nds_O1`
#' A matrix with 3 rows and 3 columns : the "Exact Connectivity Matrix".
#' Three nodes network (pRaF, ppMEK, ppERK), described in the article "Impact of measurement noise, experimental design,
#' and estimation methods on Modular Response -- Analysis based network reconstruction" C. Thomaseth et al (Scientific Reports, 2018).
#'		Pat3nds_O1\[i,j\] = the exact coefficient ri,j, computed with the perturbations (10%) corresponding to the experimental design described 
#' by P. Ravel in "Présentation_Patrice.pptx" :
#{ MapExper = matrix(c(1,0,0, 0,1,0, 0,0,1, -1,0,0, 0,-1,0, 0,0,-1, 0,1,1, 1,1,0, 1,0,1), nrow=3)

"Pat3nds_O1"