#' Institut de Recherche en Cancérologie de Montpellier (IRCM)
#' Cancer Bioinformatics and Systems Biology
#'
#' A subset of data used by the unit tests of MRARegress
#'
#' @format ## `Pat3nds_O2`
#' A matrix with 3 rows and 5 columns : the "Order 2 Matrix" (coefficients corresponding to Taylor's development, order 2), computed with the perturbations (10%) 
#' corresponding to the experimental design described by P. Ravel in "Présentation_Patrice.pptx" :
#{ MapExper = matrix(c(1,0,0, 0,1,0, 0,0,1, -1,0,0, 0,-1,0, 0,0,-1, 0,1,1, 1,1,0, 1,0,1), nrow=3)
#' Three nodes network (pRaF, ppMEK, ppERK), described in the article "Impact of measurement noise, experimental design,
#' and estimation methods on Modular Response -- Analysis based network reconstruction" C. Thomaseth et al (Scientific Reports, 2018).
#'		Pat3nds_O2\[1,j\] : r1,2; r1,3; s1,2,2; s1,3,3; s1,2,3
#'		Pat3nds_O2\[2,j\] : r2,1; r2,3; s2,1,1; s2,3,3; s2,1,3
#'		Pat3nds_O2\[3,j\] : r3,1; r3,2; s3,1,1; s3,2,2; s3,1,2

"Pat3nds_O2"
