#' Institut de Recherche en Cancérologie de Montpellier (IRCM)
#' Cancer Bioinformatics and Systems Biology
#'
#' A subset of data used by the unit tests of MRARegress
#'
#' @format ## `Matr_K10`
#' A matrix with 30 rows and 30 columns : Connectivity Matrix got from FRANK (TF=300, TA=0, file 1, 10% noise).
#' Here, we have a priori knowledge : about 10% of the edges are known (ie : node j amplifies or inhibits the expression of node i, 
#' or nodes i and j don't interact). This knowledge is used by MRARegress, by introduction of convexity constraints with the Least Square optimization problem.
#' See the article "Modular response analysis reformulated as a multilinear regression problem" (Borg et al, 2023).
#'

#' @source <"C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\MatrCVXR_TF30_TA0_1_R1_10Knwn.rda">
"Matr_K10"
