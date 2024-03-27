#' Institut de Recherche en Cancérologie de Montpellier (IRCM)
#' Cancer Bioinformatics and Systems Biology
#'
#' A subset of data used by the unit tests of MRARegress
#'
#' @format ## `FileNbr`

#' A matrix with 15 rows and 7 columns : Data used to generate FRANK files.
#  The original data delivered by FRANK Network Generator are archived, to be used y tests. Files are named ""Frankxxxxnonmodified_network.csv",
#  where xxxx are :
#	  11,  649, 8363, 5883, 6798	for TF=30,   TA=0, (z = 1,2 ..5 and Seed = Seeds[z]) and so on...
#  Column name	: TF 	TA    1 	 2 	   3 	 4 	   5
#  First row	: TF	TA 	12345 72090 87577 45648 16637		(gives the columns' name)
#  Following rows: TF value, TA value,  File nbrs (11, 649, 8363 etc ...)
#  Example 		: 30   0    11   649  8363  5883  6798
#  {TF (number of nodes affecting others), TA (number of nodes acting on no other ones)} = 
#  {(30,0), (60,0), (100,0), (200,0), (300,0), (500,0), (800,0), (1000,0), (30,30), (50,50), (100,100), (150,150), (250,250), (400,400), (500,500)}
#
#' @source <"C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\Frank_FileNbr.rda">
"FileNbr"
