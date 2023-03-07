## code to prepare `DATASET` dataset goes here

usethis::use_data(DATASET, overwrite = TRUE)

MapExper	<- array(0, dim=c(6,6))
diag(MapExper)	<- 1
usethis::use_data(MapExper)

M	<- c(131.46478, 46.84160, 55.95284, 103.05500, 139.58334, 28.40970,
		 131.46141, 46.84368, 55.94982, 103.05826, 139.57978, 28.41158,
		 131.46137, 46.84549, 55.94720, 103.06109, 139.57669, 28.41321,
		 131.46543, 46.84104, 55.95083, 103.05622, 139.58201, 28.41040,
		 131.46638, 46.84020, 55.95091, 103.05804, 139.58003, 28.41145,
		 131.46539, 46.84107, 55.95584, 103.05178, 139.57787, 28.41037,
		 131.46560, 46.84088, 55.95690, 103.05064, 139.58585, 28.41060)
MatExp	<- array(M, dim=c(6,7))	 
usethis::use_data(MatExp)

NodeNames <- c("MKKK", "MKKK-PP", "MKK", "MKK-PP", "MAPK", "MAPK-PP")
usethis::use_data(NodeNames)


load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data-raw\\Solution")
usethis::use_data(Solution)

load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data-raw\\MatExp2")
usethis::use_data(MatExp2)

Map1		<- array(0, dim=c(10,10))
diag(Map1)	<- 1
Map2		<- array(0, dim=c(10,10))
diag(Map2)	<- 1
MapExper2	<- cbind(Map1,Map2)
usethis::use_data(MapExper2)
