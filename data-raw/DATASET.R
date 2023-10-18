## code to prepare `DATASET` dataset goes here

#	load() function reloads datasets written with the function save
#	save(data, file = "data.Rdata")				Creates a .rda file

usethis::use_data(DATASET, overwrite = TRUE)

NodeNames <- c("MKKK", "MKKK-PP", "MKK", "MKK-PP", "MAPK", "MAPK-PP")
usethis::use_data(NodeNames)					# Generates the file "NodeNames.rda" in ..\Data
												# To update an existing file, add ",overwrite=TRUE"
												
load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\MatExp1.rda")
usethis::use_data(MatExp1)

load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\Solution_10_1.rda")
usethis::use_data(Solution)

load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\MatExp2.rda")
usethis::use_data(MatExp2)

load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\Perturb2.rda")
usethis::use_data(Perturb2)
