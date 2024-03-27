#
#	DrawGraph unit tests
#

#	Check the connection to Cytoscape by hand :
#	Stop Cytoscape execution, then try one of the following tests. An error must occur.
#	Don't forget to start Cytoscape again !

#	PART ONE : check input data

MatExp		<- matrix(c(1,2,3, 11,21,31, 12,22,32, 13,23,33),nrow=3)	# 3 nodes, 3 perturbations, Base : 1st column
Ret			<- MRARegress (MatExp, Relative=FALSE)

Ret1		<- Ret
Ret1$Input$Variables	<- NULL
cat ("DrawGraph Incorrect Ret -1 \n")
test_that("Incorrect Ret -1", {
  expect_null (DrawGraph(Ret1))										# Data missing in the list
})

Ret2		<- Ret
KeysR		<- Ret$Input$Variables$Keys								# Keys read
KeysR[3]	<- KeysR[3] +1	
Ret2$Input$Variables$Keys	<- KeysR
test_that("Incorrect Ret -2", {
  expect_null (DrawGraph(Ret2))										# Bad signature
})

Vect		<- c(1, 2, 3, 4)
test_that("Ret vector -3", {
  expect_null (DrawGraph(Vect))										# 'Ret' is neither a list nor a matrix
})

test_that("Ret square matrix -4", {
  expect_null (DrawGraph(MatExp))									# 'Ret' is neither a list nor a square matrix
})

MatExp0		<- MatExp[ ,-1]
MatExp0[2,1]<- 'A'
test_that("Ret non numeric -5", {
  expect_null (DrawGraph(MatExp0))									# 'Ret' is neither a list nor a matrix of numbers
})

test_that("Incorrect title -6", {
  expect_null (DrawGraph(Ret, Title=12))							# Title must be a string
})

test_that("Incorrect threshold -7", {
  expect_null (DrawGraph(Ret, Thr='abcd'))							# Thr must be a number
})

test_that("Incorrect threshold -8", {
  expect_null (DrawGraph(Ret, Thr=0))								# Thr must be a strictly positive number
})


#
#	PART TWO : Examples of networks
#
#	1/ 3 perturbations acting on 3 nodes independantly. No a priori knowledge (§ 7.2.1)
#
MatExp	<- matrix(c(1,2,3, 11,21,31, 12,22,32, 13,23,33),nrow=3)	# 3 nodes, 3 perturbations, Base : 1st column
Ret		<- MRARegress (MatExp, Relative=FALSE)
RetDG	<- DrawGraph (Ret)
test_that("Simple network", {
  expect_equal (RetDG$Variables[[2]], 3, tolerance=1E-4)			# 3 nodes (DrawGraph did well)
})

#	2/ First example (Vignette) : 6 MAP Kinase network	(article fig 1)
#
Names	<- c("MKKK (1)", "MKKK-PP (2)", "MKK (3)", "MKK-PP (4)", "MAPK (5)", "MAPK-PP (6)")
Ret		<- MRARegress (MatExp1, NodeName=Names)
RetDG	<- DrawGraph (Ret, Title="6 MAP kinases")
test_that("6 MAP kinases", {
  expect_equal (RetDG$Variables[[2]], 6, tolerance=1E-4)			# 6 nodes (DrawGraph did well)
})

#	3/ Second example (Vignette) : Dream Challenge 4 (InSilico-10-1 network)  (article fig 4A)
#
data(MatExp2)
data(Perturb2)
Ret		<- MRARegress (MatExp2, Perturb2)
Matr	<- Ret$r
diag(Matr)	<- 0
Th		<- 0.25 * max(abs(Matr))
RetDG	<- DrawGraph (Ret, Title="InSilico-10-1", Thr=Th)
test_that("InSilico_10_1", {
  expect_equal (length(RetDG$theGraph[[2]]$value), 18, tolerance=1E-4)			# 18 edges (DrawGraph did well)
})

#	4/ Use a matrix instead of a MRARegress output
#
data (Solution_10_1)				# InSilico_10_1
RetDG	<- DrawGraph (Solution, Title="Solution :InSilico-10-1")
test_that("Solution :InSilico-10-1", {
  expect_equal (RetDG$Variables[[2]], 10, tolerance=1E-4)			# 10 nodes (DrawGraph did well)
})

#	5/ Gabriel Jimenez network
#
load ("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRARegress\\data-raw\\Jimenez_Fig3.rda")		# data
Nodes <- c('LCOR', 'RIP140', 'Hoxa5', 'Luciferase')
Pert  <- c('Base', 'E2+RA+siLCoR->LCOR', 'E2+RA+siRIP140->RIP140', 'E2->Hoxa5', 'RA->Luciferase')		# Base = 'E2+RA'
Data1 <- data$mean[c('LCoR','RIP140','Hoxa5','Luciferase'), ]
MatExp_Jim 	<- Data1[ ,c('E2+RA', 'E2+RA+siLCoR', 'E2+RA+siRIP140', 'E2', 'RA')]
Res	  <- MRARegress(MatExp_Jim, Pert, Nodes)
RetDG	<- DrawGraph (Res, "aiMeRA User's Guide", 0.01)
test_that("aiMeRA", {
  expect_equal (length(RetDG$theGraph[[2]]$value), 12, tolerance=1E-4)			# 18 edges (DrawGraph did well)
})
