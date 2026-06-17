#
#	DrawGraphML unit tests
#

#	PART ONE : check input data

MatExp		<- matrix(c(1,2,3, 11,21,31, 12,22,32, 13,23,33),nrow=3)	# 3 nodes, 3 perturbations, Base : 1st column
Ret			<- MRARegress (MatExp, Relative=FALSE)

Ret1		<- Ret
Ret1$Input$Variables	<- NULL
test_that("Incorrect Ret -1", {
  expect_null (DrawGraphML(Ret1))									# File name missing
})

test_that("Incorrect Ret -2", {
  expect_null (DrawGraphML(Ret1, "..\\Temporaire\\DGML_TU2"))		# Data missing in the list
})

Ret2		<- Ret
KeysR		<- Ret$Input$Variables$Keys								# Keys read
KeysR[3]	<- KeysR[3] +1	
Ret2$Input$Variables$Keys	<- KeysR
test_that("Incorrect Ret -3", {
  expect_null (DrawGraphML(Ret2, "..\\Temporaire\\DGML_TU2"))		# Bad signature
})

Vect		<- c(1, 2, 3, 4)
test_that("Ret vector -4", {
  expect_null (DrawGraphML(Vect, "..\\Temporaire\\DGML_TU2"))		# 'Ret' is neither a list nor a matrix
})

test_that("Ret square matrix -5", {
  expect_null (DrawGraphML(MatExp, "..\\Temporaire\\DGML_TU2"))		# 'Ret' is neither a list nor a square matrix
})

MatExp0		<- MatExp[ ,-1]
MatExp0[2,1]<- 'A'
test_that("Ret non numeric -6", {
  expect_null (DrawGraphML(MatExp0, "..\\Temporaire\\DGML_TU2"))	# 'Ret' is neither a list nor a matrix of numbers
})

test_that("Incorrect title -7", {
  expect_null (DrawGraphML(Ret, File=12))							# File must be a string
})

test_that("Incorrect threshold -8", {
  expect_null (DrawGraphML(Ret, "..\\Temporaire\\DGML_TU2", Thr='abcd'))	# Thr must be a number
})

test_that("Incorrect threshold -9", {
  expect_null (DrawGraphML(Ret, "..\\Temporaire\\DGML_TU2", Thr=0))			# Thr must be a strictly positive number
})


#
#	Examples of networks
#
Path	<- "C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\Temporaire\\"

#	1/ 3 perturbations acting on 3 nodes independantly. No a priori knowledge (§ 7.2.1)
#
fileExpected	<- paste (Path, "DGML_TU2.graphml", sep="")
if (file.exists(fileExpected))
	file.remove(fileExpected)
	
MatExp	<- matrix(c(1,2,3, 11,21,31, 12,22,32, 13,23,33),nrow=3)				# 3 nodes, 3 perturbations, Base : 1st column
Ret		<- MRARegress (MatExp, Relative=FALSE)
fileExpected	<- paste (Path, "DGML_TU2", sep="")
RetDG	<- DrawGraphML (Ret, fileExpected)										# File name without file extension
fileExpected	<- paste (Path, "DGML_TU2.graphml", sep="")
test_that("Simple network", {
  expect_equal (RetDG$XML, fileExpected, tolerance=1E-4)						# 3 nodes (DrawGraph did well)
})

#	2/ 6 MAP Kinases
#
fileExpected	<- paste (Path, "6MapKinases.graphml", sep="")
if (file.exists(fileExpected))
	file.remove(fileExpected)
Names	<- c("MKKK (1)", "MKKK-PP (2)", "MKK (3)", "MKK-PP (4)", "MAPK (5)", "MAPK-PP (6)")
Ret		<- MRARegress (MatExp1, NodeName=Names)
RetDG	<- DrawGraphML (Ret,fileExpected)
test_that("6 MAP kinases", {
  expect_equal (RetDG$Variables[[2]], 6, tolerance=1E-4)						# 6 nodes (DrawGraphML did well)
})

#	3/ Second example (Vignette) : Dream Challenge 4 (InSilico-10-1 network)  (article fig 4A)
#
fileExpected	<- paste (Path, "InSilico-10-1.graphml", sep="")
if (file.exists(fileExpected))
	file.remove(fileExpected)
Ret		<- MRARegress (MatExp2, Perturb2)
Matr	<- Ret$r
diag(Matr)	<- 0
Th		<- 0.25 * max(abs(Matr))
RetDG	<- DrawGraphML (Ret, fileExpected, Thr=Th)
test_that("InSilico_10_1", {
  expect_equal (length(RetDG$theGraph[[2]]$value), 18, tolerance=1E-4)			# 18 edges (DrawGraphML did well)
})

#	4/ Use a matrix instead of a MRARegress output
#
fileExpected	<- paste (Path, "Solution_InSilico-10-1.graphml", sep="")
if (file.exists(fileExpected))
	file.remove(fileExpected)
data (Solution_10_1)				# InSilico_10_1
RetDG	<- DrawGraphML (Solution, fileExpected)
test_that("Solution :InSilico-10-1", {
  expect_equal (RetDG$Variables[[2]], 10, tolerance=1E-4)						# 10 nodes (DrawGraphML did well)
})
