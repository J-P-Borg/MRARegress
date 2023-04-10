#
#	MRARegress unit tests
#

#	PART ONE : check input data
#		(MatExp, Perturb, NodeNames, KnlgMap, Method , MapExper)

#	MatExp : matrix [nbRow1, nbCol1]
#		MatExp[i,j] >= 0
#		If Perturb is NULL, the number of perturbations must equal the number of nodes, so nbCol1 == nbRow1 +1

MatExp		<- matrix(c(0, 0, "A", 1.25, 1.54, 2.03, "Bravo", 0, 0.001), nrow=3)	
test_that("Input Data - Test 1", {
  expect_null (MRARegress (MatExp))										# Not a matrix of numbers
})

MatExp		<- matrix(c(0, 0, -1.32, 1.25, 1.54, 2.03, -0.025, 0, 0.001), nrow=3)
test_that("Input Data - Test 2", {
  expect_null (MRARegress (MatExp))										# Negative numbers
})

MatExp		<- matrix(c(0, 0, 1.32, 1.25, 1.54, 2.03, 0.025, 0, 0.001), nrow=3)
test_that("Input Data - Test 3", {
  expect_null (MRARegress (MatExp))										# Perturb is null and nbCol1 != nbRow1 +1
})


#	Perturb : vector [nbCol2]
#		One basal column at least.
#		nbCol2 == nbCol1
#		Name of nodes correspond to those of NodeName
#		If MapExper = NULL, a perturbation acts upon one node only
#		If MapExper = NULL, every node is perturbed by one perturbation at least

MatExp		<- matrix(c(0, 0, 1.32, 1.25, 1.54, 2.03, 0.025, 0, 0.001), nrow=3)
Perturb		<- c("Pert1", "Pert2", "Pert3")
test_that("Input Data - Test 4", {
  expect_null (MRARegress (MatExp, Perturb))							# No basal columns
})

Perturb		<- c("Pert1", "Pert2", "Pert3", "Base")
test_that("Input Data - Test 5", {
  expect_null (MRARegress (MatExp, Perturb))							# 3 perturbations instead of 2
})

MatExp		<- matrix(c(1, 1.1, 1.25, 0, 0, 1.32, 1.25, 1.54, 2.03, 0.025, 0, 0.001), nrow=3)
Perturb		<- c("Base", "Pert1-> Node1", "Pert2 ->  N2", "Pert3 -> N3")
test_that("Input Data - Test 6", {
  expect_null (MRARegress (MatExp, Perturb))							# Node1 doesn't belong to NodeName
})

Perturb		<- c("Base", "Pert1-> N1", "Pert2 ->  N2", "Pert1 -> N3")
test_that("Input Data - Test 7", {
  expect_null (MRARegress (MatExp, Perturb))							# Pert1 acts upon 2 nodes
})

Perturb		<- c("Base", "Pert1-> N1", "Pert2 ->  N2", "Pert3 -> N1")
test_that("Input Data - Test 8", {
  expect_null (MRARegress (MatExp, Perturb))							# No perturbation act upon N3
})


#	NodeNames : vector [nbRow3]
#		nbRow3 == nbRow1
#		Different nodes must have different names.

NodeName	<- c("G1", "G2", "G3", "G4")
test_that("Input Data - Test 9", {
  expect_null (MRARegress (MatExp, NodeName=NodeName))					# NodeName has 4 items, MatExp has 3 rows
})

NodeName	<- c("G1", "G2", "G2")
test_that("Input Data - Test 10", {
  expect_null (MRARegress (MatExp, NodeName=NodeName))					# Two nodes have the same name
})


#	KnlgMap : matrix [nbRow4, nbCol4]
#		nbRow4 == nbRow1  &&  nbCol4 == nbRow4
#		composed of {0, 1, -1, "x"}

KnlgMap		<- matrix ('x', nrow=4, ncol=4)
test_that("Input Data - Test 11", {
  expect_null (MRARegress (MatExp, Perturb, NodeNames, KnlgMap))		# KnlgMap has 4 rows, MatExp has 3 rows
})

KnlgMap		<- matrix (0, nrow=3, ncol=3)
KnlgMap[2,3]<- 2
test_that("Input Data - Test 12", {
  expect_null (MRARegress (MatExp, Perturb, NodeNames, KnlgMap))		# 2 is not allowed in KnlgMap
})


#	Method : a string in c("TLR", "LASSO", "RIDGE", "Elastic Net", "STEP", "ARACNE", "CLR", "RMNET", "Random Forest")

test_that("Input Data - Test 13", {
  expect_null (MRARegress (MatExp, Perturb, Method="Random Forrest"))	# Forrest instead of Forest (unknown method)
})


#	MapExper : matrix [nbRow5, nbCol5]
#		nbCol5 == NbP
#		Perturbations must be "partially proportional"

MatExp		<- matrix(c(0, 0, 1.32, 1.25, 1.54, 2.03, 0.025, 0, 0.001), nrow=3)
MapExper	<- matrix(c(1,1,1, 2,2,2, 3,3,3), nrow=3)
test_that("Input Data - Test 14", {
  expect_null (MRARegress (MatExp, MapExper=MapExper))					# 2 perturbations only (MatExp), but MapExper has 3 columns
})

MatExp		<- matrix(c(1, 1.1, 1.25, 0, 0, 1.32, 1.25, 1.54, 2.03, 0.025, 0, 0.001), nrow=3)
test_that("Input Data - Test 15", {
  expect_null (MRARegress (MatExp, MapExper=MapExper))					# 3 perturbations : not enough if MapExper != NULL (NbP >= (NbN-1+NbM))
})

MapExper	<- matrix(c(1,1,1, 2,0,2, 3,4,3), nrow=3)
test_that("Input Data - Test 16", {
  expect_null (MRARegress (MatExp, MapExper=MapExper))					# The 3° perturbation is not proportional to the reference
})


#	PART TWO : check results (see "My Document")
#

#	§ 7.2.1 : 3 perturbations acting on 3 nodes independantly. No a priori knowledge.

MatExp		<- matrix(c(1,2,3, 11,21,31, 12,22,32, 13,23,33),nrow=3)	# 3 nodes, 3 perturbations, Base : 1st column
MatrCc		<- matrix(0, nrow=3, ncol=3)

MatD0		<- MatrCc							# Contains Delta X (for verification)
for (iNode in 1:3) {
	MatD0[ ,iNode]	<- MatExp[ ,iNode+1] - MatExp[ ,1]
}

###		test_that("Functional test - Test 7.2.1", {
###		  expect_equal (MRARegress (MatExp) %*% MatD0, MatrCc, tolerance=1E-9)
###		})
