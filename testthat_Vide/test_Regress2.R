#
#	Regress2 unit tests
#

#	PART ONE : check input data
#		(MatExp, Perturb, NodeNames, KnlgMap, Method , MapExper)
#		Check that MatExp, KnlgMap, MapExper are matrices and Perturb, NodeNames are vectors
Vect	<- c(1, 2, 3, 4)
MatExp		<- matrix(c(1, 1.1, 1.25, 0, 0, 1.32, 1.25, 1.54, 2.03, 0.025, 0, 0.001), nrow=3)
MatrArr	<- array(c(0, 0, 1.32, 1.25, 1.54, 2.03, 0.025, 0), dim=c(2,2,2))

cat ("MRARgress MatExp is a matrix -1 \n")
test_that("MatExp is a matrix -1", {
  expect_null (Regress2 (Vect)$r)										# Not a matrix
})

test_that("MatExp is a matrix -2", {
  expect_null (Regress2 (MatrArr)$r)									# Not a matrix
})

test_that("KnlgMap is null or a matrix -1", {
  expect_null (Regress2 (MatExp, KnlgMap=Vect)$r)						# Not a matrix
})

test_that("KnlgMap is null or a matrix -2", {
  expect_null (Regress2 (MatExp, KnlgMap=MatrArr)$r)					# Not a matrix
})

test_that("MapExper is null or a matrix -1", {
  expect_null (Regress2 (MatExp, MapExper=Vect)$r)					# Not a matrix
})

test_that("MapExper is null or a matrix -2", {
  expect_null (Regress2 (MatExp, MapExper=MatrArr)$r)					# Not a matrix
})

test_that("Perturb is null or a vector -1", {
  expect_null (Regress2 (MatExp, Perturb=MatExp)$r)					# Not a vector
})

test_that("Perturb is null or a vector -2", {
  expect_null (Regress2 (MatExp, Perturb=MatrArr)$r)					# Not a vector
})

test_that("NodeName is null or a vector -1", {
  expect_null (Regress2 (MatExp, NodeName=MatExp)$r)					# Not a vector
})

test_that("NodeName is null or a vector -2", {
  expect_null (Regress2 (MatExp, NodeName=MatrArr)$r)					# Not a vector
})

test_that("Relative is logical", {
  expect_null (Regress2 (MatExp, Relative="faux")$r)					# Not a logical (TRUE, FALSE)
})

test_that("Verbose is logical", {
  expect_null (Regress2 (MatExp, Verbose="faux")$r)					# Not a logical (TRUE, FALSE)
})

test_that("NoPrint is logical", {
  expect_null (Regress2 (MatExp, NoPrint="faux")$r)					# Not a logical (TRUE, FALSE)
})


#	PART TWO : check input data
#		(MatExp, Perturb, NodeNames, KnlgMap, Method , MapExper)
#		Check specific requirements of these objects

#	MatExp : matrix [nbRow1, nbCol1]
#		MatExp[i,j] >= 0
#		If Perturb is NULL, the number of perturbations must equal the number of nodes, so nbCol1 == nbRow1 +1

MatExp		<- matrix(c(0, 0, "A", 1.25, 1.54, 2.03, "Bravo", 0, 0.001), nrow=3)	
test_that("Input Data - Test 1", {
  expect_null (Regress2 (MatExp)$r)									# Not a matrix of numbers
})

MatExp		<- matrix(c(0, 0, 1.32, 1.25, 1.54, 2.03, 0.025, 0, 0.001), nrow=3)
test_that("Input Data - Test 2", {
  expect_null (Regress2 (MatExp)$r)									# Perturb is null and nbCol1 != nbRow1 +1
})