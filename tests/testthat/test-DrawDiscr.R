#
#	DrawDiscrunit tests
#

#	Check the connection to Cytoscape by hand :
#	Stop Cytoscape execution, then try one of the following tests. An error must occur.
#	Don't forget to start Cytoscape again !

#	PART ONE : check input data

vRoot		<- "C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRAregress/data/"
Fics		<- c("10_1", "10_2", "10_3", "10_4", "10_5")			# Files to study
nbFics		<- length(Fics)

load (paste(vRoot, "Perturb_10.rda", sep=""))						# Perturb_10
load (paste(vRoot, "MatExp_10_1.rda", sep=""))						# MatExp
load (paste(vRoot, "Solution_10_1.rda", sep=""))					# Solution

Ret		<- MRARegress (MatExp, Perturb_10)

test_that("Classify Return-1", {
  print("DrawDiscr -- Classify Return-1")
  expect_null (DrawDiscr(MatExp, Solution))							# rDiscr must be returned by 'Classify'
})

test_that("Classify Return-2", {
  expect_null (DrawDiscr(Ret, Solution))							# rDiscr must be returned by 'Classify'
})

RetC1	<- Classify (Ret, Classes=c(-1,0,1), MethDiscr="Threshold", Lbda=c(-0.25,0.25))

test_that("Classify Classes", {
  expect_null (DrawDiscr(RetC1, Solution))							# rDiscr must be returned by 'Classify', with Classes = c(0,1)
})

Solution1	<- cbind (Solution, rep.int(1, 10))
RetC	<- Classify (Ret)

test_that("Ref square matrix", {
  expect_null (DrawDiscr(RetC, Solution1))							# rDiscr must be a squarematrix
})

Solution1	<- Solution
Solution1[2,5]	<- 2
test_that("Refcontains 0, 1", {
  expect_null (DrawDiscr(RetC, Solution1))							# Ref must contain 0 and 1 only
})

Solution1	<-	Solution1[-2, ]
Solution1	<-	Solution1[ ,-5]
test_that("Refcontains 0, 1", {
  expect_null (DrawDiscr(RetC, Solution1))							# Ref must have as many rows and columns as the number of nodes
})

Title = 123
test_that("Refcontains 0, 1", {
  expect_null (DrawDiscr(RetC, Solution, Title))					# Title must be NULL or a string
})

#	PART TWO	: normal operation

for (iFic in 1:nbFics) {
	load (paste(vRoot, "MatExp_",     Fics[iFic], ".rda", sep=""))		#   MatExp
	load (paste(vRoot, "Solution_",   Fics[iFic], ".rda", sep=""))		#   Solution
	Ret		<- MRARegress (MatExp, Perturb_10, NoPrint=TRUE)
	RetC 	<- Classify(Ret, NoPrint=TRUE)
	
	Titre	<- paste("Essai ", Fics[iFic], sep="")
	TestName	<- paste("Normal Oper : ", Fics[iFic], sep="")
	A		<- DrawDiscr (RetC, Solution, Title=Titre)
	#	cat("Fics[iFic] ", Fics[iFic], " Target : ", A$theGraph[[2]]$target[1])
	
	test_that(TestName, {
		expect_equal (A$theGraph[[2]]$target[1], "N2")
	})
}

