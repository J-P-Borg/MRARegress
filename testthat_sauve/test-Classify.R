#
#	Classify unit tests
#

#	PART ONE : check input data

MatExp		<- matrix(c(1,2,3, 11,21,31, 12,22,32, 13,23,33),nrow=3)	# 3 nodes, 3 perturbations, Base : 1st column
Ret			<- MRARegress (MatExp, Relative=FALSE)

cat ("Classify Incorrect Ret -1 \n")
Ret1		<- Ret
Ret1$Input$Variables	<- NULL
test_that("Incorrect Ret -1", {
  expect_null (Classify(Ret1))										# Data missing in the list
})

Ret2		<- Ret
KeysR		<- Ret$Input$Variables$Keys								# Keys read
KeysR[3]	<- KeysR[3] +1	
Ret2$Input$Variables$Keys	<- KeysR
test_that("Incorrect Ret -2", {
  expect_null (Classify(Ret2))										# Bad signature
})

MatExp0		<- MatExp[ ,-1]
MatExp0[2,1]<- 'A'
test_that("Ret non numeric -3", {
  expect_null (Classify(MatExp0))									# 'Ret' is neither a list nor a matrix of numbers
})

test_that("Classes non numeric -4", {
  expect_null (Classify(MatExp, Classes=c(0,1,"a")))				# Classes must contain numbers only
})

test_that("One Classe -5", {
  expect_null (Classify(MatExp, Classes=1))							# Two classes at least
})

test_that("Not increasing order -6", {
  expect_null (Classify(MatExp, Classes=c(0,1,-1)))					# Classes not in increasing order
})

test_that("Error -7", {
  expect_null (Classify(MatExp, MethDiscr= '#' ))					# Error
})

test_that("MethDiscr not string -8", {
  expect_null (Classify(MatExp, MethDiscr=123))						# MethDiscr is not a string
})

test_that("MethDiscr not known -9", {
  expect_null (Classify(MatExp, MethDiscr="Thresold"))				# "Thresold" vs "Threhsold"
})

test_that("MethDiscr NULL -10", {
  expect_null (Classify(MatExp))									# Ret is a matrix, MethDiscr must not be NULL
})

test_that("MethDiscr NULL -11", {
  expect_null (Classify(Ret, Classes=c(-1,0,1)))					# More than 2 classes, MethDiscr must not be NULL
})

test_that("Lbda not numeric -12", {
  expect_null (Classify(Ret, Lbda="azert"))							# Lbda not numeric
})

test_that("Length (Lbda) incorrect -13", {
  expect_null (Classify(Ret, Lbda=c(0.2, 0.5)))						# Length (Lbda) must equal length (Classes) - 1 -- Here  : 1
})

test_that("Lbda NULL -14", {
  expect_null (Classify(MatExp, MethDiscr="Top"))					# Ret is a matrix, Lbda must not be NULL
})

test_that("Lbda NULL -15", {
  expect_null (Classify(Ret, Classes=c(-1,0,1), MethDiscr="Top"))	# More than 2 classes, Lbda must not be NULL
})

test_that("Threshold, Lbda decreasing -16", {
  expect_null (Classify(Ret, Classes=c(-1,0,1), MethDiscr = 'Threshold', Lbda=c(0.5, 0.2)))	# MethDiscr = 'Threshold' -- Lbda must be in increasing order
})

test_that("Top, Lbda increasing -17", {
  expect_null (Classify(Ret, Classes=c(-1,0,1), MethDiscr="Top", Lbda=c(10, 50)))	# MethDiscr = 'Top' -- Lbda must be in decreasing order
})


#
#	PART TWO : Check classifications
#

MExt1		<- matrix(c(1,1,1,1, 12,18,7,21,  4,9,6,12, 10,18,2,13, 2,9,1,4), nrow=4)
Expected	<- matrix(c(0,0,0,0, 1,1,0,1, 0,0,0,1, 0,1,0,1, 0,0,0,0), nrow=4)
test_that("Simple_1", {
	expect_equal (Classify (MExt1, MethDiscr="Threshold", Lbda=10)$rDig, Expected, tolerance=1E-4)
})

Expected	<- matrix(c(-1,-1,-1,-1, 0,1,-1,1, -1,0,-1,0, 0,1,-1,0, -1,0,-1,-1), nrow=4)
test_that("3 Classes", {
	expect_equal (Classify (MExt1, Classes=c(-1,0,1), MethDiscr="Threshold", Lbda=c(7,13))$rDig, Expected, tolerance=1E-4)
})

data(MatExp2)											# Dream Challenge 4 (InSilico-10-1 network)  (article fig 4A)
data(Perturb2)
Ret		<- MRARegress (MatExp2, Perturb2)
A=Classify (Ret, Verbose=TRUE)
test_that("Returned variables_1", {
	expect_equal (A$Input$Thresholds, 0.2214895, tolerance=1E-4)
})

test_that("Returned variables_2", {
	expect_equal (A$Input$Method, "Threshold", tolerance=1E-4)
})

y1=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
y2=c(1, 0, 0, 0, 0, 1, 0, 1, 0, 0)
y3=c(0, 0, 0, 1, 0, 0, 0, 0, 1, 0)
y4=c(1, 0, 1, 0, 0, 0, 1, 0, 0, 1)
y5=c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
y6=c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0)
y7=c(1, 0, 1, 1, 0, 0, 0, 0, 0, 1)
y8=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
y9=c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0)
y10=c(0, 0, 0, 0, 0, 0, 0, 1, 1, 0)
Expected = rbind(y1,y2,y3,y4,y5,y6,y7,y8,y9,y10)
dimnames(A$rDig)	<- NULL
dimnames(Expected)	<- NULL
test_that("InSilico_10_1", {
	expect_equal (A$rDig, Expected, tolerance=1E-4)
})

set.seed(12345)
y = rnorm (100)
B=Classify (y, MethDiscr="Top", Classes=c(0,1,2), Lbda=c(50,10))
test_that("Top_1", {
	expect_equal (sum(ifelse(B$rDig == 2, 1, 0)), 10, tolerance=1E-4)
})

test_that("Top_2", {
	expect_equal (sum(ifelse(B$rDig == 1, 1, 0)), 40, tolerance=1E-4)
})

test_that("Top_3", {
	expect_equal (sum(ifelse(B$rDig ==0, 1, 0)), 50, tolerance=1E-4)
})

Ret		<- MRARegress (MatExp2, Perturb2)
C=Classify (Ret, Classes=c(-1,0,1), MethDiscr="Top", Lbda=c(90,10), Verbose=TRUE)
test_that("Top_4", {
	expect_equal (C$Input$Thresholds, c(-0.1354131,0.2114053), tolerance=1E-4)
})
