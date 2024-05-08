#
#	MRARegress unit tests
#

#	PART ONE : check input data
#		(MatExp, Perturb, NodeNames, KnlgMap, Method , MapExper)
#		Check that MatExp, KnlgMap, MapExper are matrices and Perturb, NodeNames are vectors
Vect	<- c(1, 2, 3, 4)
MatExp		<- matrix(c(1, 1.1, 1.25, 0, 0, 1.32, 1.25, 1.54, 2.03, 0.025, 0, 0.001), nrow=3)
MatrArr	<- array(c(0, 0, 1.32, 1.25, 1.54, 2.03, 0.025, 0), dim=c(2,2,2))

cat ("MRARgress MatExp is a matrix -1 \n")
test_that("MatExp is a matrix -1", {
  expect_null (MRARegress (Vect)$r)										# Not a matrix
})

test_that("MatExp is a matrix -2", {
  expect_null (MRARegress (MatrArr)$r)									# Not a matrix
})

test_that("KnlgMap is null or a matrix -1", {
  expect_null (MRARegress (MatExp, KnlgMap=Vect)$r)						# Not a matrix
})

test_that("KnlgMap is null or a matrix -2", {
  expect_null (MRARegress (MatExp, KnlgMap=MatrArr)$r)					# Not a matrix
})

test_that("MapExper is null or a matrix -1", {
  expect_null (MRARegress (MatExp, MapExper=Vect)$r)					# Not a matrix
})

test_that("MapExper is null or a matrix -2", {
  expect_null (MRARegress (MatExp, MapExper=MatrArr)$r)					# Not a matrix
})

test_that("Perturb is null or a vector -1", {
  expect_null (MRARegress (MatExp, Perturb=MatExp)$r)					# Not a vector
})

test_that("Perturb is null or a vector -2", {
  expect_null (MRARegress (MatExp, Perturb=MatrArr)$r)					# Not a vector
})

test_that("NodeName is null or a vector -1", {
  expect_null (MRARegress (MatExp, NodeName=MatExp)$r)					# Not a vector
})

test_that("NodeName is null or a vector -2", {
  expect_null (MRARegress (MatExp, NodeName=MatrArr)$r)					# Not a vector
})

test_that("Relative is logical", {
  expect_null (MRARegress (MatExp, Relative="faux")$r)					# Not a logical (TRUE, FALSE)
})

test_that("Verbose is logical", {
  expect_null (MRARegress (MatExp, Verbose="faux")$r)					# Not a logical (TRUE, FALSE)
})

test_that("NoPrint is logical", {
  expect_null (MRARegress (MatExp, NoPrint="faux")$r)					# Not a logical (TRUE, FALSE)
})


#	PART TWO : check input data
#		(MatExp, Perturb, NodeNames, KnlgMap, Method , MapExper)
#		Check specific requirements of these objects

#	MatExp : matrix [nbRow1, nbCol1]
#		MatExp[i,j] >= 0
#		If Perturb is NULL, the number of perturbations must equal the number of nodes, so nbCol1 == nbRow1 +1

MatExp		<- matrix(c(0, 0, "A", 1.25, 1.54, 2.03, "Bravo", 0, 0.001), nrow=3)	
test_that("Input Data - Test 1", {
  expect_null (MRARegress (MatExp)$r)									# Not a matrix of numbers
})

MatExp		<- matrix(c(0, 0, 1.32, 1.25, 1.54, 2.03, 0.025, 0, 0.001), nrow=3)
test_that("Input Data - Test 2", {
  expect_null (MRARegress (MatExp)$r)									# Perturb is null and nbCol1 != nbRow1 +1
})


#	Perturb : vector [nbCol2]
#		One basal column at least.
#		nbCol2 == nbCol1
#		Name of nodes correspond to those of NodeName
#		If MapExper = NULL, a perturbation acts upon one node only
#		If MapExper = NULL, every node is perturbed by one perturbation at least
#		If MapExper != NULL, there must be exactly one basal column.

MatExp		<- matrix(c(0, 0, 1.32, 1.25, 1.54, 2.03, 0.025, 0, 0.001), nrow=3)
Perturb		<- c("Pert1", "Pert2", "Pert3")
test_that("Input Data - Test 3", {
  expect_null (MRARegress (MatExp, Perturb)$r)							# No basal columns
})

Perturb		<- c("Pert1", "Pert2", "Pert3", "Base")
test_that("Input Data - Test 4", {
  expect_null (MRARegress (MatExp, Perturb)$r)							# 3 perturbations instead of 2 in MatExp
})

MatExp		<- matrix(c(1, 1.1, 1.25, 0, 0, 1.32, 1.25, 1.54, 2.03, 0.025, 0, 0.001), nrow=3)
Perturb		<- c("Base", "Pert1-> Node1", "Pert2 ->  N2", "Pert3 -> N3")
test_that("Input Data - Test 5", {
  expect_null (MRARegress (MatExp, Perturb)$r)							# Node1 doesn't belong to NodeName
})

Perturb		<- c("Base", "Pert1-> N1", "Pert2 ->  N2", "Pert1 -> N3")
test_that("Input Data - Test 6", {
  expect_null (MRARegress (MatExp, Perturb)$r)							# Pert1 acts upon 2 nodes and H6 is true
})

Perturb		<- c("Base", "Pert1-> N1", "Pert2 ->  N2", "Pert3 -> N1")
test_that("Input Data - Test 7", {
  expect_null (MRARegress (MatExp, Perturb)$r)							# No perturbation act upon N3 and H6 is true
})

Perturb		<- c("Base", "Pert1-> N1", "Pert2 ->  N2", "Base")
MapExper	<- matrix(c(1,1, 2,2, 3,3), nrow=2)
test_that("Input Data - Test 8", {
  expect_null (MRARegress (MatExp, Perturb, MapExper=MapExper)$r)		# H6 FALSE : only one basal column is authorized
})

Perturb		<- c("Base", "Pert1-> N1", "Pert2 ->  N2", "Pert3->N3")
test_that ("Input Data - Test 9", {
	expect_vector (MRARegress (MatExp, Perturb)$r)						# H6 TRUE : works fine
})

Perturb		<- c("Base", "Pert1-> N1", "Pert2 ", "Pert3->N3")
test_that ("Input Data - Test 10", {
	expect_null (MRARegress (MatExp, Perturb)$r)						# H6 TRUE : Pert2 Syntax Error
})


MatExp		<- matrix(c(1,1.1,1.25,  0,0,1.32,  1.25,1.54,2.03,   0,0,1.32), nrow=3)
test_that ("Input Data - Test 11", {
	expect_null (MRARegress (MatExp)$r)									# H6 TRUE : The rank of the system is not sufficient !
})

MatExp		<- matrix(c(1,1.1,1.25,  0,0,1.32,  1.25,1.54,2.03,  0.025,0,0.001,  1.06,1.29,3.31), nrow=3)
MapExper	<- matrix(c(1,1, 2,2, 3,3, 4,4), nrow=2)
Perturb		<- c("Base", "Pert1-> N1", "Pert2 ->  N2", "Pert3->N3", "Pert4->N1")
ParNode		<- matrix(1, nrow=3, ncol=2)
test_that ("Input Data - Test 12", {
	expect_null (MRARegress (MatExp, Perturb, MapExper=MapExper, ParNode=ParNode)$r)	# H6 FALSE : The rank of the system is not sufficient !
})

#	Different number of replicates
MatExp		<- matrix(c(1,2,3, 11,21,31, 12,20,32, 13,23,33, 14,24,34, 15,25,35, 14,26,36), nrow=3)
Perturb		<- c("Base", "A->N1", "A->N1", "A->N1", "B->N2", "C->N3", "C->N3")
#MatrCc		<- matrix(c(-1,0.5,-1, 2,-1,2, -1,0.5,-1), nrow=3)
MatrCc		<- matrix(c(-1,0.780,0.841, -1.482,-1,0.986, 1.482,0.384,-1), nrow=3)
Res			<- MRARegress(MatExp, Perturb, Relative=FALSE)$r
dimnames(Res)		<- NULL
test_that ("Input Data - Test 13", {
	expect_equal (Res, MatrCc,  tolerance=1E-3)
})

MatExp		<- matrix(c(1,2,3, 11,21,31, 12,22,32, 13,23,33, 14,24,34, 15,25,35, 16,26,36), nrow=3)
Perturb		<- c("Base", "A->N1", "A->N1", "A->N1", "B->N3", "C->N3", "C->N3")
test_that ("Input Data - One node is not perturbed", {
	expect_null (MRARegress (MatExp, Perturb, Relative=FALSE)$r)		# One node is not perturbed (N2)
})

#	NodeName : vector [nbRow3]
#		nbRow3 == nbRow1
#		Different nodes must have different names.

MatExp		<- matrix(c(1, 1.1, 1.25, 0, 0, 1.32, 1.25, 1.54, 2.03, 0.025, 0, 0.001), nrow=3)
NodeName	<- c("G1", "G2", "G3", "G4")
test_that("Input Data - Test 14", {
  expect_null (MRARegress (MatExp, NodeName=NodeName)$r)				# NodeName has 4 items, MatExp has 3 rows
})

NodeName	<- c("G1", "G2", "G2")
test_that("Input Data - Test 15", {
  expect_null (MRARegress (MatExp, NodeName=NodeName)$r)				# Two nodes have the same name
})


#	KnlgMap : matrix [nbRow4, nbCol4]
#		nbRow4 == nbRow1  &&  nbCol4 == nbRow4
#		composed of {0, 1, -1, "x"}

MatExp		<- matrix(c(1, 1.1, 1.25, 0, 0, 1.32, 1.25, 1.54, 2.03, 0.025, 0, 0.001), nrow=3)
KnlgM		<- matrix ('x', nrow=3, ncol=4)
test_that("Input Data - Test 16", {
  expect_null (MRARegress (MatExp, KnlgMap=KnlgM)$r)					# KnlgMap is not a square matrix
})

KnlgM		<- matrix ('x', nrow=4, ncol=4)
test_that("Input Data - Test 17", {
  expect_null (MRARegress (MatExp, KnlgMap=KnlgM)$r)					# KnlgMap has 4 rows, MatExp has 3 rows
})

KnlgM		<- matrix (0, nrow=3, ncol=3)
KnlgM[2,3]	<- 2
test_that("Input Data - Test 18", {
  expect_null (MRARegress (MatExp, KnlgMap=KnlgM)$r)					# 2 is not allowed in KnlgMap
})

KnlgM		<- matrix (0, nrow=3, ncol=3)
KnlgM[2,1]	<- -1
KnlgM[2,3]	<- 1
KnlgM[3,2]	<- 1
test_that("Input Data - Test 19", {
  expect_null (MRARegress (MatExp, KnlgMap=KnlgM, Method="LASSO")$r)	# KnlgMap works only with "TLR"
})


#	Method : a string in c("TLR", "LASSO", "RIDGE", "Elastic Net", "STEP", "ARACNE", "CLR", "RMNET", "Random Forest")

test_that("Input Data - Test 20", {
  expect_null (MRARegress (MatExp, Perturb, Method="Random Forrest")$r)	# Forrest instead of Forest (unknown method)
})


#	Hyp_Lbda : a positive or 0 number

test_that("Input Data - Test 21", {
  expect_null (MRARegress (MatExp, Perturb, Hyp_Lbda = "A")$r)			# Hyp_Lbda is not a number
})

test_that("Input Data - Test 22", {
  expect_null (MRARegress (MatExp, Perturb, Hyp_Lbda = -2)$r)			# Hyp_Lbda is not a positive number
})


#	Hyp_Mu : a number between 0 and 1

test_that("Input Data - Test 23", {
  expect_null (MRARegress (MatExp, Perturb, Hyp_Mu = "A")$r)			# Hyp_Mu is not a number
})

test_that("Input Data - Test 24", {
  expect_null (MRARegress (MatExp, Perturb, Hyp_Mu = -2)$r)				# Hyp_Mu is negative
})

test_that("Input Data - Test 25", {
  expect_null (MRARegress (MatExp, Perturb, Hyp_Mu = 1.05)$r)			# Hyp_Mu is > 1
})


#	Hyp_Step must belong to "Fo", "Ba", "Bo"

test_that("Input Data - Test 26", {
  expect_null (MRARegress (MatExp, Perturb, Hyp_Step = "FO")$r)			# Hyp_Step doesn't belong to "Fo", "Ba", "Bo"
})


#	Hyp_Eps : a positive or 0 number

test_that("Input Data - Test 27", {
  expect_null (MRARegress (MatExp, Perturb, Hyp_Eps = "A")$r)			# Hyp_Eps is not a number
})

test_that("Input Data - Test 28", {
  expect_null (MRARegress (MatExp, Perturb, Hyp_Eps = -2)$r)			# Hyp_Eps is not a positive number
})


#	Hyp_Cvx : a positive or 0 number

test_that("Input Data - Test 29", {
  expect_null (MRARegress (MatExp, Perturb, Hyp_Cvx = "A")$r)			# Hyp_Cvx is not a number
})

test_that("Input Data - Test 30", {
  expect_null (MRARegress (MatExp, Perturb, Hyp_Cvx = -2)$r)			# Hyp_Cvx is not a positive number
})


#	MapExper : matrix [nbRow5, nbCol5]
#		nbCol5 == NbP
#		Perturbations must be "partially proportional"

MatExp		<- matrix(c(0, 0, 1.32, 1.25, 1.54, 2.03, 0.025, 0, 0.001), nrow=3)
MapExper	<- matrix(c(1,1,1, 2,2,2, 3,3,3), nrow=3)
test_that("Input Data - Test 31", {
  expect_null (MRARegress (MatExp, MapExper=MapExper)$r)				# 2 perturbations only (MatExp), but MapExper has 3 columns
})

MatExp		<- matrix(c(1, 1.1, 1.25, 0, 0, 1.32, 1.25, 1.54, 2.03, 0.025, 0, 0.001), nrow=3)
test_that("Input Data - Test 32", {
  expect_null (MRARegress (MatExp, MapExper=MapExper)$r)				# 3 perturbations : not enough if MapExper != NULL (NbP >= (NbN-1+NbM))
})

MapExper	<- matrix(c(1,1,1, 2,0,2, 3,4,3), nrow=3)
test_that("Input Data - Test 33", {
  expect_null (MRARegress (MatExp, MapExper=MapExper)$r)				# The 3° perturbation is not proportional to the reference
															# Pas assez de perturbations !!!
})


#	PART THREE : check results vs known results (see "My Document")
#

#	§ 5.2.1 : 3 perturbations acting on 3 nodes independantly. No a priori knowledge.

MatExp		<- matrix(c(1,2,3, 11,21,31, 12,22,32, 13,23,33),nrow=3)	# 3 nodes, 3 perturbations, Base : 1st column
MatrCc		<- matrix(0, nrow=3, ncol=3)
MatD0		<- matrix(0, nrow=3, ncol=3)
for (iNode in 1:3) {
	MatD0[ ,iNode]	<- MatExp[ ,iNode+1] - MatExp[ ,1]
}
Res			<- MRARegress (MatExp, Relative=FALSE)$r
dimnames(Res)	<- NULL						# MatrCc has no names
test_that("Functional test - Test 5.2.1/1", {
  expect_equal (Res %*% MatD0, MatrCc, tolerance=1E-9)
})

#	Many perturbations and replicates

MExp2		<- matrix(c(1,2,3, 11,21,31, 12,20,32, 13,23,33, 14,24,34, 15,25,35, 14,26,36),nrow=3)
Pert2 		<- c("Base", "Q1 -> N1", "Q2 -> N1", "Q3 -> N1", "Q4 -> N2", "Q5 -> N3", "Q6 -> N3")
Res2 		<- MRARegress(MExp2, Pert2, Relative=FALSE)$r
dimnames(Res2)	<- NULL						# MatrCc has no names
MatrCc		<- matrix(c(-1,0.780,0.841, -1.482,-1,0.986, 1.482,0.384,-1), nrow=3)

test_that("Functional test - Test 5.2.1/2", {
  expect_equal (Res2, MatrCc, tolerance=1E-3)
})


#	First example (Vignette and Fig. 1 - 1st Document) : 6 MAP Kinase network

data(MatExp1)
Row1		<- c(-1.000000e+00, -3.520785e-01, -2.006777e-06, -4.043092e-06, -1.815467e-07,  0.02588589)
Row2		<- c(-1.980530e+00, -1.000000e+00, -6.252010e-06, -1.260483e-05, -5.706423e-07, -0.09578897)
Row3		<- c(-1.402200e-04, -2.216055e-01, -1.000000e+00, -1.567440e+00, -1.267826e-05,  0.08093872)
Row4		<- c(-8.461649e-05,  2.671487e-01, -4.854243e-01, -1.000000e+00, -7.311116e-06, -0.09772401)
Row5		<- c(-3.411608e-05, -1.859338e-05, -4.348831e-06, -5.244209e-01, -1.000000e+00, -0.13468542)
Row6		<- c(-1.591421e-05, -8.644637e-06, -1.991402e-06,  9.807480e-01, -1.381415e+00, -1.00000000)
MatrCc1		<- rbind(Row1, Row2, Row3, Row4, Row5, Row6)
dimnames(MatrCc1)	<- NULL						# The returned matrix by MRARegress has no names
Res			<- MRARegress (MatExp1)$r
dimnames(Res)		<- NULL

test_that("Functional test - Example 1 (MAP Kinase)", {
  expect_equal (Res, MatrCc1, tolerance=1E-4)
})


#	Second example : Dream Challenge 4 (InSilico-10-1 network)

data(MatExp2)
data(Perturb2)

Row1		<- c(-1.00000000, -0.14231031, -0.01045375, -0.12354482, -0.17032725, -0.061322806,  0.01487041, -0.20759827, -0.007264386, -0.12200879)
Row2		<- c(-0.58045637, -1.00000000, -0.11957210, -0.07296114, -0.14397474, -0.354066047,  0.11505944, -0.62082438, -0.016653255, -0.08848024)
Row3		<- c( 0.14412771, -0.05783490, -1.00000000, -0.31010954, -0.07271220, -0.132026594,  0.18196917, -0.13286355,  0.273421187, -0.02704505)
Row4		<- c( 0.40385768,  0.02942327,  0.28026427, -1.00000000,  0.13149852, -0.006004035,  0.35452033,  0.03592345,  0.025670353,  0.27851254)
Row5		<- c(-0.74294591, -0.12538657,  0.22068150, -0.12503039, -1.00000000,  0.121369450,  0.20864994,  0.02017769, -0.156248441,  0.12383694)
Row6		<- c(-0.04914371,  0.02232601, -0.12056684, -0.00816866,  0.02322817, -1.000000000,  0.01953283, -0.35972859,  0.083981553, -0.03650592)
Row7		<- c(-0.52784177, -0.11392901, -0.27171809,  0.47950665, -0.17293211, -0.064910002, -1.00000000, -0.13557909, -0.024333719, -0.30726384)
Row8		<- c(-0.07971840, -0.06798848, -0.12703283,  0.02115746,  0.03092872, -0.037336208, -0.05994197, -1.00000000, -0.004598030, -0.09541428)
Row9		<- c(-0.20612300, -0.15611072, -0.11378455,  0.05751696, -0.12909957, -0.172998128, -0.09056198, -0.23912248, -1.000000000, -0.19818472)
Row10		<- c( 0.16569299,  0.17120122,  0.19413016, -0.02001577,  0.06362728,  0.149321003,  0.15511302,  0.28691314,  0.885958137, -1.00000000)
MatrCc2		<- rbind(Row1, Row2, Row3, Row4, Row5, Row6, Row7, Row8, Row9, Row10)
dimnames(MatrCc2)	<- NULL						# The returned matrix by MRARegress has no names
Res			<- MRARegress (MatExp2, Perturb2)$r
dimnames(Res)		<- NULL

test_that("Functional test - Example 2 (DC4 InSilico-10-1)", {
  expect_equal (Res, MatrCc2, tolerance=1E-4)
})


#	PART FOUR : check unusual configurations
#

#	Perturbations are not independant
#
MExt1	<-	matrix(c(1,1,1,1,  6,7,8,9,  11,21,16,17,  12,30,25,26,  13,39,34,35), nrow=4)
test_that("Non independant perturbations, H6", {
  expect_null (MRARegress (MExt1, Relative=FALSE)$r)			# 4 non independant perturbations
})


#	H6 hypothesis not verified
#

#	X1 = ρ.θ.cos(θ),  X1 = ρ.θ.sin(θ).	P0 (ρ = 1, θ = pi/4)

X1		<- (pi/4)*cos(pi/4)									# 0.5553604
X2		<- (pi/4)*sin(pi/4)									# 0.5553604

# P1 : we perturb ρ  : 0.99 vs 1
X11		<- 0.99*X1											# 0.5498068
X21		<- 0.99*X2											# 0.549806
# P2 : we perturb θ  : 0.99*pi/4 vs pi/4
X12		<- (0.99*pi/4)*cos(0.99*pi/4)
X22		<- (0.99*pi/4)*sin(0.99*pi/4)
# P3 : we perturb θ  : 0.80*pi/4 vs pi/4
X13		<- (0.8*pi/4)*cos(0.8*pi/4)
X23		<- (0.8*pi/4)*sin(0.8*pi/4)
# P4 : we perturb θ  : 0.70*pi/4 vs pi/4
X14		<- (0.7*pi/4)*cos(0.7*pi/4)
X24		<- (0.7*pi/4)*sin(0.7*pi/4)
# P5 : we perturb ρ  : 0.98 vs 1
X15		<- 0.98*X1
X25		<- 0.98*X2

# First, we cheat with the program. We suppose H6 is true.
MExt1		<- matrix(c(X1,X2, X11,X21, X12,X22), nrow=2)
MatrCc1		<- matrix(c(-1,1, 0.12665,-1), nrow=2)
Res			<- MRARegress (MExt1, Relative=FALSE)$r
dimnames(Res)	<- NULL
test_that("H6 is FALSE, we cheat", {
	expect_equal (Res, MatrCc1, tolerance=1E-4)
})

# Then, we suppose H6 is false.
#	but perturbations P1 and P5 lead to the same equation
MExt2		<- matrix(c(X1,X2, X11,X21, X15,X25, X14,X24), nrow=2)
MOper2		<- matrix(c(1,0, 2.010101,0, 0,3), nrow=2)
Pert2		<- c("Base", "P1->N1", "P5->N2", "P4->N2")
PNode2		<- matrix(1, nrow=2, ncol=2)
MatrCc2		<- matrix(c(-1,1, 1,-1), nrow=2)
Res			<- MRARegress (MExt2, Pert2, MapExper=MOper2, ParNode=PNode2, Relative=FALSE)$r
dimnames(Res)	<- NULL
test_that("Perturbations lead to the same equation, H6 False", {
  expect_equal (Res, MatrCc2, tolerance=1E-4)	
})

#	At last, we have three independant perturbations.
MExt3		<- matrix(c(X1,X2, X11,X21, X13,X23, X14,X24), nrow=2)
MOper3		<- matrix(c(1,0, 0,2, 0,3), nrow=2)
Pert3		<- c("Base", "P1->N1", "P3->N2", "P4->N2")
PNode3		<- matrix(1, nrow=2, ncol=2)
MatrCc3		<- matrix(c(-1,-0.683685, -1.46266,-1), nrow=2)
Res			<- MRARegress (MExt3, Pert3, MapExper=MOper3, ParNode=PNode3, Relative=FALSE)$r
dimnames(Res)	<- NULL
test_that("H6 is FALSE, we don't cheat", {
	expect_equal (Res, MatrCc3, tolerance=1E-4)
})


#	Two basal columns
#
MExt2		<- matrix(c(1,1,1,1, 3,1,6,9,  12,17,7,21, 4,9,6,12,  12,18,7,21, 4,9,6,13), nrow=4)
Pert2		<- c("Base", "Base", "P1->N1", "P2->N2", "P'1->N3", "P'2->N4")
# Remark : we have introduced P'1=P1 and P'2=P2 so as to tell the programm that the new perturbations generated act upon nodes N3 and N4, to avoid an error when checking the input parameters.
Res2		<- MRARegress (MExt2, Pert2, Relative=FALSE)$r
dimnames(Res2)	<- NULL
MatrCc		<- matrix (c(-1,-0.7167,-0.3602,0.6528, -0.6245,-1,-0.2625,0.2896, -2.5548,-3.2,-1,1.3416, 1.8281,2.1792,0.7074,-1), nrow=4)
test_that("Two basal columns", {
	expect_equal (Res2, MatrCc, tolerance=1E-4)
})


#	The basal column is not the first one
#
MExt1		<- matrix(c(1,1,1,1,   12,18,7,21,  4,9,6,12, 10,18,2,13, 2,9,1,4), nrow=4)
MExt2		<- matrix(c(12,18,7,21,  4,9,6,12, 10,18,2,13, 1,1,1,1,   2,9,1,4), nrow=4)
Pert2		<- c("P1->N1", "P2->N2", "P3->N3", "Base", "P4->N4")
Res1		<- MRARegress (MExt1, Relative=FALSE)$r
dimnames(Res1)	<- NULL
Res2		<- MRARegress (MExt2, Pert2, Relative=FALSE)$r
dimnames(Res2)	<- NULL
test_that("The basal column is not the first one", {
	expect_equal (Res1, Res2, tolerance=1E-4)
})


#	Test the different methods, 
#	using the results of Dream Challenge 4, size=10, Fic.1 then Fic.5
#
#	Load "Perturb2"
load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\Perturb2.rda")

#	Load "MatExp_10_1" (InSilico_10_1)
load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\MatExp_10_1.rda")

###		#	The tests have been done with glmnet v 4.1-4, to be compatible with the article written in BioInformatics  -- Not useful, not implemented
###		library("glmnet", lib="C:\\Users\\jean-pierre.borg\\IRCM\\glmnet414\\MyEnv")

#	Method = "TLR"
Res			<- MRARegress(MatExp, Perturb2, Method="TLR")$r
dimnames(Res)	<- NULL
diag(Res)	<- 0
load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\MatrCc_TLR_10_1.rda")	# Expected result, in MatrCc_TLR_10_1
dimnames(MatrCc_TLR_10_1)	<- NULL						# The returned matrix by MRARegress has no names
test_that("TLR_InSilico_10_1", {
	expect_equal (Res, MatrCc_TLR_10_1, tolerance=1E-4)
})

Res			<- MRARegress(MatExp, Perturb2, Method="TLR", Verbose=TRUE)$r
dimnames(Res)	<- NULL
diag(Res)	<- 0
test_that("TLR_InSilico_10_1, Verbose", {
	expect_equal (Res, MatrCc_TLR_10_1, tolerance=1E-4)
})

#	Method = "Elastic Net"
set.seed(12345)
Res			<- MRARegress(MatExp, Perturb2, Method="Elastic Net", Hyp_Mu=0.3)$r
dimnames(Res)	<- NULL
diag(Res)	<- 0
load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\MatrCc_ENet_03_10_1.rda")	#Expected result, in MatrCc_ENet_03_10_1
dimnames(MatrCc_ENet_03_10_1)	<- NULL						# The returned matrix by MRARegress has no names
test_that("Elastic Net_InSilico_10_1", {
	expect_equal (Res, MatrCc_ENet_03_10_1, tolerance=1E-4)
})

#	Method = "RIDGE"
set.seed(12345)
Res			<- MRARegress(MatExp, Perturb2, Method="RIDGE")$r
dimnames(Res)	<- NULL
diag(Res)	<- 0
load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\MatrCc_RIDGE_10_1.rda")	#Expected result, in MatrCc_ENet_03_10_1
dimnames(MatrCc_ENet_03_10_1)	<- NULL						# The returned matrix by MRARegress has no names
test_that("RIDGE_InSilico_10_1", {
	expect_equal (Res, MatrCc_RIDGE_10_1, tolerance=1E-4)
})

#	Method = "LASSO"
set.seed(130747)								# Value used by the test
Res			<- MRARegress(MatExp, Perturb2, Method="LASSO")$r
dimnames(Res)	<- NULL
diag(Res)	<- 0
load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\MatrCc_LASSO_10_1.rda")	# Expected result, in MatrCc_LASSO_10_1
dimnames(MatrCc_LASSO_10_1)	<- NULL						# The returned matrix by MRARegress has no names
test_that("LASSO_InSilico_10_1", {
	expect_equal (Res, MatrCc_LASSO_10_1, tolerance=1E-4)
})

#	Method = "STEP-Fo"
Res			<- MRARegress(MatExp, Perturb2, Method="STEP")$r
dimnames(Res)	<- NULL
diag(Res)	<- 0
load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\MatrCc_STEP_Fo_10_1.rda")	# Expected result, in MatrCc_STEP_Fo_10_1
dimnames(MatrCc_STEP_Fo_10_1)	<- NULL					# The returned matrix by MRARegress has no names
test_that("STEP_Fo_InSilico_10_1", {
	expect_equal (Res, MatrCc_STEP_Fo_10_1, tolerance=1E-4)
})

#	Method = "STEP-Ba"
Res			<- MRARegress(MatExp, Perturb2, Method="STEP", Hyp_Step="Ba")$r
dimnames(Res)	<- NULL
diag(Res)	<- 0
load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\MatrCc_STEP_Ba_10_1.rda")	# Expected result, in MatrCc_STEP_Ba_10_1
dimnames(MatrCc_STEP_Ba_10_1)	<- NULL					# The returned matrix by MRARegress has no names
test_that("STEP_Ba_InSilico_10_1", {
	expect_equal (Res, MatrCc_STEP_Ba_10_1, tolerance=1E-4)
})

#	Method = "STEP-Bo"
Res			<- MRARegress(MatExp, Perturb2, Method="STEP", Hyp_Step="Bo")$r
dimnames(Res)	<- NULL
diag(Res)	<- 0
load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\MatrCc_STEP_Bo_10_1.rda")	# Expected result, in MatrCc_STEP_Bo_10_1
dimnames(MatrCc_STEP_Bo_10_1)	<- NULL					# The returned matrix by MRARegress has no names
test_that("STEP_Bo_InSilico_10_1", {
	expect_equal (Res, MatrCc_STEP_Bo_10_1, tolerance=1E-4)
})

#	Method = "ARACNE"
Res			<- MRARegress(MatExp, Perturb2, Method="ARACNE")$r
dimnames(Res)	<- NULL
diag(Res)	<- 0
load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\MatrCc_ARACNE_10_1.rda")		# Expected result, in MatrCc_ARACNE_10_1
dimnames(MatrCc_ARACNE_10_1)	<- NULL					# The returned matrix by MRARegress has no names
test_that("ARACNE_InSilico_10_1", {
	expect_equal (Res, MatrCc_ARACNE_10_1, tolerance=1E-4)
})

#	Method = "CLR"
Res			<- MRARegress(MatExp, Perturb2, Method="CLR")$r
dimnames(Res)	<- NULL
diag(Res)	<- 0
load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\MatrCc_CLR_10_1.rda")		# Expected result, in MatrCc_CLR_10_1
dimnames(MatrCc_CLR_10_1)	<- NULL						# The returned matrix by MRARegress has no names
test_that("CLR_InSilico_10_1", {
	expect_equal (Res, MatrCc_CLR_10_1, tolerance=1E-4)
})

#	Method = "MRNET"
Res			<- MRARegress(MatExp, Perturb2, Method="MRNET")$r
dimnames(Res)	<- NULL
diag(Res)	<- 0
load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\MatrCc_MRNET_10_1.rda")		# Expected result, in MatrCc_MRNET_10_1
dimnames(MatrCc_MRNET_10_1)	<- NULL					# The returned matrix by MRARegress has no names
test_that("MRNET_InSilico_10_1", {
	expect_equal (Res, MatrCc_MRNET_10_1, tolerance=1E-4)
})

#	Method "Random Forest"
set.seed(130747)
Res			<- MRARegress(MatExp, Perturb2, Method="Random Forest")$r
dimnames(Res)	<- NULL
diag(Res)	<- 0
load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\MatrCc_RForest_10_1.rda")	# Expected result, in MatrCc_RForest_10_1
dimnames(MatrCc_RForest_10_1)	<- NULL					# The returned matrix by MRARegress has no names
test_that("RForest_InSilico_10_1", {
	expect_equal (Res, MatrCc_RForest_10_1, tolerance=1E-4)
})

#	Load "MatExp_10_5" (InSilico_10_5)
load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\MatExp_10_5.rda")

#	Method = "TLR"
Res			<- MRARegress(MatExp, Perturb2, Method="TLR")$r
dimnames(Res)	<- NULL
diag(Res)	<- 0
load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\MatrCc_TLR_10_5.rda")	# Expected result, in MatrCc_TLR_10_5
dimnames(MatrCc_TLR_10_5)	<- NULL						# The returned matrix by MRARegress has no names
test_that("TLR_InSilico_10_5", {
	expect_equal (Res, MatrCc_TLR_10_5, tolerance=1E-4)
})

#	Method = "LASSO"
set.seed(130747)								# Value used by the test
Res			<- MRARegress(MatExp, Perturb2, Method="LASSO")$r
dimnames(Res)	<- NULL
diag(Res)	<- 0
load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\MatrCc_LASSO_10_5.rda")	# Expected result, in MatrCc_LASSO_10_5
dimnames(MatrCc_LASSO_10_5)	<- NULL						# The returned matrix by MRARegress has no names
test_that("LASSO_InSilico_10_5", {
	expect_equal (Res, MatrCc_LASSO_10_5, tolerance=1E-4)
})

#	Method = "STEP-Fo"
Res			<- MRARegress(MatExp, Perturb2, Method="STEP")$r
dimnames(Res)	<- NULL
diag(Res)	<- 0
load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\MatrCc_STEP_Fo_10_5.rda")	# Expected result, in MatrCc_STEP_Fo_10_5
dimnames(MatrCc_STEP_Fo_10_5)	<- NULL					# The returned matrix by MRARegress has no names
test_that("STEP_Fo_InSilico_10_5", {
	expect_equal (Res, MatrCc_STEP_Fo_10_5, tolerance=1E-4)
})

#	Method = "STEP-Ba"
Res			<- MRARegress(MatExp, Perturb2, Method="STEP", Hyp_Step="Ba")$r
dimnames(Res)	<- NULL
diag(Res)	<- 0
load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\MatrCc_STEP_Ba_10_5.rda")	# Expected result, in MatrCc_STEP_Ba_10_5
dimnames(MatrCc_STEP_Ba_10_5)	<- NULL					# The returned matrix by MRARegress has no names
test_that("STEP_Ba_InSilico_10_5", {
	expect_equal (Res, MatrCc_STEP_Ba_10_5, tolerance=1E-4)
})

#	Method = "STEP-Bo"
Res			<- MRARegress(MatExp, Perturb2, Method="STEP", Hyp_Step="Bo")$r
dimnames(Res)	<- NULL
diag(Res)	<- 0
load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\MatrCc_STEP_Bo_10_5.rda")	# Expected result, in MatrCc_STEP_Bo_10_5
dimnames(MatrCc_STEP_Bo_10_5)	<- NULL					# The returned matrix by MRARegress has no names
test_that("STEP_Bo_InSilico_10_5", {
	expect_equal (Res, MatrCc_STEP_Bo_10_5, tolerance=1E-4)
})

#	Method = "ARACNE"
Res			<- MRARegress(MatExp, Perturb2, Method="ARACNE")$r
dimnames(Res)	<- NULL
diag(Res)	<- 0
load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\MatrCc_ARACNE_10_5.rda")		# Expected result, in MatrCc_ARACNE_10_5
dimnames(MatrCc_ARACNE_10_5)	<- NULL					# The returned matrix by MRARegress has no names
test_that("ARACNE_InSilico_10_5", {
	expect_equal (Res, MatrCc_ARACNE_10_5, tolerance=1E-4)
})

#	Method = "CLR"
Res			<- MRARegress(MatExp, Perturb2, Method="CLR")$r
dimnames(Res)	<- NULL
diag(Res)	<- 0
load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\MatrCc_CLR_10_5.rda")		# Expected result, in MatrCc_CLR_10_5
dimnames(MatrCc_CLR_10_5)	<- NULL						# The returned matrix by MRARegress has no names
test_that("CLR_InSilico_10_5", {
	expect_equal (Res, MatrCc_CLR_10_5, tolerance=1E-4)
})

#	Method = "MRNET"
Res			<- MRARegress(MatExp, Perturb2, Method="MRNET")$r
dimnames(Res)	<- NULL
diag(Res)	<- 0
load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\MatrCc_MRNET_10_5.rda")		# Expected result, in MatrCc_MRNET_10_5
dimnames(MatrCc_MRNET_10_5)	<- NULL					# The returned matrix by MRARegress has no names
test_that("MRNET_InSilico_10_5", {
	expect_equal (Res, MatrCc_MRNET_10_5, tolerance=1E-4)
})

#	Method "Random Forest"
set.seed(130747)
Res			<- MRARegress(MatExp, Perturb2, Method="Random Forest")$r
dimnames(Res)	<- NULL
diag(Res)	<- 0
load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\MatrCc_RForest_10_5.rda")	# Expected result, in MatrCc_RForest_10_5
dimnames(MatrCc_RForest_10_5)	<- NULL					# The returned matrix by MRARegress has no names
test_that("RForest_InSilico_10_5", {
	expect_equal (Res, MatrCc_RForest_10_5, tolerance=1E-4)
})

#	Test of the utility "MatR2MatExp"
load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\MatR_Frank_TF30_TA0_1_R1.rda")#	MatRN2, noise 10%
v	<- MatR2MatExp(MatRN2, 30, 2)						# Computes the MatExp and Perturb matrices, used by MRARegress
Res	<- MRARegress(v$Exp, v$Pert)$r
dimnames(Res)	<- NULL
diag(Res)	<- 0
load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\Matr_Frank_TF30_TA0_1_rcc1.rda")	# Expected result, in MatrCc
dimnames(MatrCc)	<- NULL								# The returned matrix by MRARegress has no names
diag(MatrCc)		<- 0
test_that("MatR2MatExp", {
	expect_equal (Res, MatrCc, tolerance=1E-4)
})

#	Utility "Ode2MatExp" test will be performed at the same time as Order2 method

#	Test large size networks
#	Tests with 500 and 1000 nodes work fine, but have been removed from the set of unit tests, because of the necessary processing time and
#	of the size of the data (20.3 Mb) :
#	00:01:23 (500 nodes) and 00:20:54 (1000 nodes).
#	
load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\Frank_TF100_TA0_1_R1.rda")#	MatRN2 noise 10%
v	<- MatR2MatExp(MatRN2, 100, 2)						# Computes the MatExp and Perturb matrices, used by MRARegress
test_that("100 nodes network", {
  expect_vector(MRARegress(v$Exp, v$Pert)$r, size=100)
})

load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\Frank_TF300_TA0_1_R1.rda")#	MatRN2, noise 10%
v	<- MatR2MatExp(MatRN2, 300, 2)						# Computes the MatExp and Perturb matrices, used by MRARegress
test_that("300 nodes network", {
  expect_vector(MRARegress(v$Exp, v$Pert)$r, size=300)
})

###	These two tests have been discarded, because of the time necessary to process them.
###
###	load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\Frank_TF500_TA0_1_R1.rda")#	MatRN500, noise 10%
###	v	<- MatR2MatExp(MatRN2, 500, 2)					# Computes the MatExp and Perturb matrices, used by MRARegress
###	test_that("500 nodes network", {
###	  expect_vector(MRARegress(v$Exp, v$Pert), size=500)
###	})
###	
###	load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\Frank_TF1000_TA0_1_R1.rda")#	MatRN1000, noise 10%
###	v	<- MatR2MatExp(MatRN2, 1000, 2)					# Computes the MatExp and Perturb matrices, used by MRARegress
###	test_that("1000 nodes network", {
###	  expect_vector(MRARegress(v$Exp, v$Pert), size=1000)
###	})

#	Test of a priori knowledge, using convex optimization algorithm (CVXR)

load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\MatR_Frank_TF30_TA0_1_R1.rda")			#	MatRN2
v	<- MatR2MatExp(MatRN2, 30, 2)					# Computes the MatExp and Perturb matrices, used by MRARegress

load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\KnlgMap_TF30_TA0_1_R1_10Knwn.rda")		#	Knlg_10
load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\MatrCVXR_TF30_TA0_1_R1_10Knwn.rda")		#	Matr_K10
Res		<- MRARegress(v$Exp, v$Pert, KnlgMap=Knlg_10)$r
dimnames(Res)	<- NULL
test_that("FRANK_30_Known_10%", {
	expect_equal (Res, Matr_K10, tolerance=1E-4)
})

load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\KnlgMap_TF30_TA0_1_R1_40Knwn.rda")		#	Knlg_40
load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\MatrCVXR_TF30_TA0_1_R1_40Knwn.rda")		#	Matr_K40
Res		<- MRARegress(v$Exp, v$Pert, KnlgMap=Knlg_40)$r
dimnames(Res)	<- NULL
test_that("FRANK_30_Known_40%", {
	expect_equal (Res, Matr_K40, tolerance=1E-4)
})


#	Test of Order2 method and ANOVA

#	Three nodes network (pRaF, ppMEK, ppERK), described in the article "Impact of measurement noise, experimental design,
#	and estimation methods on Modular Response -- Analysis based network reconstruction" C. Thomaseth et al (Scientific Reports, 2018).
#	(Thomaseth.pdf and Thomaseth_SI.pdf)

st 	<-c (0.1, 0.1, 0.1)
F3  <- function(P,X)
	c(F1 = (25*(20*P[1]-X[1])/((20+20*P[1]-X[1])*(5+X[3]))-10*X[1]/(20+X[1])), 
	  F2 = (3*(20*P[2]-X[2])*X[1]/(20+20*P[2]-X[2])-10*X[2]/(20+X[2])), 
	  F3 = ((20*P[3]-X[3])*X[2]/(20+20*P[3]-X[3])-10*X[3]/(20+X[3])))  
	  # X contains the expression levels of pRaF, ppMEK, ppERK respectively.

Perturbs			<- c(0.2, 0.9, 0.99)
nbPert				<- length(Perturbs)
P0					<- c(1, 1, 1)
Exp_3_02_09_099 	<- MExp (3, Perturbs, P0, F3, st)
load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\ResO2_3.rda")		#	ResO2_3
test_that("Order2 - 3 Genes network", {
	expect_equal (MRARegress(Exp_3_02_09_099$Exp, Exp_3_02_09_099$Pert, Method="Order2", Relative=FALSE)$Order2, ResO2_3, tolerance=1E-4)
})

nbN					<- 3												# Three nodes
nbReplic			<- 2												# Two replicates
XMesNPMoy			<- mean(abs(Exp_3_02_09_099$Exp[1, ]))				# Mean of basal values
Sd					<- 0.01*XMesNPMoy									# Standard deviation of the noise to add
MExp_				<- matrix(0, nrow=nbN, ncol=1)

set.seed(12345)
MExp_[ ,1]			<-	Exp_3_02_09_099$Exp[ ,1]
Pert_				<- c("Base")
for (iRep in 1:nbReplic) {
	MExp_3_2	<- Exp_3_02_09_099$Exp + rnorm(nbN*(nbN*nbPert+1), mean=0, sd=Sd)
	MExp_		<- cbind(MExp_, MExp_3_2[ ,2:(nbPert*nbN+1)])
	Pert_		<- c(Pert_, Exp_3_02_09_099$Pert[2:(nbPert*nbN+1)])
}
Res_		<- MRARegress(MExp_, Pert_, Relative=FALSE)

test_that("Order1 - 3 Genes network, 2 replicates - ANOVA1", {
	expect_equal (Res_$ANOVA[[1]]["SSR","F/M"], 26831.38, tolerance=1E-4)
})
test_that("Order1 - 3 Genes network, 2 replicates - ANOVA2", {
	expect_equal (Res_$ANOVA[[1]]["LOF","F/M"], 25.86601, tolerance=1E-4)
})


#	Four nodes network, described in the article "Inferring dynamic architecture of cellular networks using time series of gene expression, 
#	protein and metabolite data" Eduardo Sontag et al, 2004
#	(sontag_2004.pdf and SontagEtAl_Bioinformatics_2004_supplements.pdf)

KA14 <- 1.6			# Michaelis constants (nM)
KI12 <- 0.5
KA24 <- 1.6
KA32 <- 1.5
KI31 <- 0.7
KA43 <- 0.15
KD1	 <- 30
KD2	 <- 60
KD3	 <- 10
KD4	 <- 50

A14	 <- 4			# Dimensionless coefficient
n12	 <- 1
n14	 <- 2
A24	 <- 4
n24	 <- 2
A32	 <- 5
n31	 <- 1
n32	 <- 2
A43	 <- 2
n43	 <- 2

VS1	 <- 5			# Maximum enzyme level (nM.s-1) : "Untangling the wires ... "
VS2	 <- 3.5
VS3	 <- 3
VS4	 <- 4
VD1	 <- 200
VD2	 <- 500
VD3	 <- 150
VD4	 <- 500

VS1	 <- 1			# Maximum enzyme level (nM.Hr-1) : "Inferring dynamic architecture ... "
VS2	 <- 0.7
VS3	 <- 0.6
VS4	 <- 0.8
VD1	 <- 40
VD2	 <- 100
VD3	 <- 30
VD4	 <- 100

F4  <- function(P,X)
   c(F1 = (P[1]*(1+A14*(X[4]/KA14)^n14)) / ((1+(X[4]/KA14)^n14)*(1+(X[2]/KI12)^n12)) - (VD1*X[1]) / (KD1+X[1]),
	 F2 = (P[2]*(1+A24*(X[4]/KA24)^n24)) /  (1+(X[4]/KA24)^n24) - (VD2*X[2]) / (KD2+X[2]),
	 F3 = (P[3]*(1+A32*(X[2]/KA32)^n32)) / ((1+(X[2]/KA32)^n32)*(1+(X[1]/KI31)^n31)) - (VD3*X[3]) / (KD3+X[3]),
	 F4 = (P[4]*(1+A43*(X[3]/KA43)^n43)) /  (1+(X[3]/KA43)^n43) - (VD4*X[4]) / (KD4+X[4]))

nbN			<- 4
Perturbs	<- c(0.2, 0.9, 0.99)
P0			<- c(VS1, VS2, VS3, VS4)
st 			<- c (0.1, 0.1, 0.1, 0.1)
Exp_ 		<- MExp (nbN, Perturbs, P0, F4, st)

load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\ResO2_4.rda")		#	ResO2_4
Res_4_2		<-	MRARegress(Exp_$Exp, Exp_$Pert, Method="Order2", Relative=FALSE)
test_that("Order2 - 4 Genes network", {
	expect_equal (Res_4_2$Order2, ResO2_4, tolerance=1E-4)
})

Res_4_1		<-	MRARegress(Exp_$Exp, Exp_$Pert, Relative=FALSE)
test_that("Order1 - 4 Genes network, no replicate - ANOVA", {
	expect_equal (Res_4_1$ANOVA[[1]]["SSR","F/M"], 1029.353, tolerance=1E-4)
})



#	Six nodes network "Kinases MKK, MKKK et MAPK + bi-phosphorylated forms", described in the articles "Inferring dynamic architecture of cellular 
#	networks using time series of gene expression, protein and metabolite data" Eduardo Sontag et al, 2004 and "Untangling the wires: 
#	A strategy to trace functional interactions in signaling and gene networks" Boris N. Kholodenko et al, PNAS 2002 
#	(UntanglingTheWires.pdf), sontag_2004.pdf and SontagEtAl_Bioinformatics_2004_supplements.pdf)

KC1	 <- 1			# Catalytic rate (s-1)
KC2	 <- 15
KC5	 <- 1
KC6	 <- 15
KC9	 <- 1
KC10 <- 15

K11	 <- 300			# Michaelis constants (nM)
K12	 <- 20
K31	 <- 22
K32	 <- 18
K33	 <- 80
K51	 <- 300
K52	 <- 20
K71	 <- 22
K72	 <- 18
K73	 <- 80
K91	 <- 300
K92	 <- 20
K111 <- 22
K112 <- 18
K113 <- 80
Ki	 <- 100
Kmp	 <- 100

A	 <- 5			# Dimensionless coefficient

V3	 <- 18.8		# Maximum enzyme level (nM.s-1)
V4	 <- 16.4
V7	 <- 18.8
V8	 <- 16.4
V11	 <- 8.4
V12	 <- 7.3

MKKK0	<- 200		# X1+X7+X2	: total protein MKKK concentration
MKK0	<- 180		# X3+X8+X4	: total protein MKK  concentration
MAPK0	<- 360		# X5+X9+X6	: total protein MAPK concentration
U		<- 20

F6 	<- function(P,X)
   c(F1 = (KC1*U*X[1]) 	/ ((K11+X[1]+(MKKK0-X[1]-X[2])*K11/K12)*(1+X[6]/Ki)) - ((P[1]*(MKKK0-X[1]-X[2]))  / (K31+X[2]+(MKKK0-X[1]-X[2])*K31/K32+X[1]*K31/K33)),
	 F2 = (KC2*U*(MKKK0-X[1]-X[2]))    / ((K11+X[1]+(MKKK0-X[1]-X[2])*K11/K12)*(1+X[6]/Ki)) - ((P[2]*X[2])  / (K31+X[2]+(MKKK0-X[1]-X[2])*K31/K32+X[1]*K31/K33)),
	 F3 = (KC5*X[3]*X[2]) / (K51+X[3]+(MKK0-X[3]-X[4])*K51/K52) - ((P[3]*(MKK0-X[3]-X[4])*(1+A*X[6]/Kmp))   / ((K71+X[4]+(MKK0-X[3]-X[4])*K71/K72+X[3]*K71/K73)*(1+X[6]/Kmp))),
	 F4 = (KC6*(MKK0-X[3]-X[4])*X[2])  / (K51+X[3]+(MKK0-X[3]-X[4])*K51/K52) - ((P[4]*X[4]*(1+A*X[6]/Kmp))  / ((K71+X[4]+(MKK0-X[3]-X[4])*K71/K72+X[3]*K71/K73)*(1+X[6]/Kmp))),
	 F5 = (KC9*X[4]*X[5]) / (K91+X[5]+(MAPK0-X[5]-X[6])*K91/K92) - ((P[5]*(MAPK0-X[5]-X[6])) / (K111+X[6]+(MAPK0-X[5]-X[6])*K111/K112+X[5]*K111/K113)),
	 F6 = (KC10*X[4]*(MAPK0-X[5]-X[6]))/ (K91+X[5]*(MAPK0-X[5]-X[6])*K91/K92) - ((P[6]*X[6]) / (K111+X[6]+(MAPK0-X[5]-X[6])*K111/K112+X[5]*K111/K113)))

nbN			<- 6
Perturbs	<- c(0.2, 0.5, 0.9, 0.99, 1.5)
P0			<- c(V4, V3, V8, V7, V12, V11)
st			<- c(130,45,52,105,135,27)
Exp_		<-	MExp (nbN, Perturbs, P0, F6, st)

load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\ResO2_6.rda")		#	ResO2_6
Res_6_2		<-	MRARegress(Exp_$Exp, Exp_$Pert, Method="Order2", Relative=FALSE)
test_that("Order2 - 6 MAP kinases network", {
	expect_equal (Res_6_2$Order2, ResO2_6, tolerance=1E-4)
})

Res_6_1		<-	MRARegress(Exp_$Exp, Exp_$Pert, Relative=FALSE)
test_that("Order1 - 6 Genes network, no replicate - ANOVA", {
	expect_equal (Res_6_1$ANOVA[[1]]["SSR","F/M"], 76431.25, tolerance=1E-4)
})


# Comparer à Solution6K.rda avec les différentes méthodes et calculer la distance (absolu, relatif)
# MonDoc paragraphe 3.4.3, l478
 
#
#	Réseau 3 noeuds (Thomaseth)  -- tests Ordre 2 présentation Patrice
# save (Pat3nds_O1, file="C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\Pat3nds_O1.rda")		#	Réseau 3 noeuds (Thomaseth), O1, Pert= -0.001
load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\Pat3nds_O1.rda")		# Pat3nds_O1
load("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\Pat3nds_O2.rda")		# Pat3nds_O2

MatExpJP_3	<- matrix(c(2.133728, 6.819618, 7.235918,   2.132878, 6.817047, 7.233394,  2.134409, 6.815775, 7.232144,  2.134481, 6.821894, 7.231744), nrow=3)
Res			<- MRARegress(MatExpJP_3, Relative=FALSE)$r
dimnames(Res)	<- NULL
test_that("Order1 - Thomaseth 3 nodes network", {
	expect_equal (Res, Pat3nds_O1, tolerance=1E-4)
})

MExpPat_10	<- matrix(c(2.133728,6.819618,7.235918,2.214443,7.061827,7.471919,2.071154,7.187810,7.593163,2.064243,6.608409,7.633927,
						2.043932,6.546210,6.965091,2.208236,6.418148,6.836690,2.215755,7.065726,6.797877,
						2.002126,6.950560,8.012930,2.147944,7.449084,7.841187,2.139687,6.837609,7.888627), nrow=3)
ExperPat	<- matrix(c(1,0,0, 0,1,0, 0,0,1, -1,0,0, 0,-1,0, 0,0,-1, 0,1,1, 1,1,0, 1,0,1), nrow=3)		#	Réseau 3 noeuds (Thomaseth), O1, Pert= plan expé Patrice 10%
Res			<- MRARegress(MExpPat_10, MapExper=ExperPat, Relative=FALSE)$r
dimnames(Res)	<- NULL
test_that("Order1 - Thomaseth 3 nodes network Plan Expé Patrice", {
	expect_equal (Res, Pat3nds_O1, tolerance=1E-2)
})


Res			<- MRARegress(MExpPat_10, MapExper=ExperPat, Method="Order2", Relative=FALSE)$r
dimnames(Res)	<- NULL
test_that("Order2 - Thomaseth 3 nodes network Plan Expé Patrice", {
	expect_equal (Res, Pat3nds_O1, tolerance=1E-3)
})

# save (Pat3nds_O2, file="C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\Pat3nds_O2.rda")		#	Réseau 3 noeuds (Thomaseth), O2, Pert= 10%
test_that("Order2 - Thomaseth 3 nodes network Plan Expé Patrice", {
	expect_equal (MRARegress(MExpPat_10, MapExper=ExperPat, Method="Order2", Relative=FALSE)$Order2, Pat3nds_O2, tolerance=1E-4)
})


#	Four nodes network, described in the article "aiMeRA: A generic modular response analysis R package and its application to estrogen and
#	retinoic acid receptors crosstalk", Gabriel Jimenez-Dominguez et al, bioRxiv, 31/01/2020
#	(aiMeRA_Manual.pdf)
load ("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRARegress\\data-raw\\Jimenez_Fig3.rda")		# data
Nodes <- c('LCOR', 'RIP140', 'Hoxa5', 'Luciferase')
Pert  <- c('Base', 'E2+RA+siLCoR->LCOR', 'E2+RA+siRIP140->RIP140', 'E2->Hoxa5', 'RA->Luciferase')					# Base = 'E2+RA'
Data1 <- data$mean[c('LCoR','RIP140','Hoxa5','Luciferase'), ]
MatExp_Jim 	<- Data1[ ,c('E2+RA', 'E2+RA+siLCoR', 'E2+RA+siRIP140', 'E2', 'RA')]
Expected 	<- matrix(c(-1,-2.67,0.54,0.29, 0.05,-1,-1.47,-0.99, 0.04,3,-1,-0.07, -0.25,1.98,-0.68,-1), nrow=4)
Res	  <- round(MRARegress(MatExp_Jim, Pert, Nodes)$r,2)
dimnames(Res)	<- NULL
test_that("Réseau Jimenez", {
	expect_equal (Res,Expected, tolerance=1E-9)
})


#	Another solution for this network
load ("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRARegress\\data-raw\\Demo_4nds.rda")			# Demo : MatExp matrix
Nodes <- c('LCOR', 'RIP140', 'Hoxa5', 'Luciferase')
Pert  <- c('Base', 'E2+RA+siLCoR->LCOR', 'E2+RA+siRIP140->RIP140', 'E2->Hoxa5', 'RA->Luciferase')					# Base = 'E2+RA'
Expected 	<- matrix(c(-1,-2.67,0.54,0.29, 0.05,-1,-1.47,-0.99, 0.04,3,-1,-0.07, -0.25,1.98,-0.68,-1), nrow=4)
Res	  <- round(MRARegress(Demo, Pert, Nodes)$r,2)
dimnames(Res)	<- NULL
test_that("Réseau Jimenez", {
	expect_equal (Res,Expected, tolerance=1E-9)
})



#	Test rows'names and cols'names of MatrCc
Nodes1		<- c("Nod1", "Nod2", "Nod3")
MExt1		<- matrix(c(1,1,1, 1,0,0, 0,2,0, 0,0,3), nrow=3)
Expected	<- matrix(c(-1,1.5,4/3, 1.8,-1,1, 1.6,1,-1), nrow=3)
rownames(Expected)	<- Nodes1
colnames(Expected)	<- c("PNod1", "PNod2", "PNod3")

#	Pertubations are indicated in the correct order
Perts1		<- c("Base", "PNod1->Nod1", "PNod2->Nod2", "PNod3->Nod3")
Res			<- MRARegress(MExt1, Perts1, Nodes1)$r
test_that("Perturbations are indicated in the correct order", {
  expect_equal (Res, Expected, tolerance=1E-4)	
})


# Base at the end
MExt1b		<- matrix(c(1,0,0, 0,2,0, 0,0,3, 1,1,1), nrow=3)
Perts1b		<- c("PNod1->Nod1", "PNod2->Nod2", "PNod3->Nod3","Base")
Res			<- MRARegress(MExt1b, Perts1b, Nodes1)$r
test_that("Base at the end", {
  expect_equal (Res, Expected, tolerance=1E-4)	
})


#	Pertubations are indicated in disorder
MExt2		<- matrix(c(1,1,1, 0,2,0,  1,0,0, 0,0,3), nrow=3)
Perts2		<- c("Base", "PNod2->Nod2", "PNod1->Nod1", "PNod3->Nod3")
Res			<- MRARegress(MExt2, Perts2, Nodes1)$r
test_that("Pertubations are indicated in disorder", {
  expect_equal (Res, Expected, tolerance=1E-4)	
})

#	MExt and perturbations don't match
Res			<- MRARegress(MExt2, Perts1, Nodes1)$r
test_that("MExt and perturbations don't match", {
  expect_false (isTRUE(all.equal(Res, Expected)))
})

#	Base and pertubations are indicated in disorder
MExt3		<- matrix(c(0,0,3, 0,2,0, 1,0,0, 1,1,1), nrow=3)
Perts3		<- c("PNod3->Nod3", "PNod2->Nod2", "PNod1->Nod1", "Base")
Res			<- MRARegress(MExt3, Perts3, Nodes1)$r
test_that("Base and pertubations are indicated in disorder", {
  expect_equal (Res, Expected, tolerance=1E-4)	
})

#	Many pertubations
MExt4		<- matrix(c(1,1,1, 1,0,0, 0,2,0, 0,0,3, 0,0,3.1), nrow=3)
Expected	<- matrix(c(-1,1.506060,4/3, 1.81162,-1,1, 1.6037298,0.9999256,-1), nrow=3)
rownames(Expected)	<- Nodes1
colnames(Expected)	<- c("PNod1", "PNod2", "PNod3; P2Nod3")

Perts4		<- c("Base", "PNod1->Nod1", "PNod2->Nod2", "PNod3->Nod3", "P2Nod3->Nod3")
Res			<- MRARegress(MExt4, Perts4, Nodes1)$r
test_that("Many perturbations -1", {
  expect_equal (Res, Expected, tolerance=1E-4)	
})

MExt5		<- matrix(c(0,0,3, 0,2,0, 1,0,0, 1,1,1, 0,0,3.1), nrow=3)
Perts5		<- c("PNod3->Nod3", "PNod2->Nod2", "PNod1->Nod1", "Base", "P2Nod3->Nod3")
Res			<- MRARegress(MExt5, Perts5, Nodes1)$r
test_that("Many perturbations -2", {
  expect_equal (Res, Expected, tolerance=1E-4)	
})
