#
#	Score unit tests
#

#	rm(list = ls())

#	PART ONE : check input data

MatExp		<- matrix(c(1,2,3, 11,21,31, 12,22,32, 13,23,33),nrow=3)	# 3 nodes, 3 perturbations, Base : 1st column
Res			<- MRARegress (MatExp, Relative=FALSE)
ResC		<- Classify(Res)

ResC1		<- ResC
test_that("Missing Parameter", {
  expect_null (Score(ResC1))										# Parameter missing
})

ResC1$Input$Classes	<- NULL
test_that("Incorrect Ret -1", {
  expect_null (Score(Ret1, MatExp))									# Data missing in the list
})

MatExp0		<- MatExp[ ,-1]
MatExp0[2,1]<- 'A'
test_that("Data type differents -3", {
  expect_null (Score(MatExp0, MatExp, c(0,1)))						# Data type different
})

Dig			<- matrix(c(0,0,1, 1,1,0, 1,2,0), nrow=3)
Ref			<- matrix(c(0,0,1, 1,1,0, 1,1,0), nrow=3)
test_that("Bad value -1", {
  expect_null (Score(Dig, Ref, Classes=c(0,1)))						# Value not in Classes
})

Dig			<- matrix(c("F","F","T", "T","T","F", "T","TRUE","F"), nrow=3)
Ref			<- matrix(c("F","F","T", "T","T","F", "T","T","F"), nrow=3)
test_that("Bad value -2", {
  expect_null (Score(Dig, Ref, Classes=c("T","F")))					# Value not in Classes
})


#
#	PART TWO : Check Confusion Matrix
#

#	Two classes

Ref		<- matrix(c(1,0,0, 1,1,0, 0,1,1), nrow=3)
Dig		<- matrix(c(1,0,0, 0,1,0, 0,0,1), nrow=3)
Res		<- Score (Dig, Ref, Classes=c(0,1))
Discovered	<- matrix (c(4,2, 0,3), nrow=2)
rownames(Discovered)	<- c(0,1)
colnames(Discovered)	<- c(0,1)
test_that("Simple_1", {
	expect_equal(Res$ConfMat, Discovered, tolerance=1E-9)
})
test_that("Simple_2", {
	expect_equal(Res$Scores2$Se, 0.6, tolerance=1E-9)
})
test_that("Simple_3", {
	expect_equal(Res$Scores2$Sp, 1.0, tolerance=1E-9)
})


Ref		<- matrix(c(1,0,0, 1,1,0, 0,1,1), nrow=3)
Dig		<- matrix(c(1,0,0, 0,1,0, 0,0,1), nrow=3)
Res		<- Score (Dig, Ref, Classes=c(1,0))
test_that("Inverted_2", {
	expect_equal(Res$Scores2$Se, 0.6, tolerance=1E-9)
})
test_that("Inverted_3", {
	expect_equal(Res$Scores2$Sp, 1.0, tolerance=1E-9)
})


set.seed (12345)
A		<- sample (c(0,1), 9, replace=TRUE)
Ref		<- matrix(A, nrow=3)
Dig		<- Ref
diag(Dig)	<- 1
Res		<- Score (Dig, Ref, Classes=c(0,1))
dimnames(Res$ConfMat)	<- NULL
Discovered	<- matrix (c(2,0, 1,6), nrow=2)
test_that("2Classes_1", {
	expect_equal(Res$ConfMat, Discovered, tolerance=1E-9)
})


set.seed (12345)
A		<- sample (c(0,1), 36, replace=TRUE)
Ref		<- matrix(A, nrow=6)
Dig		<- Ref
B		<- sample (1:36, 10)
Dig[B]	<- 1-Dig[B]
Res		<- Score (Dig, Ref, Classes=c(0,1))
dimnames(Res$ConfMat)	<- NULL
Discovered	<- matrix (c(5,4, 6,21), nrow=2)
test_that("2Classes_2", {
	expect_equal(Res$ConfMat, Discovered, tolerance=1E-9)
})


set.seed (12345)
A		<- sample (c('T','F'), 36, replace=TRUE)
Ref		<- matrix(A, nrow=6)
Dig		<- Ref
B		<- sample (1:36, 10)
for (i in B) {
	if (Dig[i] =='T')
		Dig[i] = 'F'
	else
		Dig[i] = 'T'
}
Res		<- Score (Dig, Ref, Classes=c('T','F'))
dimnames(Res$ConfMat)	<- NULL
Discovered	<- matrix (c(5,4, 6,21), nrow=2)
test_that("2Classes_3", {
	expect_equal(Res$ConfMat, Discovered, tolerance=1E-9)
})


set.seed (12345)
A		<- sample (c(FALSE,TRUE), 36, replace=TRUE)
Ref		<- matrix(A, nrow=6)
Dig		<- Ref
B		<- sample (1:36, 10)
for (i in B) {
	if (Dig[i] == TRUE)
		Dig[i] = FALSE
	else
		Dig[i] = TRUE
}
Res		<- Score (Dig, Ref, Classes=c(FALSE,TRUE))
dimnames(Res$ConfMat)	<- NULL
Discovered	<- matrix (c(5,4, 6,21), nrow=2)
test_that("2Classes_4", {
	expect_equal(Res$ConfMat, Discovered, tolerance=1E-9)
})


#	Three classes or more

set.seed (12345)
A		<- sample (c(-1, 0, 1), 36, replace=TRUE)
Dig		<- matrix(A, nrow=6)
B		<- sample (c(-1, 0, 1), 36, replace=TRUE)
Ref		<- matrix(B, nrow=6)
Res		<- Score (Dig, Ref, Classes=c(-1, 0, 1))

test_that("3Classes_1", {
	expect_equal(Res$Scores$NR, c(9,9,18), tolerance=1E-9)
})
test_that("3Classes_2", {
	expect_equal(Res$Scores$ND, c(9,16,11), tolerance=1E-9)
})


Dig		<- Ref
diag(Dig) <- -1
Res		<-	Score (Dig, Ref, Classes=c(-1, 0, 1))
dimnames(Res$ConfMat) <- NULL
Expected	<-	matrix(c(9,0,4, 0,9,0, 0,0,14), nrow=3)
test_that("3 Classes_3", {
	expect_equal (Res$ConfMat, Expected, tolerance=1E-9)
})


set.seed (12345)
A		<- sample (c('a','b','c','d','e'), 100, replace=TRUE)
Ref		<- matrix(A, nrow=10)
Dig		<- Ref
diag(Dig) <- 'a'
Res		<-	Score (Dig, Ref, Classes=c('a','b','c','d','e'))
dimnames(Res$ConfMat) <- NULL
Expected	<-	matrix(c(19,1,4,3,1, 0,24,0,0,0, 0,0,18,0,0, 0,0,0,19,0, 0,0,0,0,11), nrow=5)
test_that("5 Classes", {
	expect_equal (Res$ConfMat, Expected, tolerance=1E-9)
})


#
#	Tests "Dream Challenge 4"
#
rm(list = ls())

load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Perturb_10.rda")			# Perturb_10

load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/MatExp_10_1.rda")		# MatExp
load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Solution_10_1.rda")		# Solution

#	load ("C:/Users/jean-pierre.borg/IRCM/Tests/Tests_R/DC4/MatExp_10_1.rda")			# MatExp
#	load ("C:/Users/jean-pierre.borg/IRCM/Tests/Tests_R/DC4/Solution_10_1.rda")			# Solution

Res	 	<- MRARegress(MatExp, Perturb_10)
ResC	<- Classify(Res)
Sc		<- Score(ResC, Solution)
test_that("InSilico_10_1", {
	expect_equal (c(Sc$Scores2_de$Se, Sc$Scores2_de$Sp, Sc$Scores2_de$Dst), c(0.8, 0.92, 0.72), tolerance=1E-3)
})

set.seed(12345)
Res	 	<- MRARegress(MatExp, Perturb_10, Method="Random Forest")
ResC	<- Classify(Res)
Sc		<- Score(ResC, Solution)
test_that("InSilico_10_1 RF", {
	expect_equal (c(Sc$Scores2_de$Se, Sc$Scores2_de$Sp, Sc$Scores2_de$Dst), c(0.667, 0.867, 0.533), tolerance=1E-3)
})
cat ("Se", Sc$Scores2_de$Se, "Sp", Sc$Scores2_de$Sp, "Dst", Sc$Scores2_de$Dst, "\n")					# Se 0.6666667 Sp 0.8666667 Dst 0.5333333 


load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/MatExp_10_2.rda")		# MatExp
load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Solution_10_2.rda")		# Solution
Res	 	<- MRARegress(MatExp, Perturb_10)
ResC	<- Classify(Res)
Sc		<- Score(ResC, Solution)
test_that("InSilico_10_2", {
	expect_equal (c(Sc$Scores2_de$Se, Sc$Scores2_de$Sp, Sc$Scores2_de$Dst), c(0.375, 0.892, 0.267), tolerance=1E-3)
})

set.seed(12345)
Res	 	<- MRARegress(MatExp, Perturb_10, Method="Random Forest")
ResC	<- Classify(Res)
Sc		<- Score(ResC, Solution)
test_that("InSilico_10_2 RF", {
	expect_equal (c(Sc$Scores2_de$Se, Sc$Scores2_de$Sp, Sc$Scores2_de$Dst), c(0.25, 0.784, 0.034), tolerance=1E-3)
})
cat ("Se", Sc$Scores2_de$Se, "Sp", Sc$Scores2_de$Sp, "Dst", Sc$Scores2_de$Dst, "\n")					# Se 0.25 Sp 0.7837838 Dst 0.03378378


load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/MatExp_10_3.rda")		# MatExp
load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Solution_10_3.rda")		# Solution
Res	 	<- MRARegress(MatExp, Perturb_10)
ResC	<- Classify(Res)
Sc		<- Score(ResC, Solution)
test_that("InSilico_10_3", {
	expect_equal (c(Sc$Scores2_de$Se, Sc$Scores2_de$Sp, Sc$Scores2_de$Dst), c(0.667, 0.787, 0.454), tolerance=1E-3)
})

set.seed(12345)
Res	 	<- MRARegress(MatExp, Perturb_10, Method="Random Forest")
ResC	<- Classify(Res)
Sc		<- Score(ResC, Solution)
test_that("InSilico_10_3 RF", {
	expect_equal (c(Sc$Scores2_de$Se, Sc$Scores2_de$Sp, Sc$Scores2_de$Dst), c(0.467, 0.827, 0.293), tolerance=1E-3)
})
cat ("Se", Sc$Scores2_de$Se, "Sp", Sc$Scores2_de$Sp, "Dst", Sc$Scores2_de$Dst, "\n")					# Se 0.4666667 Sp 0.8266667 Dst 0.2933333


load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/MatExp_10_4.rda")		# MatExp
load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Solution_10_4.rda")		# Solution
Res	 	<- MRARegress(MatExp, Perturb_10)
ResC	<- Classify(Res)
Sc		<- Score(ResC, Solution)
test_that("InSilico_10_4", {
	expect_equal (c(Sc$Scores2_de$Se, Sc$Scores2_de$Sp, Sc$Scores2_de$Dst), c(0.692, 0.818, 0.51), tolerance=1E-3)
})

set.seed(12345)
Res	 	<- MRARegress(MatExp, Perturb_10, Method="Random Forest")
ResC	<- Classify(Res)
Sc		<- Score(ResC, Solution)
test_that("InSilico_10_4 RF", {
	expect_equal (c(Sc$Scores2_de$Se, Sc$Scores2_de$Sp, Sc$Scores2_de$Dst), c(0.538, 0.831, 0.370), tolerance=1E-3)
})
cat ("Se", Sc$Scores2_de$Se, "Sp", Sc$Scores2_de$Sp, "Dst", Sc$Scores2_de$Dst, "\n")					# Se 0.5384615 Sp 0.8311688 Dst 0.3696304


load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/MatExp_10_5.rda")		# MatExp
load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Solution_10_5.rda")		# Solution
Res	 	<- MRARegress(MatExp, Perturb_10)
ResC	<- Classify(Res)
Sc		<- Score(ResC, Solution)
test_that("InSilico_10_2", {
	expect_equal (c(Sc$Scores2_de$Se, Sc$Scores2_de$Sp, Sc$Scores2_de$Dst), c(0.500, 0.718, 0.218), tolerance=1E-3)
})

set.seed(12345)
Res	 	<- MRARegress(MatExp, Perturb_10, Method="Random Forest")
ResC	<- Classify(Res)
Sc		<- Score(ResC, Solution)
test_that("InSilico_10_5 RF", {
	expect_equal (c(Sc$Scores2_de$Se, Sc$Scores2_de$Sp, Sc$Scores2_de$Dst), c(0.500, 0.821, 0.321), tolerance=1E-3)
})
cat ("Se", Sc$Scores2_de$Se, "Sp", Sc$Scores2_de$Sp, "Dst", Sc$Scores2_de$Dst, "\n")					# Se 0.5 Sp 0.8205128 Dst 0.3205128


load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Perturb_100.rda")		# Perturb_100

load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/MatExp_100_1.rda")		# MatExp
load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Solution_100_1.rda")		# Solution

Res	 	<- MRARegress(MatExp, Perturb_100)
ResC	<- Classify(Res)
Sc		<- Score(ResC, Solution)
test_that("InSilico_100_1", {
	expect_equal (c(Sc$Scores2_de$Se, Sc$Scores2_de$Sp, Sc$Scores2_de$Dst), c(0.233, 0.985, 0.218), tolerance=1E-3)
})

set.seed(12345)
Res	 	<- MRARegress(MatExp, Perturb_100, Method="Random Forest")
ResC	<- Classify(Res)
Sc		<- Score(ResC, Solution)
test_that("InSilico_100_1 RF", {
	expect_equal (c(Sc$Scores2_de$Se, Sc$Scores2_de$Sp, Sc$Scores2_de$Dst), c(0.489, 0.803, 0.292), tolerance=1E-3)
})
cat ("Se", Sc$Scores2_de$Se, "Sp", Sc$Scores2_de$Sp, "Dst", Sc$Scores2_de$Dst, "\n")					# Se 0.4886364 Sp 0.8031674 Dst 0.2918038 


load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/MatExp_100_2.rda")		# MatExp
load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Solution_100_2.rda")		# Solution
Res	 	<- MRARegress(MatExp, Perturb_100)
ResC	<- Classify(Res)
Sc		<- Score(ResC, Solution)
test_that("InSilico_100_2", {
	expect_equal (c(Sc$Scores2_de$Se, Sc$Scores2_de$Sp, Sc$Scores2_de$Dst), c(0.229, 0.978, 0.207), tolerance=1E-3)
})

#	set.seed(12345)
#	Res	 	<- MRARegress(MatExp, Perturb_100, Method="Random Forest")
#	ResC	<- Classify(Res)
#	Sc		<- Score(ResC, Solution)
#	test_that("InSilico_100_2 RF", {
#		expect_equal (c(Sc$Scores2_de$Se, Sc$Scores2_de$Sp, Sc$Scores2_de$Dst), c(0.486, 0.805, 0.291), tolerance=1E-3)
#	})
#	cat ("Se", Sc$Scores2_de$Se, "Sp", Sc$Scores2_de$Sp, "Dst", Sc$Scores2_de$Dst, "\n")					# Se 0.4859438 Sp 0.8053051 Dst 0.2912489


load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/MatExp_100_3.rda")		# MatExp
load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Solution_100_3.rda")		# Solution
Res	 	<- MRARegress(MatExp, Perturb_100)
ResC	<- Classify(Res)
Sc		<- Score(ResC, Solution)
test_that("InSilico_100_3", {
	expect_equal (c(Sc$Scores2_de$Se, Sc$Scores2_de$Sp, Sc$Scores2_de$Dst), c(0.303, 0.960, 0.263), tolerance=1E-3)
})

#	set.seed(12345)
#	Res	 	<- MRARegress(MatExp, Perturb_100, Method="Random Forest")
#	ResC	<- Classify(Res)
#	Sc		<- Score(ResC, Solution)
#	test_that("InSilico_100_3 RF", {
#		expect_equal (c(Sc$Scores2_de$Se, Sc$Scores2_de$Sp, Sc$Scores2_de$Dst), c(0.533, 0.805, 0.338), tolerance=1E-3)
#	})
#	cat ("Se", Sc$Scores2_de$Se, "Sp", Sc$Scores2_de$Sp, "Dst", Sc$Scores2_de$Dst, "\n")					# Se 0.5333333 Sp 0.8046368 Dst 0.3379701


load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/MatExp_100_4.rda")		# MatExp
load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Solution_100_4.rda")		# Solution
Res	 	<- MRARegress(MatExp, Perturb_100)
ResC	<- Classify(Res)
Sc		<- Score(ResC, Solution)
test_that("InSilico_100_4", {
	expect_equal (c(Sc$Scores2_de$Se, Sc$Scores2_de$Sp, Sc$Scores2_de$Dst), c(0.346, 0.959, 0.305), tolerance=1E-3)
})

#	set.seed(12345)
#	Res	 	<- MRARegress(MatExp, Perturb_100, Method="Random Forest")
#	ResC	<- Classify(Res)
#	Sc		<- Score(ResC, Solution)
#	test_that("InSilico_100_4 RF", {
#		expect_equal (c(Sc$Scores2_de$Se, Sc$Scores2_de$Sp, Sc$Scores2_de$Dst), c(0.540, 0.805, 0.346), tolerance=1E-3)
#	})
#	cat ("Se", Sc$Scores2_de$Se, "Sp", Sc$Scores2_de$Sp, "Dst", Sc$Scores2_de$Dst, "\n")					# Se 0.5402844 Sp 0.8053463 Dst 0.3456306


load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/MatExp_100_5.rda")		# MatExp
load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Solution_100_5.rda")		# Solution
Res	 	<- MRARegress(MatExp, Perturb_100)
ResC	<- Classify(Res)
Sc		<- Score(ResC, Solution)
test_that("InSilico_100_2", {
	expect_equal (c(Sc$Scores2_de$Se, Sc$Scores2_de$Sp, Sc$Scores2_de$Dst), c(0.207, 0.964, 0.171), tolerance=1E-3)
})

#	set.seed(12345)
#	Res	 	<- MRARegress(MatExp, Perturb_100, Method="Random Forest")
#	ResC	<- Classify(Res)
#	Sc		<- Score(ResC, Solution)
#	test_that("InSilico_100_5 RF", {
#		expect_equal (c(Sc$Scores2_de$Se, Sc$Scores2_de$Sp, Sc$Scores2_de$Dst), c(0.585, 0.806, 0.391), tolerance=1E-3)
#	})
#	cat ("Se", Sc$Scores2_de$Se, "Sp", Sc$Scores2_de$Sp, "Dst", Sc$Scores2_de$Dst, "\n")					# Se 0.5854922 Sp 0.8056042 Dst 0.3910964


#
#		Tests "FRANK Networks"
#
#	vRoot		<- "C:/Users/jean-pierre.borg/IRCM/Publications/BioInformatics/MRA_Regression_ed1/Vers_Rev1/Donnees/FRANK2/Donnees/"
#	Solution 	<- as.matrix(read.table(paste(vRoot, "Frank_TF30_TA30_1_Sol.csv", sep="")))
#
library ("MRARegress")
nbK		<- 2			# KO, KD50

nbN		<- 2*30			# Number of nodes
MatDif	<- matrix(0, nrow=2, ncol=nbN*nbN)

load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Frank_TF30_TA30_1_Sol.rda")	# Solution
load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Frank_TF30_TA30_1_R1.rda")	# MatRN2,   k=0.1
A		<-	MatR2MatExp	(MatRN2, nbN, nbK)
MatrCc	<-	MRARegress (A$Exp, A$Pert)$r					# Mode : 'Relative', Method : 'TLR', Parameter : 0.25	(default values)
MatDif[1, ]	<- Solution
MatDif[2, ]	<- MatrCc
dist(MatDif)			# 0.3285881

load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Frank_TF30_TA30_2_Sol.rda")	# Solution
load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Frank_TF30_TA30_2_R1.rda")	# MatRN2,   k=0.1
A		<-	MatR2MatExp	(MatRN2, nbN, nbK)
MatrCc	<-	MRARegress (A$Exp, A$Pert)$r					# Mode : 'Relative', Method : 'TLR', Parameter : 0.25	(default values)
MatDif[1, ]	<- Solution
MatDif[2, ]	<- MatrCc
dist(MatDif)			# 0.6141877

load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Frank_TF30_TA30_3_Sol.rda")	# Solution
load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Frank_TF30_TA30_3_R1.rda")	# MatRN2,   k=0.1
A		<-	MatR2MatExp	(MatRN2, nbN, nbK)
MatrCc	<-	MRARegress (A$Exp, A$Pert)$r					# Mode : 'Relative', Method : 'TLR', Parameter : 0.25	(default values)
MatDif[1, ]	<- Solution
MatDif[2, ]	<- MatrCc
dist(MatDif)			# 0.8553274

load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Frank_TF30_TA30_4_Sol.rda")	# Solution
load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Frank_TF30_TA30_4_R1.rda")	# MatRN2,   k=0.1
A		<-	MatR2MatExp	(MatRN2, nbN, nbK)
MatrCc	<-	MRARegress (A$Exp, A$Pert)$r					# Mode : 'Relative', Method : 'TLR', Parameter : 0.25	(default values)
MatDif[1, ]	<- Solution
MatDif[2, ]	<- MatrCc
dist(MatDif)			# 0.3880909

load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Frank_TF30_TA30_5_Sol.rda")	# Solution
load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Frank_TF30_TA30_5_R1.rda")	# MatRN2,   k=0.1
A		<-	MatR2MatExp	(MatRN2, nbN, nbK)
MatrCc	<-	MRARegress (A$Exp, A$Pert)$r					# Mode : 'Relative', Method : 'TLR', Parameter : 0.25	(default values)
MatDif[1, ]	<- Solution
MatDif[2, ]	<- MatrCc
dist(MatDif)			# 0.4655604

mean (c(0.3285881, 0.6141877, 0.8553274, 0.3880909, 0.4655604))		# 0.5303509
sd	 (c(0.3285881, 0.6141877, 0.8553274, 0.3880909, 0.4655604))		# 0.2108212

#
#	Knowledge introduction : columns nbN/2 +1 to 2*nbN are set to 0  (TA nodes don't act on any node)
#
Knlg	<- matrix ("x", nrow=nbN, ncol=nbN)
Knlg[ , (nbN/2 +1):nbN]	 <- 0

load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Frank_TF30_TA30_1_Sol.rda")	# Solution
load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Frank_TF30_TA30_1_R1.rda")	# MatRN2,   k=0.1
A		<-	MatR2MatExp	(MatRN2, nbN, nbK)
MatrCc	<-	MRARegress (A$Exp, A$Pert, KnlgMap=Knlg)$r					# Mode : 'Relative', Method : 'TLR', Parameter : 0.25	(default values)
MatDif[1, ]	<- Solution
MatDif[2, ]	<- MatrCc
dist(MatDif)			# 0.2427032

load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Frank_TF30_TA30_2_Sol.rda")	# Solution
load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Frank_TF30_TA30_2_R1.rda")	# MatRN2,   k=0.1
A		<-	MatR2MatExp	(MatRN2, nbN, nbK)
MatrCc	<-	MRARegress (A$Exp, A$Pert, KnlgMap=Knlg)$r					# Mode : 'Relative', Method : 'TLR', Parameter : 0.25	(default values)
MatDif[1, ]	<- Solution
MatDif[2, ]	<- MatrCc
dist(MatDif)			# 0.4567236

load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Frank_TF30_TA30_3_Sol.rda")	# Solution
load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Frank_TF30_TA30_3_R1.rda")	# MatRN2,   k=0.1
A		<-	MatR2MatExp	(MatRN2, nbN, nbK)
MatrCc	<-	MRARegress (A$Exp, A$Pert, KnlgMap=Knlg)$r					# Mode : 'Relative', Method : 'TLR', Parameter : 0.25	(default values)
MatDif[1, ]	<- Solution
MatDif[2, ]	<- MatrCc
dist(MatDif)			# 0.637722

load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Frank_TF30_TA30_4_Sol.rda")	# Solution
load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Frank_TF30_TA30_4_R1.rda")	# MatRN2,   k=0.1
A		<-	MatR2MatExp	(MatRN2, nbN, nbK)
MatrCc	<-	MRARegress (A$Exp, A$Pert, KnlgMap=Knlg)$r					# Mode : 'Relative', Method : 'TLR', Parameter : 0.25	(default values)
MatDif[1, ]	<- Solution
MatDif[2, ]	<- MatrCc
dist(MatDif)			# 0.2969855

load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Frank_TF30_TA30_5_Sol.rda")	# Solution
load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Frank_TF30_TA30_5_R1.rda")	# MatRN2,   k=0.1
A		<-	MatR2MatExp	(MatRN2, nbN, nbK)
MatrCc	<-	MRARegress (A$Exp, A$Pert, KnlgMap=Knlg)$r					# Mode : 'Relative', Method : 'TLR', Parameter : 0.25	(default values)
MatDif[1, ]	<- Solution
MatDif[2, ]	<- MatrCc
dist(MatDif)			# 0.3482446

mean (c(0.2427032, 0.4567236, 0.637722,  0.2969855, 0.3482446))		# 0.3964758 vs. 0.5303509
sd	 (c(0.2427032, 0.4567236, 0.637722,  0.2969855, 0.3482446))		# 0.1562851 vs. 0.2108212

#
#	Contribution of knowledge if the noise level is stronger
#
load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Frank_TF30_TA30_1_Sol.rda")	# Solution
load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Frank_TF30_TA30_1_R2.rda")	# MatRN2,   k=0.5
A		<-	MatR2MatExp	(MatRN2, nbN, nbK)
MatrCc	<-	MRARegress (A$Exp, A$Pert)$r						# Mode : 'Relative', Method : 'TLR', Parameter : 0.25	(default values)
MatDif[1, ]	<- Solution
MatDif[2, ]	<- MatrCc
dist(MatDif)			# 1.636543

MatrCc	<-	MRARegress (A$Exp, A$Pert, KnlgMap=Knlg)$r			# Mode : 'Relative', Method : 'TLR', Parameter : 0.25	(default values)
MatDif[1, ]	<- Solution
MatDif[2, ]	<- MatrCc
dist(MatDif)			# 1.192742 vs. 1.636543

#
#	Contribution of knowledge if the number of nodes is larger
#
nbN		<- 2*100			# Number of nodes
MatDif	<- matrix(0, nrow=2, ncol=nbN*nbN)
Knlg	<- matrix ("x", nrow=nbN, ncol=nbN)
Knlg[ , (nbN/2 +1):nbN]	 <- 0

load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Frank_TF100_TA100_1_Sol.rda")	# Solution
load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Frank_TF100_TA100_1_R1.rda")		# MatRN2,   k=0.1
A		<-	MatR2MatExp	(MatRN2, nbN, nbK)
MatrCc	<-	MRARegress (A$Exp, A$Pert)$r						# Mode : 'Relative', Method : 'TLR', Parameter : 0.25	(default values)
MatDif[1, ]	<- Solution
MatDif[2, ]	<- MatrCc
dist(MatDif)			# 1.195933

MatrCc	<-	MRARegress (A$Exp, A$Pert, KnlgMap=Knlg)$r			# Mode : 'Relative', Method : 'TLR', Parameter : 0.25	(default values)
MatDif[1, ]	<- Solution
MatDif[2, ]	<- MatrCc
dist(MatDif)			# 0.9045669

load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Frank_TF100_TA100_1_R2.rda")		# MatRN2,   k=0.5
A		<-	MatR2MatExp	(MatRN2, nbN, nbK)
MatrCc	<-	MRARegress (A$Exp, A$Pert)$r						# Mode : 'Relative', Method : 'TLR', Parameter : 0.25	(default values)
MatDif[1, ]	<- Solution
MatDif[2, ]	<- MatrCc
dist(MatDif)			# 5.824652

MatrCc	<-	MRARegress (A$Exp, A$Pert, KnlgMap=Knlg)$r			# Mode : 'Relative', Method : 'TLR', Parameter : 0.25	(default values)
MatDif[1, ]	<- Solution
MatDif[2, ]	<- MatrCc
dist(MatDif)			# 4.406507

#	https://orcid.org/0009-0007-6193-1019
