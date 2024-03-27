#
#	Classify unit tests
#

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
	 

nbN=6
Perturbs	<- 	1.5				# utilisé dans l'article BioInfo										#  c(0.2, 0.5, 0.9, 0.99, 1.5)
P0			<- c(V4, V3, V8, V7, V12, V11)
St			<- c(130,45,52,105,135,27)		

MatExp	<- MExp (nbN, Perturbs, P0, F6, St)$Exp
Res		<- MRARegress (MatExp)
NomSol	<- "C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Solution_6K.rda"
RootW	<- "C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/Temporaire"


#	PART ONE : check input data

Ret			<- MRARegress (MatExp, Relative=FALSE)

Ret1		<- Ret
Ret1$Input$Variables	<- NULL
cat ("rCI - Incorrect Ret -1\n")
test_that("Incorrect Ret -1", {
  expect_null (rCI (Ret1, F6, St, P0, Perturbs, 2, 10, 0.05))		# Bad return of MRARegress
})

Ret2		<- Ret
KeysR		<- Ret$Input$Variables$Keys								# Keys read
KeysR[3]	<- KeysR[3] +1	
Ret2$Input$Variables$Keys	<- KeysR
cat ("rCI - Incorrect Ret -2\n")
test_that("Incorrect Ret -2", {
  expect_null (rCI (Ret2, F6, St, P0, Perturbs, 2, 10, 0.05))		# Bad signature
})

cat ("rCI - Incorrect Func\n")
test_that("Incorrect Func", {
  expect_null (rCI (Ret, 6, St, P0, Perturbs, 2, 10, 0.05))		# Bad function 'Func'
})

cat ("rCI - Bad St\n")
St1		<- St
St1[3]	<- "aa"
test_that("Bad St -1", {
  expect_null (rCI (Ret, F6, St1, P0, Perturbs, 2, 10, 0.05))		# St is not digital
})

St1		<- St[-5]
test_that("Bad St -2", {
  expect_null (rCI (Ret, F6, St1, P0, Perturbs, 2, 10, 0.05))		# St doesn't have the good size
})

cat ("rCI - Bad P0\n")
P1		<- P0[-5]
test_that("Bad P0", {
  expect_null (rCI (Ret, F6, St, P1, Perturbs, 2, 10, 0.05))		# P0 doesn't have the good size
})

cat ("rCI - Bad Perturbs \n")
test_that("Bad Perturbs", {
  expect_null (rCI (Ret, F6, St, P0, 'Perturbs', 2, 10, 0.05))	# Perturbs is not digital
})

cat ("rCI - Bad nbRep \n")
test_that("Bad nbRep -1", {
  expect_null (rCI (Ret, F6, St, P0, Perturbs, -2, 10, 0.05))		# nbRep < 0
})

test_that("rCI - Bad nbRep -2", {
  expect_null (rCI (Ret, F6, St, P0, Perturbs, 2.5, 10, 0.05))	# nbRep is not integer
})

cat ("rCI - Bad nbD \n")
test_that("Bad nbD", {
  expect_null (rCI (Ret, F6, St, P0, Perturbs, 2, 0, 0.05))		# nbD <= 0
})

cat ("rCI - Bad K \n")
test_that("Bad K", {
  expect_null (rCI (Ret, F6, St, P0, Perturbs, 2, 10, -0.05))		# K < 0
})

cat ("rCI - Bad Files \n")
test_that("Bad Root -1", {
  expect_null (rCI (Ret, F6, St, P0, Perturbs, 2, 10, 0.05, NomSol))				# Solution NOT NULL and Root NULL
})

test_that("Bad Solution -1", {
  expect_null (rCI (Ret, F6, St, P0, Perturbs, 2, 10, 0.05, Root=RootW))			# Solution  NULL and Root NOT NULL
})

test_that("Bad Root -2", {
  expect_null (rCI (Ret, F6, St, P0, Perturbs, 2, 10, 0.05, NomSol, "C:"))			# Root is not a writable folder
})

BadSol	<- "C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/Solution_6K.rda"
test_that("Bad Solution -1", {
  expect_null (rCI (Ret, F6, St, P0, Perturbs, 2, 10, 0.05, BadSol, RootW))		# Solution doesn't exist
})


#	PART TWO : test the proper functioning

RRoot	<-	"C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/"
load (paste(RRoot, "Mean_6K_100D3RK5pc", ".rda", sep=""))			# Mean_6K100D	3 replicates, K=0.05
load (paste(RRoot, "Min_6K_100D3RK5pc",  ".rda", sep=""))			# Min_6K100D
load (paste(RRoot, "Max_6K_100D3RK5pc",  ".rda", sep=""))			# Max_6K100D

#	6 nodes network

Result	<- rCI (Ret, F6, St, P0, Perturbs, 3, 100, 0.05)

cat ("rCI - Good results \n")
test_that("Good results -1", {
	expect_equal (Result$IC95$Mean, Mean_6K_100D3RK5pc, tolerance=1E-4)
})

test_that("Good results -2", {
	expect_equal (Result$IC95$Min, Min_6K_100D3RK5pc, tolerance=1E-4)
})

test_that("Good results -3", {
	expect_equal (Result$IC95$Max, Max_6K_100D3RK5pc, tolerance=1E-4)
})

cat ("rCI - Files creation \n")
Result	<- rCI (Ret, F6, St, P0, Perturbs, 3, 100, 0.05, NomSol, RootW)		# Generates a graph saved in RootW/NomSol (without extension).pdf
NomFic	<- strsplit(basename (NomSol), ".", fixed=TRUE)[[1]][1]	
NomFic	<- paste (RootW, "/", NomFic, ".pdf", sep="")

test_that("File saved -1", {
	expect_true (file.exists(NomFic))
})

test_that("File saved -2", {
	expect_true (file.size(NomFic) > 7400)										# The file size is correct
})


#	4 nodes network  -- Order 2

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

###		nbN			<- 4
###		load ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/ResO2_4.rda")		#  This is an 'Order2' solution. We need the corresponding MatrCc matrix
###		Res_4	<- matrix(0, nrow=nbN, ncol=nbN)		# MatrCc matrix
###		for (iRow in 1:nbN) {
###			for (jCol in 1:nbN) {
###				if (jCol < iRow)
###					Res_4[iRow,jCol]	<- ResO2_4[iRow,jCol]
###				else if (jCol == iRow)
###					Res_4[iRow,jCol]	<- -1
###				else
###					Res_4[iRow,jCol]	<- ResO2_4[iRow,jCol-1]
###			}
###		}
###		
###		save (Res_4, file="C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Res_4.rda")
NomSol4		<- "C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/Res_4.rda"

nbN			<- 4
Perturbs	<- c(0.2, 0.9, 0.99)
P0			<- c(VS1, VS2, VS3, VS4)
st 			<- c (0.1, 0.1, 0.1, 0.1)
Exp_ 		<- MExp (nbN, Perturbs, P0, F4, st)

Res_4_2		<-	MRARegress(Exp_$Exp, Exp_$Pert, Method="Order2", Relative=FALSE)

Result	<- rCI (Res_4_2, F4, st, P0, Perturbs, 2, 10, 0.05, NomSol4, RootW)		# Generates a graph saved in RootW/NomSol (without extension).pdf
NomFic	<- strsplit(basename (NomSol4), ".", fixed=TRUE)[[1]][1]	
NomFic	<- paste (RootW, "/", NomFic, ".pdf", sep="")

cat ("rCI - Good results 4 nodes \n")
test_that("File saved -3", {
	expect_true (file.exists(NomFic))
})

test_that("File saved -4", {
	expect_true (file.size(NomFic) > 5600)											# The file size is correct
})
