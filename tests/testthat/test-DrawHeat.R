#
#	DrawHeat unit tests
#

#	PART ONE : check input data

MatExp		<- matrix(c(1,2,3, 11,21,31, 12,22,32, 13,23,33),nrow=3)	# 3 nodes, 3 perturbations, Base : 1st column
Ret			<- MRARegress (MatExp, Relative=FALSE)

cat ("DrawHeat - Incorrect Ret -1s \n")
Ret1		<- Ret
Ret1$Input$Variables	<- NULL
test_that("Incorrect Ret -1", {
  expect_null (DrawHeat(Ret1))										# Data missing in the list
})

Ret2		<- Ret
KeysR		<- Ret$Input$Variables$Keys								# Keys read
KeysR[3]	<- KeysR[3] +1	
Ret2$Input$Variables$Keys	<- KeysR
test_that("Incorrect Ret -2", {
  expect_null (DrawHeat(Ret2))										# Bad signature
})

Vect		<- c(1, 2, 3, 4)
test_that("Ret vector -3", {
  expect_null (DrawHeat(Vect))										# 'Ret' is neither a list nor a matrix
})

test_that("Ret square matrix -4", {
  expect_null (DrawHeat(MatExp))									# 'Ret' is neither a list nor a square matrix
})

MatExp0		<- MatExp[ ,-1]
MatExp0[2,1]<- 'A'
test_that("Ret non numeric -5", {
  expect_null (DrawHeat(MatExp0))									# 'Ret' is neither a list nor a matrix of numbers
})

test_that("Incorrect title -6", {
  expect_null (DrawHeat(Ret, Title=12))								# Title must be a string
})

test_that("Absolute is logical", {
  expect_null (DrawHeat(Ret, Absolute="Faux"))						# Not a logical (TRUE, FALSE)
})

test_that("Legend is logical", {
  expect_null (DrawHeat(Ret, Legend="Faux"))						# Not a logical (TRUE, FALSE)
})

test_that("DisplayNumbers is logical", {
  expect_null (DrawHeat(Ret, DisplayNumbers="Faux"))				# Not a logical (TRUE, FALSE)
})

test_that("NoPrint is logical", {
  expect_null (DrawHeat(Ret, NoPrint="Faux"))						# Not a logical (TRUE, FALSE)
})

test_that("FontSize is a number", {
  expect_null (DrawHeat(Ret, FontSize='abcd'))						# FontSize must be a number
})

test_that("FontSize is > 0", {
  expect_null (DrawHeat(Ret, FontSize=0))							# FontSize must be a strictly positive number
})

test_that("DecNumbers is a number", {
  expect_null (DrawHeat(Ret, DecNumbers='abcd'))					# DecNumbers must be a number
})

test_that("DecNumbers is >= 0", {
  expect_null (DrawHeat(Ret, DecNumbers=-2))						# DecNumbers must be a positive number
})

test_that("Colour is in a list", {
  expect_null (DrawHeat(Ret, Colour='abcd'))						# Colour must be in a specific list
})

###		test_that("FileName is a valid name", {
###		  expect_null (DrawHeat(Ret, FileName="C:\Users\jean-pierre.borg\IRCM\These\Recherche\Packages\Temporaire\T_DrawHeat"))			# FileName incorrect : \ vs /
###		})

test_that("FileName is a valid name", {
  expect_null (DrawHeat(Ret, FileName="C:\\Users\\Dupont\\T_DrawHeat"))			# FileName incorrect : écriture impossible
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

Res_6_2		<-	MRARegress(Exp_$Exp, Exp_$Pert, Method="Order2", Relative=FALSE)

T	<- DrawHeat(Res_6_2$r, FileName="C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/Temporaire/T_DrawHeat")
test_that("Network6_T1", {
  expect_true (file.exists(	"C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/Temporaire/T_DrawHeat.pdf"))		# File is written
})
file.remove ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/Temporaire/T_DrawHeat.pdf")

T	<- DrawHeat(Res_6_2$r, Colour="RdBu", FileName="C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/Temporaire/T_DrawHeat")
test_that("Network6_T2", {
  expect_true (file.exists(	"C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/Temporaire/T_DrawHeat.pdf"))		# File is written
})
file.remove ("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/Temporaire/T_DrawHeat.pdf")

T	<- DrawHeat(Res_6_2$r)
test_that("Bon affichage sur l'écran", {
	expect_equal (names(dev.cur()), "RStudioGD")
})

#	Big networks (100 nodes)

vRoot		<- "C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRAregress/data/"
Fics		<- c("100_1", "100_2", "100_3", "100_4", "100_5")		# Files to study
nbFics		<- length(Fics)
ColourSet	<- c('Spectral', 'RdYlGn', 'RdGy', 'RdBu', 'PuOr')		# Colour set

load (paste(vRoot, "Perturb_100.rda", sep=""))		#   Perturb_100

for (iFic in 1:nbFics) {
	load (paste(vRoot, "MatExp_",   Fics[iFic], ".rda", sep=""))		#   MatExp
	Ret		<- MRARegress (MatExp, Perturb_100, NoPrint=TRUE)
	
	Titre	<- paste("Essai ", Fics[iFic], sep="")
	FN	<- paste("C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/Temporaire/T_DrawHeat_", iFic, sep="")
	T	<- DrawHeat(Ret$r, Colour=ColourSet[iFic], Title=Titre, DisplayNumbers=FALSE, FileName=FN)
	TestName	<- paste("Big Network : ", Fics[iFic], sep="")
	test_that(TestName, {
		expect_true (file.exists(paste(FN, ".pdf", sep="")))		# File is written
	})
	FN1	<- paste(FN, ".pdf", sep="")
	file.remove (FN1)
}
