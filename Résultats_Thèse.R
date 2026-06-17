#
#	  Résultats_Thèse.R
#     Copyright (C) 2023... 2026  Jean-Pierre BORG
#
#	  R software used to write my PHD thesis.
#	  The files to download concerning figures ************   along with their URL, are indicated in the code.
# 
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
# 
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
# 
#     If you don't have a copy of the GNU General Public License,
#     see <https://www.gnu.org/licenses/>.
# 
#     Jean-Pierre BORG
#     University of Montpellier, France
#     Institut de Recherche en Cancérologie de Montpellier, INSERM, France
#	  Cancer Bioinformatics and Systems Biology  -- INSERM U1194
#     jean-pierre.borg@inserm.fr  or jeanpierre.borg@orange.fr


#
#	Reset of the R session and environment variables, to start again from scratch.
#
rm(list = ls())		# We have also to restart a new session to unload all the packages.
					# We can also detach every package individually by : detach(package:packagename)
Verbose	<- FALSE
					
#
# 	Declaration of the libraries and of the functions used in this document
#
library ("rootSolve")		# 	To use multiroot
library ("deSolve")			# 	To use ode
library ("Deriv")			#	To use Deriv
library ("ggplot2")			# 	To use ggplot
library ("glmnet")			#	To use glmnet
library ("dplyr")			# 	To use rename
library ("data.table")		# 	To use fread
library	("forcats")			#	To use fct_relevel
library ("pracma")			# 	To use trapz
library ("verification")	# 	To use roc.area
library ("minet")			# 	To use build.mim, clr, aracne, mrnet -- If necessary, install pkg "BiocManager", then BiocManager::install("minet")
library ("parallel")		#	To use parallel
library ("foreach")			#	To use foreach
library ("doParallel")		#	To use doParallel

library('MRARegress')		# Ne pas activer cette librairie si on modifie "MRARegress"
library('stringr')
library('RCy3')
library('CVXR')				#	To use convexity
library('upstartr')			#	To use unaccent
library ("data.table")		# 	To use fread
library("testthat")			# 	To use test_that

vRoot		<- "C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/"

#	Figure 5 : Illustration de la méthode MRA
MatExp	<- matrix(c(1,1,1, 11,12,13, 21,22,23, 31,32,33), nrow=3)
R_Fig5	<- MRARegress(MatExp, Relative=FALSE)
R_Fig5$r
###		  	 Q1  Q2  Q3
###		N1 -1.0  2 -1.0
###		N2  0.5 -1  0.5
###		N3 -1.0  2 -1.0

Trash	<- DrawGraph(R_Fig5)



#	Figure 6 : Un réseau de 6 nœuds : "Cascade MAPK"

# 	Constants in relation with the kinetics of the reactions
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

F6n 	<- function(P,X)
   c(F1 = (KC1*U*X[1]) 	/ ((K11+X[1]+(MKKK0-X[1]-X[2])*K11/K12)*(1+X[6]/Ki)) - ((P[1]*(MKKK0-X[1]-X[2]))  / (K31+X[2]+(MKKK0-X[1]-X[2])*K31/K32+X[1]*K31/K33)),
	 F2 = (KC2*U*(MKKK0-X[1]-X[2]))    / ((K11+X[1]+(MKKK0-X[1]-X[2])*K11/K12)*(1+X[6]/Ki)) - ((P[2]*X[2])  / (K31+X[2]+(MKKK0-X[1]-X[2])*K31/K32+X[1]*K31/K33)),
	 F3 = (KC5*X[3]*X[2]) / (K51+X[3]+(MKK0-X[3]-X[4])*K51/K52) - ((P[3]*(MKK0-X[3]-X[4])*(1+A*X[6]/Kmp))   / ((K71+X[4]+(MKK0-X[3]-X[4])*K71/K72+X[3]*K71/K73)*(1+X[6]/Kmp))),
	 F4 = (KC6*(MKK0-X[3]-X[4])*X[2])  / (K51+X[3]+(MKK0-X[3]-X[4])*K51/K52) - ((P[4]*X[4]*(1+A*X[6]/Kmp))  / ((K71+X[4]+(MKK0-X[3]-X[4])*K71/K72+X[3]*K71/K73)*(1+X[6]/Kmp))),
	 F5 = (KC9*X[4]*X[5]) / (K91+X[5]+(MAPK0-X[5]-X[6])*K91/K92) - ((P[5]*(MAPK0-X[5]-X[6])) / (K111+X[6]+(MAPK0-X[5]-X[6])*K111/K112+X[5]*K111/K113)),
	 F6 = (KC10*X[4]*(MAPK0-X[5]-X[6]))/ (K91+X[5]*(MAPK0-X[5]-X[6])*K91/K92) - ((P[6]*X[6]) / (K111+X[6]+(MAPK0-X[5]-X[6])*K111/K112+X[5]*K111/K113)))

NBN			<- 6

P0			<- c(V4, V3, V8, V7, V12, V11)
st			<- c(130,45,52,105,135,27)
Names		<- c("MKKK (1)", "MKKK-PP (2)", "MKK (3)", "MKK-PP (4)", "MAPK (5)", "MAPK-PP (6)")

#	Panneau B	: valeur exacte
Perturbs	<- c(0.99999)
MExp_6_099999	<- MExp (NBN, Perturbs, P0, F6n, st)			# Simulation of 5 perturbations applied to each node. Network without noise. Exact value
Res_6_099999	<-  MRARegress(MExp_6_099999$Exp, MExp_6_099999$Pert)
round(Res_6_099999$r,3)
###		      Q1a    Q2a    Q3a    Q4a    Q5a    Q6a
###		N1 -1.000 -0.352  0.000  0.000  0.000  0.026
###		N2 -1.981 -1.000  0.000  0.000  0.000 -0.096
###		N3  0.000 -0.222 -1.000 -1.567  0.000  0.081
###		N4  0.000  0.267 -0.485 -1.000  0.000 -0.098
###		N5  0.000  0.000  0.000 -0.524 -1.000 -0.135
###		N6  0.000  0.000  0.000  0.981 -1.381 -1.000

#	Panneau A 	: dessin du réseau
Trash	<- DrawGraph(Res_6_099999, Thr=0.01)				# Pour avoir le nom des noeuds, il faut les introduire dans Res_6_099999

#	Panneau C	: Perturbation -50%
Perturbs	<- c(0.5)
MExp_6_05	<- MExp (NBN, Perturbs, P0, F6n, st)			# Simulation of 5 perturbations applied to each node. Network without noise.
Res_6_05	<-  MRARegress(MExp_6_05$Exp, MExp_6_05$Pert)
round(Res_6_05$r,3)
###		      Q1a    Q2a    Q3a    Q4a    Q5a    Q6a
###		N1 -1.000 -0.443 -0.017 -0.031 -0.002 -0.046
###		N2 -1.689 -1.000 -0.041 -0.074 -0.006 -0.219
###		N3 -0.986 -0.807 -1.000 -1.347 -0.061 -0.351
###		N4 -0.699 -0.076 -0.537 -1.000 -0.038 -0.413
###		N5 -0.306 -0.187 -0.035 -0.564 -1.000 -0.230
###		N6 -0.237 -0.144 -0.023  0.909 -1.270 -1.000

#	Panneau D	: Perturbation +50%
Perturbs	<- c(1.5)
MExp_6_15	<- MExp (NBN, Perturbs, P0, F6n, st)			# Simulation of 5 perturbations applied to each node. Network without noise.
Res_6_15	<-  MRARegress(MExp_6_15$Exp, MExp_6_15$Pert)
round(Res_6_15$r,3)
###		      Q1a    Q2a    Q3a    Q4a    Q5a    Q6a
###		N1 -1.000 -0.329  0.007  0.016  0.000  0.043
###		N2 -2.147 -1.000  0.025  0.054  0.001 -0.041
###		N3  0.602  0.082 -1.000 -1.739  0.059  0.302
###		N4  0.330  0.389 -0.448 -1.000  0.032  0.038
###		N5  0.129  0.067  0.017 -0.499 -1.000 -0.109
###		N6 -0.022 -0.009  0.005  0.975 -1.418 -1.000



#
#		Figure 7
#		Impact of perturbation and noise level for the 6 nodes MAP kinase network
#

NBN			<-	6										# Number of nodes
Perturbs	<- c(0.2, 0.3, 0.5, 0.7, 0.9, 0.99, 1.01, 1.10, 1.2, 1.3, 1.50, 1.6, 1.7)		# 13 perturbations
NBP			<- length(Perturbs)

st <- c(200,  0, 100,  50, 100,  0)			# Start time
#F	<- function(X)
#	F6n(Perturbs, X)
F <- function(X)
   c(F1 = (KC1*U*X[1]) 	/ ((K11+X[1]+(MKKK0-X[1]-X[2])*K11/K12)*(1+X[6]/Ki)) - ((V4*(MKKK0-X[1]-X[2]))  / (K31+X[2]+(MKKK0-X[1]-X[2])*K31/K32+X[1]*K31/K33)),
	 F2 = (KC2*U*(MKKK0-X[1]-X[2]))    / ((K11+X[1]+(MKKK0-X[1]-X[2])*K11/K12)*(1+X[6]/Ki)) - ((V3*X[2])  / (K31+X[2]+(MKKK0-X[1]-X[2])*K31/K32+X[1]*K31/K33)),
	 F3 = (KC5*X[3]*X[2]) / (K51+X[3]+(MKK0-X[3]-X[4])*K51/K52) - ((V8*(MKK0-X[3]-X[4])*(1+A*X[6]/Kmp))   / ((K71+X[4]+(MKK0-X[3]-X[4])*K71/K72+X[3]*K71/K73)*(1+X[6]/Kmp))),
	 F4 = (KC6*(MKK0-X[3]-X[4])*X[2])  / (K51+X[3]+(MKK0-X[3]-X[4])*K51/K52) - ((V7*X[4]*(1+A*X[6]/Kmp))  / ((K71+X[4]+(MKK0-X[3]-X[4])*K71/K72+X[3]*K71/K73)*(1+X[6]/Kmp))),
	 F5 = (KC9*X[4]*X[5]) / (K91+X[5]+(MAPK0-X[5]-X[6])*K91/K92) - ((V12*(MAPK0-X[5]-X[6])) / (K111+X[6]+(MAPK0-X[5]-X[6])*K111/K112+X[5]*K111/K113)),
	 F6 = (KC10*X[4]*(MAPK0-X[5]-X[6]))/ (K91+X[5]*(MAPK0-X[5]-X[6])*K91/K92) - ((V11*X[6]) / (K111+X[6]+(MAPK0-X[5]-X[6])*K111/K112+X[5]*K111/K113)))
	

XMesNP		<- array(dim=c(NBN))						# Abundance of the tested product (protein, gene ...) WITHOUT perturbation
XMesP		<- array(dim=c(NBN, NBN))					# Abundance of the tested product (protein, gene ...) WITH perturbation
MatR		<- array(dim=c(NBN, NBN))					# Global Response matrix ("R")
pX   		<- array(dim=c(NBN-1, NBN-1))				# row (iRow), column (iCol)	
pY   		<- array(dim=c(NBN-1))						# row
	
MatrCc		<- array(-1, dim=c(NBN, NBN, NBP))			# Computed Connection matrix ("r")  -  we keep the NBP values for tests afterward.
ErrorQ		<- vector(length=NBP)						# Quadratic error
MatrTh		<- Res_6_099999$r

ss <- multiroot (f=F, start= st)						# Non perturbed measure
XMesNP[ ]	<- c (ss$root[1], ss$root[2], ss$root[3], ss$root[4], ss$root[5], ss$root[6])
XMesNPMoy	<- 	mean(XMesNP)			# Noise standard deviation will be defined from this mean level
									# Measured concentrations in the 6 nodes, non perturbed network, steady state	

for (iPert in c(1:NBP)) {
	cat("iPert ", iPert," st ", st, "\n")
	Perturb	 <- Perturbs[iPert]							# Perturbation level
	
	V4 <- Perturb*V4		# P1
	ss <- multiroot (f=F, start= st)
	XMesP[ ,1] 	<- c (ss$root[1], ss$root[2], ss$root[3], ss$root[4], ss$root[5], ss$root[6])
	V4 <- V4/Perturb		# Return to initial value
	
	V3 <- Perturb*V3		# P2
	ss <- multiroot (f=F, start= st)
	XMesP[ ,2] 	<- c (ss$root[1], ss$root[2], ss$root[3], ss$root[4], ss$root[5], ss$root[6])
	V3 <- V3/Perturb		# Return to initial value
	
	V8 <- Perturb*V8		# P3
	ss <- multiroot (f=F, start= st)
	XMesP[ ,3] 	<- c (ss$root[1], ss$root[2], ss$root[3], ss$root[4], ss$root[5], ss$root[6])
	V8 <- V8/Perturb		# Return to initial value
	
	V7 <- Perturb*V7		# P4
	ss <- multiroot (f=F, start= st)
	XMesP[ ,4] 	<- c (ss$root[1], ss$root[2], ss$root[3], ss$root[4], ss$root[5], ss$root[6])
	V7 <- V7/Perturb		# Return to initial value
	
	V12 <- Perturb*V12		# P5
	ss <- multiroot (f=F, start= st)
	XMesP[ ,5] 	<- c (ss$root[1], ss$root[2], ss$root[3], ss$root[4], ss$root[5], ss$root[6])
	V12 <- V12/Perturb		# Return to initial value
	
	V11 <- Perturb*V11		# P6
	ss <- multiroot (f=F, start= st)
	XMesP[ ,6] 	<- c (ss$root[1], ss$root[2], ss$root[3], ss$root[4], ss$root[5], ss$root[6])
	V11 <- V11/Perturb		# Return to initial value

	for (iCol in 1:NBN) {
		MatR[ ,iCol]  <- 2 * (XMesP[ ,iCol]-XMesNP[ ]) / (XMesP[ ,iCol]+XMesNP[ ])
	}	
	
	for (iMat in 1:NBN) {
		col <- 1:NBN
		col <- col[-iMat]
							
		for (iRow in 1:(NBN-1)) {							# Remove one row and one column from MatR
			pY[iRow] = MatR[iMat, col[iRow]]
								
			for (iCol in 1:NBN-1) {
				pX[iRow, iCol] = MatR[col[iCol], col[iRow]]
			}
		}
							
		Form <- paste("pY[]~pX[,]+0")
		MatrCc[iMat, col, iPert] 	<- (lm(as.formula(Form)))$coefficients
	}		# Loop on iMat
	
	qe2		<- sum((MatrCc[,,iPert] - MatrTh)**2)
	ErrorQ[iPert]	<- sqrt(qe2)
}			# Loop on iPert
	
# NomFic 	<- paste(vRoot, "Fig2a_MRA3.pdf", sep="")
# pdf(file=NomFic, height=6, width=6) 					# Sizes default to 7

#	Panneau A

plot (Perturbs[2:13], ErrorQ[2:13], xlim=c(0.3, 1.8), ylim=c(-0.1,4), main="Error Cc vs. Th", type="b", col="blue", xlab="Perturbation %", ylab="Error", xaxt="n", axes=TRUE, bty="l")
axis(1, at= seq(0.5, 1.5, by=0.5), labels= c("-50", "0", "+50"))		# vs types ="l"
 
segments(x0 = 0, y0 = ErrorQ[3],  x1 = Perturbs[3], y1 = ErrorQ[3], col = "darkgrey", lty="dashed") 
segments(x0 = Perturbs[3], y0 = -0.1, x1 = Perturbs[3], y1 = ErrorQ[3], col = "darkgrey", lty="dashed") 
text(0.3, ErrorQ[3], round(ErrorQ[3], digits=2), font=3)		# font : 1=normal, 2=bold, 3=italic, 4=bold+italic
points(Perturbs[3], ErrorQ[3], pch=16, col="black")
text(Perturbs[3]+0.05, ErrorQ[3]+0.05, "A")

segments(x0 = 0, y0 = 0, x1 = (Perturbs[3]+Perturbs[11])/2, y1 = 0, col = "darkgrey", lty="dashed") 
segments(x0 = (Perturbs[3]+Perturbs[11])/2, y0 = -0.1, x1 = (Perturbs[3]+Perturbs[11])/2, y1 = 0, col = "darkgrey", lty="dashed") 
points((Perturbs[3]+Perturbs[11])/2, 0, pch=16, col="black")
text((Perturbs[3]+Perturbs[11])/2, 0.25, "B")

segments(x0 = 0, y0 = ErrorQ[11],  x1 = Perturbs[11], y1 = ErrorQ[11], col = "darkgrey", lty="dashed") 
segments(x0 = Perturbs[11], y0 = -0.1, x1 = Perturbs[11], y1 = ErrorQ[11], col = "darkgrey", lty="dashed") 
text(0.3, ErrorQ[11], round(ErrorQ[11], digits=2), font=3)		# font : 1=normal, 2=bold, 3=italic, 4=bold+italic
points(Perturbs[11], ErrorQ[11], pch=16, col="black")
text(Perturbs[11], ErrorQ[11]+0.25, "C")

dev.off()


#	Panneau B

NBN 		<- 6								# Number of nodes
Perturbs 	<- c(0.5, 0.7, 0.9, 0.99, 1.01, 1.1, 1.3, 1.5)					# 8 perturbations
PercentPert	<- c("-50", "-30", "-10", "-1", "+1", "+10", "+30", "+50")		# Percentages of perturbation
NBP			<- length(Perturbs)
Noises		<-	c(0.001, 0.005, 0.01)			# Noise  levels
NBT			<- length(Noises)

NBD			<-	100								# Number of random draws

XMesNP		<- array(dim=c(NBN))				# Abundance of the tested product (protein, gene ...) WITHOUT perturbation, WITH noise = abundance measured
XMesP		<- array(dim=c(NBN, NBN))			# Abundance of the tested product (protein, gene ...) WITH perturbation, WITH noise  = abundance measured
MatRN		<- array(dim=c(NBN, NBN))			# Global Response matrix ("R"), with noise
pX   		<- array(dim=c(NBN-1, NBN-1))		# row (iRow), column (iCol)	
pY   		<- array(dim=c(NBN-1))				# row
	
MatrCc		<- array(-1, dim=c(NBN, NBN))		# Computed Connection matrix ("r")
   
ErrorQ		<- array(dim=c(NBD))				# Quadratic error
ErrorQLn	<- array(dim=c(NBD))				# Ln(1+Quad. error)

ErrMoy		<- array(dim=c(NBT, NBP))			# Mean value of ErrorQ (amongst the random draws)
ErrSd		<- array(dim=c(NBT, NBP))			# Standard deviation of ErrorQ (amongst the random draws)
ErrMoyLn	<- array(dim=c(NBT, NBP))			# Mean value of ErrorQLn (amongst the random draws)
ErrSdLn		<- array(dim=c(NBT, NBP))
		 
for (iPert in c(1:NBP)) {
	cat("iPert ", iPert," st ", st, "\n")
	Perturb	 <- Perturbs[iPert]						# Perturbation level
	set.seed(12345)									# So as to generate the same seqences of noise	
	
	for (iTest in c(1:NBT)) {						# Noise level
		sd <- Noises[iTest]*XMesNPMoy				# Noise standard deviation

		for (iDraw in 1:NBD) {
			ss <- multiroot (f=F, start= st)		# Non perturbed measure, with noise
			XMesNP[ ]	<- c (ss$root[1], ss$root[2], ss$root[3], ss$root[4], ss$root[5], ss$root[6]) + rnorm(NBN, mean=0, sd=sd)
			
			#	Perturbed measures, with noise
			V4 <- Perturb*V4		# P1
			ss <- multiroot (f=F, start= st)
			XMesP[ ,1] 	<- c (ss$root[1], ss$root[2], ss$root[3], ss$root[4], ss$root[5], ss$root[6]) + rnorm(NBN, mean=0, sd=sd)
			V4 <- V4/Perturb		# Return to initial value	
			
			V3 <- Perturb*V3		# P2
			ss <- multiroot (f=F, start= st)
			XMesP[ ,2] 	<- c (ss$root[1], ss$root[2], ss$root[3], ss$root[4], ss$root[5], ss$root[6]) + rnorm(NBN, mean=0, sd=sd)
			V3 <- V3/Perturb		# Return to initial value
			
			V8 <- Perturb*V8		# P3
			ss <- multiroot (f=F, start= st)
			XMesP[ ,3]	 <- c (ss$root[1], ss$root[2], ss$root[3], ss$root[4], ss$root[5], ss$root[6]) + rnorm(NBN, mean=0, sd=sd)
			V8 <- V8/Perturb		# Return to initial value				
			
			V7 <- Perturb*V7		# P4
			ss <- multiroot (f=F, start= st)
			XMesP[ ,4] 	<- c (ss$root[1], ss$root[2], ss$root[3], ss$root[4], ss$root[5], ss$root[6]) + rnorm(NBN, mean=0, sd=sd)
			V7 <- V7/Perturb		# Return to initial value				
			
			V12 <- Perturb*V12		# P5
			ss <- multiroot (f=F, start= st)
			XMesP[ ,5] 	<- c (ss$root[1], ss$root[2], ss$root[3], ss$root[4], ss$root[5], ss$root[6]) + rnorm(NBN, mean=0, sd=sd)
			V12 <- V12/Perturb		# Return to initial value				
			
			V11 <- Perturb*V11		# P6
			ss <- multiroot (f=F, start= st)
			XMesP[ ,6] 	<- c (ss$root[1], ss$root[2], ss$root[3], ss$root[4], ss$root[5], ss$root[6]) + rnorm(NBN, mean=0, sd=sd)
			V11 <- V11/Perturb		# Return to initial valu				
				
			for (iCol in 1:NBN) {
				MatRN[ ,iCol]  <- 2 * (XMesP[ ,iCol]-XMesNP[ ]) / (XMesP[ ,iCol]+XMesNP[ ])
			}

			for (iMat in 1:NBN) {
				col <- 1:NBN
				col <- col[-iMat]
									
				for (iRow in 1:(NBN-1)) {								# Remove one row and one column from MatRN
					pY[iRow] = MatRN[iMat, col[iRow]]
										
					for (iCol in 1:NBN-1) {
						pX[iRow, iCol] = MatRN[col[iCol], col[iRow]]
					}
				}
									
				Form <- paste("pY[]~pX[,]+0")
				MatrCc[iMat, col] 	<- (lm(as.formula(Form)))$coefficients
			}		# Loop on iMat

			qe2		<- sum((MatrCc - MatrTh)**2)
			ErrorQ  [iDraw]		<- sqrt(qe2)
			ErrorQLn[iDraw]	 = log(1+ErrorQ  [iDraw])

#			ErrorQLn[iDraw]		<- log1p(ErrorQ[iDraw])		# Ln(1+ErrorQ). The function log1p(x= = Ln(1+x) is more accurate than Log(1+x) if |x| << 1
		}			# Loop on iDraw
	
		ErrMoy  [iTest,iPert]		<- mean	(ErrorQ)
		ErrSd	[iTest,iPert]		<- sd  	(ErrorQ)	
		ErrMoyLn[iTest,iPert]		<- mean	(ErrorQLn)
		ErrSdLn	[iTest,iPert]		<- sd  	(ErrorQLn)
	}			# Loop on iTest
}				# Loop on iPert		

matrN2.df	<- data.frame(Pert=1:8, mean=ErrMoyLn[1, 1:8],  lower=ErrMoyLn[1, 1:8]-ErrSdLn[1, 1:8],   upper=ErrMoyLn[1, 1:8]+ErrSdLn[1, 1:8], Coef = "k = 0.002")
matrN5.df	<- data.frame(Pert=1:8, mean=ErrMoyLn[2, 1:8],  lower=ErrMoyLn[2, 1:8]-ErrSdLn[2, 1:8],   upper=ErrMoyLn[2, 1:8]+ErrSdLn[2, 1:8], Coef = "k = 0.005")
matrN10.df	<- data.frame(Pert=1:8, mean=ErrMoyLn[3, 1:8],  lower=ErrMoyLn[3, 1:8]-ErrSdLn[3, 1:8],   upper=ErrMoyLn[3, 1:8]+ErrSdLn[3, 1:8], Coef = "k = 0.01")

# NomFic 	<- paste(vRoot, "Fig2b_MRA3.pdf", sep="")
# pdf(file=NomFic, height=6, width=6) 					# Sizes default to 7

Titre		<- "Ln (1+Err) as a function of the perturbation"			# Title of the figure
szLBar		<- 7				# Size of the bars  3
spLBar		<- 0.25				# Space between the bars  0.15
szEBar		<- 0.1				# Size of the error bars
ggplot() + 
	geom_linerange(data=matrN2.df,   aes(x=Pert, 			ymin=0, ymax=mean), size=szLBar, color="blue")+ 
	geom_errorbar (data=matrN2.df,   aes(x=Pert, 			ymin=lower, ymax=upper), color = "black", width = szEBar)+
	geom_linerange(data=matrN5.df,   aes(x=Pert+spLBar,		ymin=0, ymax=mean), size=szLBar, color="yellow")+ 
	geom_errorbar (data=matrN5.df,   aes(x=Pert+spLBar,		ymin=lower, ymax=upper), color = "black", width = szEBar)+
	geom_linerange(data=matrN10.df,  aes(x=Pert+2*spLBar, 	ymin=0, ymax=mean), size=szLBar, color="red")+ 
	geom_errorbar (data=matrN10.df,  aes(x=Pert+2*spLBar, 	ymin=lower, ymax=upper), color = "black", width = szEBar)+
	scale_colour_manual(name="Coef", values=c("k = 0.002"="blue", "k = 0.005"="yellow", "k = 0.01"="red"))+
	labs(title=Titre, x="\nPerturbation (%)", y="Ln (1+Err)") + scale_x_continuous(breaks=1:8+spLBar, labels=PercentPert)+
	theme(text = element_text(size = 14)) 
		   
dev.off()



#	Figure 8
#	Impact de l'utilisation des réplicas sur le réseau "Cascade MAPK"

#		Inference performance for the 6 nodes MAP kinase network
#		Comparison of computation methods, 3 noise levels, perturbation level +50%
#

NBN 		<- 6								# Number of nodes
Perturb 	<- 1.5								# Perturbation level +50%
Noises		<-	c(0.001, 0.005, 0.01)			# Noise  levels
NBT			<- length(Noises)

NBD			<-	100								# Number of random draws
Replics		<-  c(3, 5)							# Number of replicates
NBR			<- length(Replics)
NBRM		<- max(Replics)

Methods		<-  c("MRA", "LSE_CI", "LASSO", "LASSO1", "TLR", "STEP-Fo", "STEP-Ba", "STEP-Bo")	# Methods used (see details inside the functions)
NBM			<- length(Methods)					# Number of méthods
NBRST		<-	3								# Number of STEP methods
ScoreTests	<-	c("T+", "T-", "T0", "F+", "F-", "F0", "P", "N", "Sensib", "Specif")				# Scores to measure
NBSC		<- length(ScoreTests)				# Number of scores

MatrThDig	<- array(dim=c(NBN, NBN))			# Theoretical matix "r", digitalized (-1, 0, 1)
Score		<- array(dim=c(NBSC, NBM, NBT))		# Scores measured
dimnames(Score)	<- list(ScoreTests, Methods, Noises)
DistM 			<- array(0, dim=c(NBM, NBT))	# Distance to the first bisecting line of the point (Se, 1-Sp)
dimnames(DistM) <- list(Methods, Noises)

XMesNP		<- array(dim=c(NBN))				# Abundance of the tested product (protein, gene ...) WITHOUT perturbation, WITH noise = abundance measured
XMesP		<- array(dim=c(NBN, NBN))			# Abundance of the tested product (protein, gene ...) WITH perturbation, WITH noise  = abundance measured
MatRN		<- array(dim=c(NBN, NBN, NBD, NBT))	# Global Response matrix ("R"), with noise
pX   		<- array(dim=c(NBN-1, NBN-1))		# row (iRow), column (iCol)	
pY   		<- array(dim=c(NBN-1))				# row
gX   		<- array(dim=c(NBRM*(NBN-1), NBN-1, NBN, NBT))	# row (iRow), column (iCol), Mat nbr (iMat), Noise (iTest)	 -- 5 replicates
gY   		<- array(dim=c(NBRM*(NBN-1), NBN, NBT))			# row, , Mat nbr (iMat), Noise (iTest)						 -- 5 replicates
gX3   		<- array(dim=c(3*(NBN-1), NBN-1, NBN, NBT))		# row (iRow), column (iCol), , Mat nbr (iMat), Noise (iTest) -- 3 replicates
gY3   		<- array(dim=c(3*(NBN-1), NBN, NBT))			# row, Mat nbr (iMat), Noise (iTest)						 -- 3 replicates

MatrCc		<- array(-1, dim=c(NBN, NBN,NBD))	# Computed Connection matrix ("r")
MatrCcDig	<- array(-1, dim=c(NBN, NBN))		# Computed Connection matrix digitalized ((-1, 0, 1)

ValMeanBP	<- array(dim=c(NBN, NBN, NBT))		# Mean value of rij, bootstrap method
ValMinBP	<- array(dim=c(NBN, NBN, NBT))		# Min value at IC95 of rij, bootstrap method
ValMaxBP	<- array(dim=c(NBN, NBN, NBT))		# Max value at IC95 of rij, bootstrap method

ValMean3	<- array(dim=c(NBN, NBN, NBT))		# Mean value of rij, 3 replicates
ValMin3		<- array(dim=c(NBN, NBN, NBT))		# Min value at IC95 of rij, 3 replicates
ValMax3		<- array(dim=c(NBN, NBN, NBT))		# Max value at IC95 of rij, 3 replicates
ValCc3		<- array(dim=c(NBN, NBN, NBT))		# Coefficient values, 3 replicates

ValMean5	<- array(dim=c(NBN, NBN, NBT))		# Mean value of rij, 5 replicates
ValMin5		<- array(dim=c(NBN, NBN, NBT))		# Min value at IC95 of rij, 5 replicates
ValMax5		<- array(dim=c(NBN, NBN, NBT))		# Max value at IC95 of rij, 5 replicates
ValCc5		<- array(dim=c(NBN, NBN, NBT))		# Coefficient values, 5 replicates

rL 			<- array(dim=c(NBN))				# Result of Lasso method (ie sol. of Yi = Ai * Xi)
rLasso  	<- array(dim=c(NBN, NBN))			# Integration of results Xi, add -1 on the diagonal and transpose the matrix  to get the matrix "r"
rij 		<- matrix(nrow=1, ncol=NBN-1)		# Intermediate computing of rij

set.seed(12345)									# So as to generate the same seqences of noise
Seed	<- .Random.seed

for (iTest in c(1:NBT)) {						# Noise level
	.Random.seed	<- Seed
	sd <- Noises[iTest]*XMesNPMoy				# Noise standard deviation

	for (iDraw in 1:NBD) {
		ss <- multiroot (f=F, start= st)		# Non perturbed measure, with noise
		XMesNP[ ]	<- c (ss$root[1], ss$root[2], ss$root[3], ss$root[4], ss$root[5], ss$root[6]) + rnorm(NBN, mean=0, sd=sd)
		
		#	Perturbed measures, with noise
		V4 <- Perturb*V4		# P1
		ss <- multiroot (f=F, start= st)
		XMesP[ ,1] 	<- c (ss$root[1], ss$root[2], ss$root[3], ss$root[4], ss$root[5], ss$root[6]) + rnorm(NBN, mean=0, sd=sd)
		V4 <- V4/Perturb		# Return to initial value	
		
		V3 <- Perturb*V3		# P2
		ss <- multiroot (f=F, start= st)
		XMesP[ ,2] 	<- c (ss$root[1], ss$root[2], ss$root[3], ss$root[4], ss$root[5], ss$root[6]) + rnorm(NBN, mean=0, sd=sd)
		V3 <- V3/Perturb		# Return to initial value
		
		V8 <- Perturb*V8		# P3
		ss <- multiroot (f=F, start= st)
		XMesP[ ,3]	 <- c (ss$root[1], ss$root[2], ss$root[3], ss$root[4], ss$root[5], ss$root[6]) + rnorm(NBN, mean=0, sd=sd)
		V8 <- V8/Perturb		# Return to initial value				
		
		V7 <- Perturb*V7		# P4
		ss <- multiroot (f=F, start= st)
		XMesP[ ,4] 	<- c (ss$root[1], ss$root[2], ss$root[3], ss$root[4], ss$root[5], ss$root[6]) + rnorm(NBN, mean=0, sd=sd)
		V7 <- V7/Perturb		# Return to initial value				
		
		V12 <- Perturb*V12		# P5
		ss <- multiroot (f=F, start= st)
		XMesP[ ,5] 	<- c (ss$root[1], ss$root[2], ss$root[3], ss$root[4], ss$root[5], ss$root[6]) + rnorm(NBN, mean=0, sd=sd)
		V12 <- V12/Perturb		# Return to initial value				
		
		V11 <- Perturb*V11		# P6
		ss <- multiroot (f=F, start= st)
		XMesP[ ,6] 	<- c (ss$root[1], ss$root[2], ss$root[3], ss$root[4], ss$root[5], ss$root[6]) + rnorm(NBN, mean=0, sd=sd)
		V11 <- V11/Perturb		# Return to initial valu				
			
		for (iCol in 1:NBN) {
			MatRN[ ,iCol,iDraw,iTest]  <- 2 * (XMesP[ ,iCol]-XMesNP[ ]) / (XMesP[ ,iCol]+XMesNP[ ])
		}
	}		# Loop on iDraw
	
	Seed	<- .Random.seed			# Seed is modified by Lasso etc.. in a randomly fashion

#
#		Bootstrap
#
	for (iDraw in 1:NBD) {
		for (iMat in 1:NBN) {
			col <- 1:NBN
			col <- col[-iMat]
								
			for (iRow in 1:(NBN-1)) {								# Remove one row and one column from MatRN
				pY[iRow] = MatRN[iMat,col[iRow],iDraw,iTest]
									
				for (iCol in 1:NBN-1) {
					pX[iRow, iCol] = MatRN[col[iCol], col[iRow],iDraw,iTest]
				}
			}
								
			Form <- paste("pY[]~pX[,]+0")
			MatrCc[iMat, col, iDraw] 	<- (lm(as.formula(Form)))$coefficients
		}		# Loop on iMat
	}			# Loop on iDraw
	
	for (iRow in 1:NBN) {
		col <- 1:NBN
		col <- col[-iRow]
		
		for (iCol in col) {	
			ValMeanBP[iRow,iCol,iTest] 		<- mean(MatrCc[iRow,iCol, ])
			qq	<- quantile(MatrCc[iRow,iCol, ], probs=c(0.025,0.975))		# IC 95%
			ValMinBP [iRow,iCol,iTest] 		<- qq[1]
			ValMaxBP [iRow,iCol,iTest] 		<- qq[2]
		}	# Loop on iCol
	}		# Loop on iRow
	
#
#	Use of replicates
#
	for (iDraw in 1:NBRM) {
		for (iMat in 1:NBN) {
			col <- 1:NBN
			col <- col[-iMat]
								
			for (iRow in 1:(NBN-1)) {								# Remove one row and one column from MatRN
				pY[iRow] = MatRN[iMat,col[iRow],iDraw,iTest]
									
				for (iCol in 1:NBN-1) {
					pX[iRow, iCol] = MatRN[col[iCol], col[iRow],iDraw,iTest]
				}
			}
		
			gX[((NBN-1)*(iDraw-1)+1) : ((NBN-1)*iDraw), ,iMat,iTest] 	<- pX[ , ]
			gY[((NBN-1)*(iDraw-1)+1) : ((NBN-1)*iDraw),iMat,iTest]  	<- pY[  ]
		}
	}
	
	gX3[ , , ,iTest]	<- gX[1:(3*(NBN-1)), , ,iTest]
	gY3[ , ,iTest]		<- gY[1:(3*(NBN-1)), ,iTest]
	
	for (iRow in 1:NBN) {
		col 	<- 1:NBN
		col 	<- col[-iRow]

		Form3 	<- paste("gY3[ ,", iRow, ",", iTest, "]~gX3[ , ,", iRow, ",", iTest, "]+0")		# 3 replicates
		pQ 		<- lm(as.formula(Form3))
		ValMean3[iRow,col,iTest]  	<- broom::tidy(pQ)$estimate
		ValMin3 [iRow,col,iTest]  	<- broom::tidy(pQ)$estimate - 1.96*broom::tidy(pQ)$std.error	# 1.96 to get an IC 95%
		ValMax3 [iRow,col,iTest]  	<- broom::tidy(pQ)$estimate + 1.96*broom::tidy(pQ)$std.error
		ValCc3  [iRow,col,iTest]  	<- pQ$coefficients		
	
		Form5 	<- paste("gY[ ,", iRow, ",", iTest, "]~gX[ , ,", iRow, ",", iTest, "]+0")			# 5 replicates
		pQ 		<- lm(as.formula(Form5))
		ValMean5[iRow,col,iTest]  	<- broom::tidy(pQ)$estimate
		ValMin5 [iRow,col,iTest]  	<- broom::tidy(pQ)$estimate - 1.96*broom::tidy(pQ)$std.error	# 1.96 to get an IC 95%
		ValMax5 [iRow,col,iTest]  	<- broom::tidy(pQ)$estimate + 1.96*broom::tidy(pQ)$std.error	
		ValCc5  [iRow,col,iTest]  	<- pQ$coefficients				
	}		# Loop on iRow
}			# Loop on iTest


#		Informations for the X axis

OName	<-	"MKKK (1)             MKKK-PP (2)               MKK (3)                 MKK-PP (4)               MAPK (5)                 MAPK-PP (6)"
												# Names of the origins of the edges
Breaks	<-  c(1:(NBN*NBN))


#	Panneau A
#	noise level k = 0.1%

VMin	<- min(ValMinBP[ , ,1], ValMin3[ , ,1], ValMin5[ , ,1], na.rm=TRUE)		# Minimum on the Y axis
VMax	<- max(ValMaxBP[ , ,1], ValMax3[ , ,1], ValMax5[ , ,1], na.rm=TRUE)		# Maximum on the Y axis

MatrTh1		<- ifelse (MatrTh == -1, VMin, MatrTh)									# exact values

matrBP.df	<- data.frame(mean=NULL, lower=NULL, upper=NULL)		# To draw figures 3a and 3b
matr3.df	<- data.frame(mean=NULL, lower=NULL, upper=NULL)
matr5.df	<- data.frame(mean=NULL, lower=NULL, upper=NULL)
matrTh1.df	<- data.frame(val=NULL)

for (iMat in 1:NBN) {
	matrBP.tmp.df	<- data.frame(name=1:NBN, mean=as.vector(ValMeanBP[1:NBN,iMat,1]), lower=as.vector(ValMinBP[1:NBN,iMat,1]), upper=as.vector(ValMaxBP[1:NBN,iMat,1]))
	matrBP.tmp.df	<- matrBP.tmp.df[-iMat, ]
	matrBP.df		<- rbind(matrBP.df, matrBP.tmp.df)
	matrBP.tmp.df	<- data.frame(name=" ", mean=VMin, lower=VMin, upper=VMin)
	matrBP.df		<- rbind(matrBP.df, matrBP.tmp.df)

	matr3.tmp.df	<- data.frame(name=1:NBN, mean=as.vector(ValMean3[1:NBN,iMat,1]), lower=as.vector(ValMin3[1:NBN,iMat,1]), upper=as.vector(ValMax3[1:NBN,iMat,1]))
	matr3.tmp.df	<- matr3.tmp.df[-iMat, ]
	matr3.df		<- rbind(matr3.df, matr3.tmp.df)
	matr3.tmp.df	<- data.frame(name=" ", mean=VMin, lower=VMin, upper=VMin)
	matr3.df		<- rbind(matr3.df, matr3.tmp.df)

	matr5.tmp.df	<- data.frame(name=1:NBN, mean=as.vector(ValMean5[1:NBN,iMat,1]), lower=as.vector(ValMin5[1:NBN,iMat,1]), upper=as.vector(ValMax5[1:NBN,iMat,1]))
	matr5.tmp.df	<- matr5.tmp.df[-iMat, ]
	matr5.df		<- rbind(matr5.df, matr5.tmp.df)
	matr5.tmp.df	<- data.frame(name=" ", mean=VMin, lower=VMin, upper=VMin)
	matr5.df		<- rbind(matr5.df, matr5.tmp.df)
	
	matrTh1.tmp.df	<- data.frame(name=1:NBN, val=as.vector(MatrTh1[1:NBN,iMat]))
	matrTh1.tmp.df	<- matrTh1.tmp.df[-iMat, ]
	matrTh1.df		<- rbind(matrTh1.df, matrTh1.tmp.df)
	matrTh1.tmp.df	<- data.frame(name=" ", val=VMin)
	matrTh1.df		<- rbind(matrTh1.df, matrTh1.tmp.df)
}

matrBP.df	<- cbind(ind=1:(NBN*NBN), matrBP.df)
matr3.df	<- cbind(ind=1:(NBN*NBN), matr3.df)
matr5.df	<- cbind(ind=1:(NBN*NBN), matr5.df)
matrTh1.df	<- cbind(ind=1:(NBN*NBN), matrTh1.df)


# NomFic 	<- paste(vRoot, "Fig3a_MRA3.pdf", sep="")
# pdf(file=NomFic, height=10, width=10) 					# Sizes default to 7

ggplot()+ 
	geom_linerange(data=matr3.df,  aes(x=ind,	   ymin=lower, ymax=upper), size=0.9, color="blue")+
	geom_linerange(data=matr5.df,  aes(x=ind+0.25, ymin=lower, ymax=upper), size=0.9, color="red")+
	geom_linerange(data=matrBP.df, aes(x=ind-0.25, ymin=lower, ymax=upper), size=0.9, color="black")+
	geom_hline(yintercept=0, size=1, color="orange")+
	geom_point(data=matrTh1.df, aes(x=ind, y=val), size=1.8, color="green") +		#vs. 0.9
	ylim(VMin+0.003, VMax+0.3)+														# remove the points corresponding to NA (NA replaced by VMin for computations)								
	labs(title="", x="", y="") + scale_x_continuous(breaks=Breaks)+
	theme(axis.text=element_text(size=12))


#	Panneau B
#	noise level k = 0.5%

VMin	<- min(ValMinBP[ , ,2], ValMin3[ , ,2], ValMin5[ , ,2], na.rm=TRUE)		# Minimum on the Y axis
VMax	<- max(ValMaxBP[ , ,2], ValMax3[ , ,2], ValMax5[ , ,2], na.rm=TRUE)		# Maximum on the Y axis

MatrTh1		<- ifelse (MatrTh == -1, VMin, MatrTh)									# exact values

matrBP.df	<- data.frame(mean=NULL, lower=NULL, upper=NULL)		# To draw figures 3a and 3b
matr3.df	<- data.frame(mean=NULL, lower=NULL, upper=NULL)
matr5.df	<- data.frame(mean=NULL, lower=NULL, upper=NULL)
matrTh1.df	<- data.frame(val=NULL)

for (iMat in 1:NBN) {
	matrBP.tmp.df	<- data.frame(name=1:NBN, mean=as.vector(ValMeanBP[1:NBN,iMat,2]), lower=as.vector(ValMinBP[1:NBN,iMat,2]), upper=as.vector(ValMaxBP[1:NBN,iMat,2]))
	matrBP.tmp.df	<- matrBP.tmp.df[-iMat, ]
	matrBP.df		<- rbind(matrBP.df, matrBP.tmp.df)
	matrBP.tmp.df	<- data.frame(name=" ", mean=VMin, lower=VMin, upper=VMin)
	matrBP.df		<- rbind(matrBP.df, matrBP.tmp.df)

	matr3.tmp.df	<- data.frame(name=1:NBN, mean=as.vector(ValMean3[1:NBN,iMat,2]), lower=as.vector(ValMin3[1:NBN,iMat,2]), upper=as.vector(ValMax3[1:NBN,iMat,2]))
	matr3.tmp.df	<- matr3.tmp.df[-iMat, ]
	matr3.df		<- rbind(matr3.df, matr3.tmp.df)
	matr3.tmp.df	<- data.frame(name=" ", mean=VMin, lower=VMin, upper=VMin)
	matr3.df		<- rbind(matr3.df, matr3.tmp.df)

	matr5.tmp.df	<- data.frame(name=1:NBN, mean=as.vector(ValMean5[1:NBN,iMat,2]), lower=as.vector(ValMin5[1:NBN,iMat,2]), upper=as.vector(ValMax5[1:NBN,iMat,2]))
	matr5.tmp.df	<- matr5.tmp.df[-iMat, ]
	matr5.df		<- rbind(matr5.df, matr5.tmp.df)
	matr5.tmp.df	<- data.frame(name=" ", mean=VMin, lower=VMin, upper=VMin)
	matr5.df		<- rbind(matr5.df, matr5.tmp.df)
	
	matrTh1.tmp.df	<- data.frame(name=1:NBN, val=as.vector(MatrTh1[1:NBN,iMat]))
	matrTh1.tmp.df	<- matrTh1.tmp.df[-iMat, ]
	matrTh1.df		<- rbind(matrTh1.df, matrTh1.tmp.df)
	matrTh1.tmp.df	<- data.frame(name=" ", val=VMin)
	matrTh1.df		<- rbind(matrTh1.df, matrTh1.tmp.df)
}

matrBP.df	<- cbind(ind=1:(NBN*NBN), matrBP.df)
matr3.df	<- cbind(ind=1:(NBN*NBN), matr3.df)
matr5.df	<- cbind(ind=1:(NBN*NBN), matr5.df)
matrTh1.df	<- cbind(ind=1:(NBN*NBN), matrTh1.df)

# NomFic 	<- paste(vRoot, "Fig3b_MRA3.pdf", sep="")
# pdf(file=NomFic, height=10, width=10) 					# Sizes default to 7

Titre		<- paste("rij distribution : k = ", 100*Noises[2], "%")					# ri,j distribution
ggplot()+ 
	geom_linerange(data=matr3.df,  aes(x=ind,	   ymin=lower, ymax=upper), size=0.9, color="blue")+
	geom_linerange(data=matr5.df,  aes(x=ind+0.25, ymin=lower, ymax=upper), size=0.9, color="red")+
	geom_linerange(data=matrBP.df, aes(x=ind-0.25, ymin=lower, ymax=upper), size=0.9, color="black")+
	geom_hline(yintercept=0, size=1, color="orange")+
	geom_point(data=matrTh1.df, aes(x=ind, y=val), size=1.8, color="green") +		# vs 0.9
	ylim(VMin+0.003, VMax+0.3)+														# remove the points corresponding to NA (NA replaced by VMin for computations)								
#	labs(title=Titre, x="", y="CI 95%") + scale_x_continuous(breaks=Breaks, labels=DName)+	# y = "IC 95%" for ri,j
	labs(title="", x="", y="") + scale_x_continuous(breaks=Breaks, labels=matrBP.df$name)+
	xlab(OName)+
	theme(axis.text=element_text(size=12))
	
dev.off()



#	Figure 9
#	Impact du bruit de mesure et de la taille des réseaux, évalué à partir de réseaux FRANK (partiel)

NBSets		<-	4				# TA=0, TF in [30, 60, 100, 200]
NBTrials	<-	5				# Nbr of trials by set of values
NBK			<-	2				# Nbr of perturbations (KO, KD -50%)
Noises		<- c(0.1, 0.5)		# Error coefficients
NBT			<- length(Noises)

Methods		<- c("MRA", "TLR")						#, "STEP-Fo", "STEP-Ba", "STEP-Bo")														
NBM			<- length(Methods)							# Nbr of methods to test

ErrorQ		<- array(0, dim=c(NBM+1, NBT))				# Quadratic error, as a function of noise level and method used
Noms 		<- vector(length=NBM+1)
Noms[1] 	<- "k err"
Noms[2:(NBM+1)] 	<- Methods
rownames(ErrorQ)  	<- Noms
for (iT in 1:NBT) {
	ErrorQ[1,iT]  <- Noises[iT]
}

# FileNbr	<- array(dim=c(NBSets+2, NBTrials+2))
					# Column name	: TF 	TA    1 	 2 	   3 	 4 	   5
					# First row		: Seed	 	12345 72090 87577 45648 16637
					# Following rows: TF val TA val File nbrs				
#val			<- paste(vRoot, "FicOrig.csv", sep="")
val			<- paste(vRoot, "FileNbr.csv", sep="")
FileNbr		<- as.matrix(fread(val, data.table=F))
FileErrorQ	<- paste(vRoot, "Frank_ErrorQ.csv", sep="")

for (iSet in 1:NBSets) {
	TF			<- as.numeric(FileNbr[iSet, 1])
	TA			<- as.numeric(FileNbr[iSet, 2])
	NBN			<- TF+TA
	#Res_MRA		<- matrix(0, nrow=NBN, ncol=NBN)			# Résutat avec MRA
	#Res_TLR		<- matrix(0, nrow=NBN, ncol=NBN)			# Résutat avec TLR
	MatrCc		<- matrix(0, nrow=NBN, ncol=NBN)			# Résutat
	
	MatRN		<- array(dim=c(NBN, NBN, NBK, NBT))			# Matrix R with noise   (format MRA)
	MatRN3		<- array(dim=c(NBK*NBN, NBN, NBT))			# Matrix R with noise   (format MRARegress)
	MatRNm1		<- array(dim=c(NBN, NBN, NBK))				# RN ** -1
	dgRNm1M1	<- array(0, dim=c(NBN, NBN, NBK))			# [dg(RN**-1)]**-1
	
	for (iTrial in 1:NBTrials) {
		cat("iSet ", iSet, " TF ", TF, " TA ", TA, " iTrial ", iTrial, "\n")

		val			<- paste(vRoot, "Frank_TF", TF, "_TA", TA, "_", iTrial, "_Sol.rda", sep="")
		load (val)								# Solution

		val		<- paste(vRoot, "Frank_TF", TF, "_TA", TA, "_", iTrial, "_R1.rda", sep="")
		load (val)								# MatRN2	bruit médium (k=0.1)
		MatRN[1:NBN, 1:NBN, 1, 1]	<- MatRN2[1:NBN, 1:NBN]
		MatRN[1:NBN, 1:NBN, 2, 1]  <- MatRN2[((NBK-1)*NBN+1):(NBK*NBN), 1:NBN]
		MatRN3[,,1]					<- MatRN2

		val		<- paste(vRoot, "Frank_TF", TF, "_TA", TA, "_", iTrial, "_R2.rda", sep="")
		load (val)								# MatRN2	bruit fort (k=0.5)
		MatRN[1:NBN, 1:NBN, 1, 2]	<- MatRN2[1:NBN, 1:NBN]
		MatRN[1:NBN, 1:NBN, 2, 2]  <- MatRN2[((NBK-1)*NBN+1):(NBK*NBN), 1:NBN]
		MatRN3[,,2]					<- MatRN2		

		val		<- paste("Frank_TF", TF, "_TA", TA, "_", iTrial, sep="")	
		write.table(val, FileErrorQ, append=TRUE, row.names=FALSE, col.names=FALSE, sep="\t", eol="\r\n")	# Frank_ErrorQ.csv

		for (iT in 1:NBT) {
			for (iK in 1:NBK) {
				MatRNm1[,,iK]		 <- solve(MatRN[,,iK,iT])
				for (i in 1:NBN) {
					dgRNm1M1[i,i,iK] <- 1/MatRNm1[i,i,iK]
				}
			}

			for (iMeth in 1:NBM) {
				Method 	<- Methods[iMeth]
				cat(" iT ", iT, " Method ", Method, " Time ", as.character(Sys.time()), "\n")
			
				if (Method == "MRA") {
					MatrCc	<- -0.5 * (dgRNm1M1[,,1] %*% MatRNm1[,,1] + dgRNm1M1[,,2] %*% MatRNm1[,,2])
				}

				if (Method == "TLR") {
					Mat		<-	MatR2MatExp(MatRN3[,,iT], NBN, NBK)
					MatrCc	<-	MRARegress(Mat$Exp, Mat$Pert)$r
				}

				ss	<- sum((MatrCc-Solution)**2)
				ErrorQ[iMeth+1,iT]	<- sqrt(ss)
			}		# Loop on iMeth		
		}			# Loop on iT
		
		write.table(ErrorQ, FileErrorQ, append=TRUE, row.names=FALSE, col.names=FALSE, sep="\t", eol="\r\n")	# Frank_ErrorQ.csv
	}				# Loop on iTrial
}					# Loop on iSet		

NbNodes		<- vector(length=NBSets)								# Number of nodes
val			<- paste(vRoot, "Frank_ErrorQ.csv", sep="")
# ErrorsA		<- array(unlist(fread(val,data.table=F)),dim=c(NBM,NBT,NBTrials,NBSets))		# Errors stored previously (ErrorQ)
#	dimnames(ErrorsA)	<- list(Methods, Noises, NULL, NULL)

txt <- readLines(val)
txt <- txt[nchar(trimws(txt)) > 0]  	# enlève les lignes vides
txt <- txt[!grepl('^"', txt)]        	# enlève les noms de fichiers
v <- as.numeric(unlist(strsplit(txt, "[[:space:]]+")))
M <- matrix(v, ncol = 2, byrow = TRUE)
M <- M[!(M[,1] == 0.1 & M[,2] == 0.5), ]

ErrorsA <- array(NA_real_, dim = c(2, 2, 5, 4))
k <- 1
for (bloc in 1:4) {
  for (fichier in 1:5) {
    ErrorsA[, , fichier, bloc] <- M[k:(k+1), ]
    k <- k + 2
  }
}

ErrorsM		<- array(0, dim=c(4, NBM, NBT, NBSets))				# Statistics about quadratic errors (mean, sd and log on the different trials)
dimnames(ErrorsM)	<- list(c("mean", "sd", "mean(Log)", "sd(Log)"), Methods, Noises, NULL)

#
#	Mean and standard deviation (computed from the NBTrials results) of quadratic error, as a function of the number of nodes (iSet), the noise level (iT) and the method used (iMeth)
#

for (iSet in 1:NBSets) {
	TF			<- as.numeric(FileNbr[iSet, 1])
	TA			<- as.numeric(FileNbr[iSet, 2])
	NbNodes[iSet]	<- TF+TA
	
	for (iT in 1:NBT)  {
		for (iMeth in 1:NBM) {
			ErrorsM["mean",	iMeth,iT,iSet]  	= round (mean(as.numeric(ErrorsA[iMeth,iT, ,iSet])), 				digits=3)
			ErrorsM["sd", 	iMeth,iT,iSet]  	= round ((var(as.numeric(ErrorsA[iMeth,iT, ,iSet])))**0.5, 		digits=3)
			ErrorsM["mean(Log)",iMeth,iT,iSet] 	= round (mean(log(1+as.numeric(ErrorsA[iMeth,iT, ,iSet]))),  		digits=3)	# log1p : Ln(1+x)
			ErrorsM["sd(Log)",	iMeth,iT,iSet] 	= round ((var(log(1+as.numeric(ErrorsA[iMeth,iT, ,iSet]))))**0.5,	digits=3)
		}		# Loop on iMeth
	}			# Loop on iT
}				# Loop on iSet

#	Panneau A
#	TA = 0		k = 0.1

Err1TA0_MRA.df		<- data.frame(NbNodes=NbNodes[1:NBSets], avg=ErrorsM["mean","MRA",1,1:NBSets], 	 	sd=ErrorsM["sd","MRA",1,1:NBSets], 		Method="MRA")
Err1TA0_TLR.df		<- data.frame(NbNodes=NbNodes[1:NBSets], avg=ErrorsM["mean","TLR",1,1:NBSets], 	 	sd=ErrorsM["sd","TLR",1,1:NBSets], 		Method="TLR")
Err1TA0.df			<- rbind(Err1TA0_MRA.df, Err1TA0_TLR.df)

# NomFic 	<- paste(vRoot, "Resultats/", "FRANK_Err1TA0.pdf", sep="")
# pdf(file=NomFic, height=6,width=3) 							# Sizes default to 7

ggplot() + 
	geom_line(data=Err1TA0.df, aes(x=NbNodes, y=avg, colour=factor(Method)), show.legend=FALSE)+	# legend removed to get more space (4 figs. with the same caption, added with Inkscape)
	xlab("Number of nodes") + ylab("Average squared error") + ggtitle("Squared Error, k = 10%, TA = 0")   +
	scale_colour_manual(name="Method", values=c("MRA"="blue", "TLR"="red")) +
	geom_ribbon(data=Err1TA0_TLR.df,  aes(x=NbNodes, ymin=avg-sd, ymax=avg+sd), fill="grey70", 	alpha=0.3) +
	theme(axis.text.x = element_text(color="black")) +
	theme(axis.text.y = element_text(color="black")) +
	theme(text = element_text(size = 10))					# width = 3 (4x8 cm) vs size = 13 width = 6 (8x8 cm)
	
dev.off()


#	Panneau B
#	TA = TF		k = 0.1

#	Il faudrait compléter les ensembles testés, pour inclure les cas TA=TF

#	Panneau C
#	TA = 0		k = 0.5

Err5TA0_MRA.df		<- data.frame(NbNodes=NbNodes[1:NBSets], avg=ErrorsM["mean(Log)","MRA",2,1:NBSets], 	  	sd=ErrorsM["sd(Log)","MRA",2,1:NBSets], 		Method="MRA")
Err5TA0_TLR.df		<- data.frame(NbNodes=NbNodes[1:NBSets], avg=ErrorsM["mean(Log)","TLR",2,1:NBSets], 	  	sd=ErrorsM["sd(Log)","TLR",2,1:NBSets], 		Method="TLR")
Err5TA0.df			<- rbind(Err5TA0_MRA.df, Err5TA0_TLR.df)

# NomFic 	<- paste(vRoot, "Resultats/", "FRANK_Err5TA0.pdf", sep="")
# pdf(file=NomFic, height=6,width=3.5) 							# Sizes default to 7

ggplot() + 
	geom_line(data=Err5TA0.df, aes(x=NbNodes, y=avg, colour=factor(Method)), show.legend=FALSE)+
	xlab("Number of nodes") + ylab("Log(1+Average squared error)") + ggtitle("Log(1+Sq. Error), k = 50%, TA = 0") +
	scale_colour_manual(name="Method", values=c("MRA"="blue", "TLR"="red")) +
	geom_ribbon(data=Err5TA0_TLR.df,  aes(x=NbNodes, ymin=avg-sd, ymax=avg+sd), fill="grey70", 	alpha=0.3) +
	theme(axis.text.x = element_text(color="black")) +
	theme(axis.text.y = element_text(color="black")) +
	theme(text = element_text(size = 10))					# width = 3.5 (4x8 cm) vs size = 13 width = 6 (8x8 cm)
	
dev.off()

#	Panneau D
#	TA = TF		k = 0.5

#	Il faudrait compléter les ensembles testés, pour inclure les cas TA=TF



#	Figure 10
#	Un réseau 10 gènes du Dream Challenge 4

# vRoot		<- "C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/"
val			<- paste(vRoot, "Perturb2.rda", sep="")
load(val)									# Perturb2

val			<- paste(vRoot, "MatExp_10_1.rda", sep="")
load(val)									# MatExp

val			<- paste(vRoot, "Solution_10_1.rda", sep="")
load(val)									# Solution

Res_10_1	<- MRARegress(MatExp, Perturb2)
RetC 		<- Classify(Res_10_1) 					#, Lbda=0.38)
A			<- DrawDiscr (RetC, Solution)



#	Tableau 1
#	Découverte des arêtes des réseaux du "Dream Challenge 4" (DC4)

Fics		<-  matrix("", nrow=2, ncol = 5)
Fics[1, ]	<-  c("10_1", "10_2", "10_3", "10_4", "10_5")
Fics[2, ]	<-  c("100_1", "100_2", "100_3", "100_4", "100_5")					# Files to study
nbFics		<-  5
NBK			<- 	2							# 2 perturbations
Methods		<-	c("MRA-KD",	"MRA-KO", "LSE_CI",	"LASSO", "TLR",	"STEP-Fo", "STEP-Ba", "STEP-Bo", "CLR", "ARACNE", "MRNET")

for (i in 1:2) {
	if (i == 1) {
		val		<- paste(vRoot, "Perturb_10.rda", sep="")
		NBN		<- 10
	} else {
		val		<- paste(vRoot, "Perturb_100.rda", sep="")
		NBN 	<- 100
	}
	load(val)								# Perturb_10 ou 100
	if (i == 1) {
		Perturbation = Perturb_10
	} else {
		Perturbation = Perturb_100
	}
	
	MatRN		<- array(dim=c(NBN, NBN, NBK))
	pX   <- array(dim=c(NBN-1, NBN-1, NBN))						# ligne (iRow), colonne (iCol), n° matrice (iMat)
	pY   <- array(dim=c(NBN-1, NBN))								# ligne, n° matrice
	gX   <- array(dim=c(NBK*(NBN-1), NBN-1, NBN))					# ligne (iRow), colonne (iCol), n° matrice (iMat)
	gY   <- array(dim=c(NBK*(NBN-1), NBN))							# ligne, n° matrice	
	MatRNm1		<- array(dim=c(NBN, NBN, NBK))						# RN ** -1
	dgRNm1M1	<- array(0, dim=c(NBN, NBN, NBK))					# [dg(RN**-1)]**-1
	Res1		<- array(0, dim=c(NBN, NBN))
	Results		<- array(0, dim=c(5, length(Methods)))				# Résutat moyens
	Results1	<- array(0, dim=c(2, nbFics, length(Methods)))		# Résultats unitaires (Se, Sp)
	
	for (j in 1:nbFics) {
		val		<- paste(vRoot, "MatExp_", Fics[i,j], ".rda", sep="")
		load(val)							# MatExp
		val		<- paste(vRoot, "Solution_", Fics[i,j], ".rda", sep="")
		load(val)							# Solution
		
		MatRN[1:NBN, 1:NBN, 2]	<-	2 * (MatExp[1:NBN, 2:(NBN+1)]-MatExp[1:NBN, 1])        / (MatExp[1:NBN, 2:(NBN+1)]+MatExp[1:NBN, 1])				# KO
		MatRN[1:NBN, 1:NBN, 1]	<-	2 * (MatExp[1:NBN, (NBN+2):(2*NBN+1)]-MatExp[1:NBN, 1]) / (MatExp[1:NBN, (NBN+2):(2*NBN+1)]+MatExp[1:NBN, 1])		# KD			

		for (k in 1:NBK) {
			for (iMat in 1:NBN) {
				col <- 1:NBN
				col <- col[-iMat]
			
				for (iRow in 1:(NBN-1)) {
					pY[iRow, iMat] = MatRN[iMat, col[iRow], k]
				
					for (iCol in 1:NBN-1) {
						pX[iRow, iCol, iMat] = MatRN[col[iCol], col[iRow], k]
					}
				}
			}
		
			gX[((NBN-1)*(k-1)+1) : ((NBN-1)*k), ,]  <- pX[, ,]
			gY[((NBN-1)*(k-1)+1) : ((NBN-1)*k), ]   <- pY[, ]

			MatRNm1[,,k]	<- solve(MatRN[,,k])
			for (iL in 1:NBN) {
				dgRNm1M1[iL,iL,k] <- 1/MatRNm1[iL,iL,k]
			}
		}

		meth	<- 3			# Méthode LSE
		for (iMat in 1:NBN) {
			col <- 1:NBN
			col <- col[-iMat]
					
			Form <- paste("gY[,",iMat,"]~gX[,,",iMat,"]+0")
			Res1[iMat, col] 	<- (lm(as.formula(Form)))$coefficients
			Res1[iMat, iMat]  <- -1			# L'élément diagonal est mis à  -1
		}
		
		RetC 	<- Classify(abs(Res1), MethDiscr="Top", Lbda=20)
		Sc		<- Score(RetC, Solution)$Scores2
		Results1[1, j, meth]	<-	Sc$Se
		Results1[2, j, meth]	<-	Sc$Sp

		meth	<- 2			# Méthode MRA_KO
		Res		<-	-dgRNm1M1[,,1] %*% MatRNm1[,,1]			# MRA_KO		Attention à l'inversion des indices des méthodes
		RetC 	<- Classify(abs(Res), MethDiscr="Top", Lbda=20)
		Sc		<- Score(RetC, Solution)$Scores2
		Results1[1, j, meth]	<-	Sc$Se
		Results1[2, j, meth]	<-	Sc$Sp

		meth	<- 1			# Méthode MRA_KD
		Res		<-	-dgRNm1M1[,,2] %*% MatRNm1[,,2]			# MRA_KD		Attention à l'inversion des indices des méthodes
		RetC 	<- Classify(abs(Res), MethDiscr="Top", Lbda=20)
		Sc		<- Score(RetC, Solution)$Scores2
		Results1[1, j, meth]	<-	Sc$Se
		Results1[2, j, meth]	<-	Sc$Sp

		meth	<- 5			# Méthode TLR
		Res		<- MRARegress(MatExp, Perturb=Perturbation)
		RetC	<- Classify(Res)
		Sc		<- Score(RetC, Solution)$Scores2
		Results1[1, j, meth]	<-	Sc$Se
		Results1[2, j, meth]	<-	Sc$Sp

		meth	<- 4			# Méthode LASSO
		Res		<- MRARegress(MatExp, Perturb=Perturbation, Method="LASSO")
		RetC	<- Classify(Res)
		Sc		<- Score(RetC, Solution)$Scores2
		Results1[1, j, meth]	<-	Sc$Se
		Results1[2, j, meth]	<-	Sc$Sp

		meth	<- 6			# Méthode STEP-Fo
		Res		<- MRARegress(MatExp, Perturb=Perturbation, Method="STEP", Hyp_Step="Fo")
		RetC	<- Classify(Res)
		Sc		<- Score(RetC, Solution)$Scores2
		Results1[1, j, meth]	<-	Sc$Se
		Results1[2, j, meth]	<-	Sc$Sp

		meth	<- 7			# Méthode STEP-Ba
		Res		<- MRARegress(MatExp, Perturb=Perturbation, Method="STEP", Hyp_Step="Ba")
		RetC	<- Classify(Res)
		Sc		<- Score(RetC, Solution)$Scores2
		Results1[1, j, meth]	<-	Sc$Se
		Results1[2, j, meth]	<-	Sc$Sp
		
		meth	<- 8			# Méthode STEP-Bo
		Res		<- MRARegress(MatExp, Perturb=Perturbation, Method="STEP", Hyp_Step="Bo")
		RetC	<- Classify(Res)
		Sc		<- Score(RetC, Solution)$Scores2
		Results1[1, j, meth]	<-	Sc$Se
		Results1[2, j, meth]	<-	Sc$Sp
		
		meth	<- 9			# Méthode CLR
		Res		<- MRARegress(MatExp, Perturb=Perturbation, Method="CLR")
		RetC	<- Classify(Res)
		Sc		<- Score(RetC, Solution)$Scores2
		Results1[1, j, meth]	<-	Sc$Se
		Results1[2, j, meth]	<-	Sc$Sp
		
		meth	<- 10			# Méthode ARACNE
		Res		<- MRARegress(MatExp, Perturb=Perturbation, Method="ARACNE")
		RetC	<- Classify(Res)
		Sc		<- Score(RetC, Solution)$Scores2
		Results1[1, j, meth]	<-	Sc$Se
		Results1[2, j, meth]	<-	Sc$Sp
		
		meth	<- 11			# Méthode MRNET
		Res		<- MRARegress(MatExp, Perturb=Perturbation, Method="MRNET")
		RetC	<- Classify(Res)
		Sc		<- Score(RetC, Solution)$Scores2
		Results1[1, j, meth]	<-	Sc$Se
		Results1[2, j, meth]	<-	Sc$Sp
	}
	
	for (meth in 1:length(Methods)) {
		Results[1,meth]	<- round (mean(Results1[1, ,meth]), 3)			# Moyenne Se
		Results[2,meth]	<- round (var (Results1[1, ,meth])**0.5, 3)	# Sd Se
		Results[3,meth]	<- round (mean(Results1[2, ,meth]), 3)			# Moyenne Sp
		Results[4,meth]	<- round (var (Results1[2, ,meth])**0.5, 3)	# Sd Sp
		Results[5,meth]	<- Results[1,meth]+Results[3,meth]-1			# D2D
	}
	Fic <- paste(vRoot, "Results", i, ".rda")
	save(Results, file=Fic)
}

#
#	Calcul des AUC :
# Tracer la courbe Se = f(1-Sp) et cacluer l'aire sous la courbe par -trapz(1-Score["Specif",,iMeth], Score["Sensib",,iMeth])



#	Figure 11
#	Performances comparées en fonction de la taille du réseau et du niveau de bruit
#	Performances évaluées à partir de réseaux FRANK (partiel), pour plusieurs méthodes  (partiel)
#

NBSets		<-	3				# TA=0, TF in [30, 60, 100]
NBTrials	<-	5				# Nbr of trials by set of values
NBK			<-	2				# Nbr of perturbations (KO, KD -50%)
Noises		<- c(0.1, 0.5)		# Error coefficients
NBT			<- length(Noises)

Methods		<- c("MRA", "TLR", "LASSO", "STEP-Fo", "ARACNE")									
NBM			<- length(Methods)							# Nbr of methods to test
Results1	<- array(0, dim=c(NBTrials, NBSets, NBT, NBM))		# Résultat (D2D) pour un fichier
Results		<- array(0, dim=c(NBSets, NBT, NBM))		# Résultat (D2D) moyenne sur les fichiers du Set
NbNodes		<- array(0, dim=c(NBSets))					# Nombre de noeuds

vRoot		<- "C:/Users/jean-pierre.borg/IRCM/These/Recherche/Packages/MRARegress/data/"

val			<- paste(vRoot, "FileNbr.csv", sep="")
FileNbr		<- as.matrix(fread(val, data.table=F))

for (iSet in 1:NBSets) {
	TF			<- as.numeric(FileNbr[iSet, 1])
	TA			<- as.numeric(FileNbr[iSet, 2])
	NBN			<- TF+TA
	NbNodes[iSet]	<- NBN
	MatrCc		<- matrix(0, nrow=NBN, ncol=NBN)			# Résutat
	
	MatRN		<- array(dim=c(NBN, NBN, NBK, NBT))			# Matrix R with noise   (format MRA)
	MatRN3		<- array(dim=c(NBK*NBN, NBN, NBT))			# Matrix R with noise   (format MRARegress)
	MatRNm1		<- array(dim=c(NBN, NBN, NBK))				# RN ** -1
	dgRNm1M1	<- array(0, dim=c(NBN, NBN, NBK))			# [dg(RN**-1)]**-1
	
	for (iTrial in 1:NBTrials) {
		cat("iSet ", iSet, " TF ", TF, " TA ", TA, " iTrial ", iTrial, "\n")

		val			<- paste(vRoot, "Frank_TF", TF, "_TA", TA, "_", iTrial, "_Sol.rda", sep="")
		load (val)								# Solution
		
		Solut1		<- array(0, dim=c(NBN,NBN))		# Solution digitalisée (0,1). Méthode décrite dans "EssaisRevision_Rev2.R". On choisit un seuil our avoir une sparsité max de 10%	
		ThSol 		<- quantile(abs(Solution), probs=c(0.90))	# We choose the threshold to have a sparsity max of 10% 
		Solut1[which(abs(Solution[]) > ThSol)]	<- 1	
		#P = sum(Solut1)	- NBN						# Nbr. of true positives (excludes the diagonal)
		#N = NBN*(NBN-1) - P							# Nbr. of true negatives
		#cat ("NBN ", NBN, " Sparsity ", P/(NBN*(NBN-1)), "\n")		
		
		val		<- paste(vRoot, "Frank_TF", TF, "_TA", TA, "_", iTrial, "_R1.rda", sep="")
		load (val)								# MatRN2	bruit médium (k=0.1)
		MatRN[1:NBN, 1:NBN, 1, 1]	<- MatRN2[1:NBN, 1:NBN]
		MatRN[1:NBN, 1:NBN, 2, 1]  <- MatRN2[((NBK-1)*NBN+1):(NBK*NBN), 1:NBN]
		MatRN3[,,1]					<- MatRN2

		val		<- paste(vRoot, "Frank_TF", TF, "_TA", TA, "_", iTrial, "_R2.rda", sep="")
		load (val)								# MatRN2	bruit fort (k=0.5)
		MatRN[1:NBN, 1:NBN, 1, 2]	<- MatRN2[1:NBN, 1:NBN]
		MatRN[1:NBN, 1:NBN, 2, 2]  <- MatRN2[((NBK-1)*NBN+1):(NBK*NBN), 1:NBN]
		MatRN3[,,2]					<- MatRN2

		for (iT in 1:NBT) {
			for (iK in 1:NBK) {
				MatRNm1[,,iK]		 <- solve(MatRN[,,iK,iT])
				for (i in 1:NBN) {
					dgRNm1M1[i,i,iK] <- 1/MatRNm1[i,i,iK]
				}
			}

			for (iMeth in 1:NBM) {	
				Method 	<- Methods[iMeth]			
				cat(" iT ", iT, " Method ", Method, " Time ", as.character(Sys.time()), "\n")
			
				if (Method == "MRA") {
					MatrCc	<- -0.5 * (dgRNm1M1[,,1] %*% MatRNm1[,,1] + dgRNm1M1[,,2] %*% MatRNm1[,,2])				
					RetC 	<- Classify(abs(MatrCc), MethDiscr="Top", Lbda=20)
					Sc		<- Score(RetC, Solut1)$Scores2
					Results1 [iTrial, iSet, iT, iMeth]		<- Sc$Dst			#	D2D
				} else {
					Mat		<-	MatR2MatExp(MatRN3[,,iT], NBN, NBK)
				}

				if (Method == "TLR") {
					MatrCc	<-	MRARegress(Mat$Exp, Mat$Pert)
					#RetC 	<- Classify(abs(MatrCc$r), MethDiscr="Threshold", Lbda=0.25)
					RetC 	<- Classify(MatrCc)					
					Sc		<- Score(RetC, Solut1)$Scores2
					Results1 [iTrial, iSet, iT, iMeth]		<- Sc$Dst			#	D2D					
				}

 				if (Method == "LASSO") {
					#MatrCc	<-	MRARegress(Mat$Exp, Mat$Pert, Method="LASSO")
					RetC 	<- Classify(abs(MatrCc$r), MethDiscr="Threshold", Lbda=0)
					RetC 	<- Classify(MatrCc)
					Sc		<- Score(RetC, Solut1)$Scores2
					Results1 [iTrial, iSet, iT, iMeth]		<- Sc$Dst			#	D2D					
				}

 				if (Method == "STEP-Fo") {
					MatrCc	<-	MRARegress(Mat$Exp, Mat$Pert, Method="STEP", Hyp_Step="Fo")
					RetC 	<- Classify(abs(MatrCc$r), MethDiscr="Threshold", Lbda=0)
					RetC= Classify(MatrCc)
					Sc		<- Score(RetC, Solut1)$Scores2
					Results1 [iTrial, iSet, iT, iMeth]		<- Sc$Dst			#	D2D					
				}

 				if (Method == "ARACNE") {
					MatrCc	<-	MRARegress(Mat$Exp, Mat$Pert, Method="ARACNE")
					RetC 	<- Classify(abs(MatrCc$r), MethDiscr="Top", Lbda=20)						
					Sc		<- Score(RetC, Solut1)$Scores2
					Results1 [iTrial, iSet, iT, iMeth]		<- Sc$Dst			#	D2D					
				}				
			}		# Loop on iMeth		
		}			# Loop on iT
	}				# Loop on iTrial
}					# Loop on iSet

for (iMeth in 1:NBM)
	for (iT in 1:NBT)
		for (iSet in 1:NBSets)
			Results[iSet, iT, iMeth]	<- round(mean(Results2[ , iSet, iT, iMeth]), 3)

#	Fic <- paste(vRoot, "Results1_Fig11.rda")
#	save(Results1, file=Fic)
#	Fic <- paste(vRoot, "Results_Fig11.rda")
#	save(Results, file=Fic)
#	
D2DTA0_L1_MRA.df		<- data.frame(NbNodes=NbNodes[1:NBSets], dst=Results[1:NBSets, 1, 1], Method="MRA")
D2DTA0_L1_TLR.df		<- data.frame(NbNodes=NbNodes[1:NBSets], dst=Results[1:NBSets, 1, 2], Method="TLR")
D2DTA0_L1_LASSO.df		<- data.frame(NbNodes=NbNodes[1:NBSets], dst=Results[1:NBSets, 1, 3], Method="LASSO")
D2DTA0_L1_STEP.df		<- data.frame(NbNodes=NbNodes[1:NBSets], dst=Results[1:NBSets, 1, 4], Method="STEP")
D2DTA0_L1_ARACNE.df		<- data.frame(NbNodes=NbNodes[1:NBSets], dst=Results[1:NBSets, 1, 5], Method="ARACNE")
D2DTA0_L1.df			<- rbind(D2DTA0_L1_MRA.df, D2DTA0_L1_TLR.df, D2DTA0_L1_LASSO.df, D2DTA0_L1_STEP.df, D2DTA0_L1_ARACNE.df)

D2DTA0_L1_MRA.df		<- data.frame(NbNodes=NbNodes[1:3], dst=Results[1:3, 1, 1], Method="MRA")
D2DTA0_L1_TLR.df		<- data.frame(NbNodes=NbNodes[1:3], dst=Results[1:3, 1, 2], Method="TLR")
D2DTA0_L1_LASSO.df		<- data.frame(NbNodes=NbNodes[1:3], dst=Results[1:3, 1, 3], Method="LASSO")
D2DTA0_L1_STEP.df		<- data.frame(NbNodes=NbNodes[1:3], dst=Results[1:3, 1, 4], Method="STEP")
D2DTA0_L1_ARACNE.df		<- data.frame(NbNodes=NbNodes[1:3], dst=Results[1:3, 1, 5], Method="ARACNE")
D2DTA0_L1.df			<- rbind(D2DTA0_L1_MRA.df, D2DTA0_L1_TLR.df, D2DTA0_L1_LASSO.df, D2DTA0_L1_STEP.df, D2DTA0_L1_ARACNE.df)


ggplot() + 
	geom_line(data=D2DTA0_L1.df, aes(x=NbNodes, y=dst, colour=factor(Method)), show.legend=TRUE)+
	xlab("Number of nodes") + ylab("D2D") + ggtitle("Dist. to the diagonal, k=10%, TA=0")   +
	scale_colour_manual(name="Method", values=c("MRA"="blue", "TLR"="red", "ARACNE"="maroon", "STEP"="yellow", "LASSO"="green")) +
	theme(axis.text.x = element_text(color="black")) +
	theme(axis.text.y = element_text(color="black")) +
	theme(text = element_text(size = 10))					# width = 3 (4x8 cm) vs size = 13 width = 6 (8x8 cm)
	
dev.off()

D2DTA0_L2_MRA.df		<- data.frame(NbNodes=NbNodes[1:3], dst=Results[1:3, 2, 1], Method="MRA")
D2DTA0_L2_TLR.df		<- data.frame(NbNodes=NbNodes[1:3], dst=Results[1:3, 2, 2], Method="TLR")
D2DTA0_L2_LASSO.df		<- data.frame(NbNodes=NbNodes[1:3], dst=Results[1:3, 2, 3], Method="LASSO")
D2DTA0_L2_STEP.df		<- data.frame(NbNodes=NbNodes[1:3], dst=Results[1:3, 2, 4], Method="STEP")
D2DTA0_L2_ARACNE.df		<- data.frame(NbNodes=NbNodes[1:3], dst=Results[1:3, 2, 5], Method="ARACNE")
D2DTA0_L2.df			<- rbind(D2DTA0_L2_MRA.df, D2DTA0_L2_TLR.df, D2DTA0_L2_LASSO.df, D2DTA0_L2_STEP.df, D2DTA0_L2_ARACNE.df)


ggplot() + 
	geom_line(data=D2DTA0_L2.df, aes(x=NbNodes, y=dst, colour=factor(Method)), show.legend=TRUE)+
	xlab("Number of nodes") + ylab("D2D") + ggtitle("Dist. to the diagonal, k=50%, TA=0")   +
	scale_colour_manual(name="Method", values=c("MRA"="blue", "TLR"="red", "ARACNE"="maroon", "STEP"="yellow", "LASSO"="green")) +
	theme(axis.text.x = element_text(color="black")) +
	theme(axis.text.y = element_text(color="black")) +
	theme(text = element_text(size = 10))	

dev.off()



#	Tableau 4
#	Résultats ANOVA

#	Panneau A :	Réseau 3 Kinases

#	Réseau 3 noeuds (pRaF, ppMEK, ppERK), décrit dans l'article "Impact of measurement noise, experimental design,
#	and estimation methods on Modular Response -- Analysis based network reconstruction" par C. Thomaseth et al (Scientific Reports, 2018).
#	Voir Annexe 3 §2.3

F3n <- function(P,X)
	c(F1 = (25*(20*P[1]-X[1])/((20+20*P[1]-X[1])*(5+X[3]))-10*X[1]/(20+X[1])), 
	  F2 = (3*(20*P[2]-X[2])*X[1]/(20+20*P[2]-X[2])-10*X[2]/(20+X[2])), 
	  F3 = ((20*P[3]-X[3])*X[2]/(20+20*P[3]-X[3])-10*X[3]/(20+X[3])))
#	où X représente les niveaux d'expression de pRaF, ppMEK, ppERK respectivement.

nbN			<- 3
nbReplic	<- 2
Perturbs	<- c(0.2, 0.9, 0.99)
nbPert		<- length(Perturbs)
P0 			<- c(1, 1, 1)					# Valeur des paramètres
st 			<- c(0.1, 0.1, 0.1)
NName		<- c("pRAF", "ppMEK", "ppERK")

MExp_3_02_09_099 	<- MExp (nbN, Perturbs, P0, F3n, st)$Exp		# Simulation de 3 perturbations appliquées à chacun des noeuds. Réseau sans bruit.
Pert_3_02_09_099	<- MExp (nbN, Perturbs, P0, F3n, st)$Pert

#	On simule deux réplicas
nbReplic	<- 2
nbPert		<- length(Perturbs)
XMesNPMoy	<- mean(abs(MExp_3_02_09_099[1, ]))		# Moyenne des expressions non perturbées
Sd			<- 0.01*XMesNPMoy				# Ecart-type du bruit
MExp_		<- matrix(0, nrow=nbN, ncol=1)

set.seed(12345)
MExp_[ ,1]	<-	MExp_3_02_09_099[ ,1]		# La 1° colonne (base) est à part, de façon à ajouter les 9 colonnes perturbées de chaque réplicat
Pert_		<- c("Base")

for (iRep in 1:nbReplic) {
	MExp_3_2	<- MExp_3_02_09_099 + rnorm(nbN*(nbN*nbPert+1), mean=0, sd=Sd)
	MExp_		<- cbind(MExp_, MExp_3_2[ ,2:(nbPert*nbN+1)])	# On ajoute à la colonne basale, les 9 colonnes perturbées correspondant à chaque réplicat
	Pert_		<- c(Pert_, Pert_3_02_09_099  [2:(nbPert*nbN+1)])
}

for (iNode in 1:nbN) {
	orig	<- paste("N", iNode, sep="")
	Pert_	<- str_replace_all(Pert_, orig, NName[iNode])
}

Res_3G		<- MRARegress(MExp_, Pert_, NodeName=NName, Relative=FALSE)
Res_O1_3G	<- MRARegress(MExp_3_02_09_099, Pert_3_02_09_099, Relative=FALSE)	
Res_O2_3G	<- MRARegress(MExp_3_02_09_099, Pert_3_02_09_099, Relative=FALSE, Method="Order2")	
round (Res_3G$ANOVA[[1]], 2)

#	Panneau B :	Réseau linéaire 3 noeuds (d'après suggestion de Alon)
#	Voir Annexe 3 §2.3

# 	Taux de production des protéines :
a1	= 0.5 			# (unité de concentration par unité de temps)
a2	= 0.4
a3	= 0.3

#	Taux de dégradation des protéines :
b1	= 0.1 			# (unité de temps inverse : 1/t)
b2	= 0.2
b3	= 0.15

#	Taux de répression entre les gènes :
c12	= 0.05			#  (pour la répression de x2 par x1)
c13	= 0.03
c21	= 0.04
c23	= 0.02
c31	= 0.06
c32	= 0.07

F3Ln	<- function(P,X)
	c(F1 = (a1*P[1] -  b1*X[1] - c12*X[2] - c13*X[3]),
	  F2 = (a2*P[2] - c21*X[1] -  b2*X[2] - c23*X[3]),
	  F3 = (a3*P[3] - c31*X[1] - c32*X[2] -  b3*X[3]))
	  
nbN			<-	3
Perturbs	<- c(0.2, 0.9, 0.99)
P0 			<- c(1, 1, 1)					# Valeur des paramètres
st 			<- c(0.1, 0.1, 0.1)

MExp_3L_02_09_099 	<- MExp (nbN, Perturbs, P0, F3Ln, st)$Exp		# Simulation de 3 perturbations "linéaires" appliquées à chacun des noeuds. Réseau sans bruit.
Pert_3L_02_09_099	<- MExp (nbN, Perturbs, P0, F3Ln, st)$Pert  

#	On simule deux réplicas
nbReplic	<- 2
nbPert		<- length(Perturbs)
XMesNPMoy	<- mean(abs(MExp_3L_02_09_099[1, ]))		# Moyenne des expressions non perturbées
Sd			<- 0.01*XMesNPMoy				# Ecart-type du bruit
MExp_		<- matrix(0, nrow=nbN, ncol=1)

set.seed(12345)
MExp_[ ,1]	<-	MExp_3L_02_09_099[ ,1]
Pert_		<- c("Base")

for (iRep in 1:nbReplic) {
	MExp_3_2	<- MExp_3L_02_09_099 + rnorm(nbN*(nbN*nbPert+1), mean=0, sd=Sd)
	MExp_		<- cbind(MExp_, MExp_3_2[ ,2:(nbPert*nbN+1)])
	Pert_		<- c(Pert_, Pert_3L_02_09_099  [2:(nbPert*nbN+1)])
}

Res_3L		<- MRARegress(MExp_, Pert_, Relative=FALSE)
Res_O1_3L	<- MRARegress(MExp_3L_02_09_099, Pert_3L_02_09_099, Relative=FALSE)	
Res_O2_3L	<- MRARegress(MExp_3L_02_09_099, Pert_3L_02_09_099, Relative=FALSE, Method="Order2")
round (Res_3L$ANOVA[[1]], 2)

#	Panneau C : Réseau 4 noeuds, décrit dans l'article : "Inferring dynamic architecture of cellular networks using time series of gene expression, protein and metabolite data"
#	(Eduardo Sontag et al, 2004
#	Voir Annexe 3 §2.3

# Constantes liées à la cinétique des réactions	
KA14 <- 1.6			# Constantes de Michaelis en nM
KI12 <- 0.5
KA24 <- 1.6
KA32 <- 1.5
KI31 <- 0.7
KA43 <- 0.15
KD1	 <- 30
KD2	 <- 60
KD3	 <- 10
KD4	 <- 50

A14	 <- 4			# Coeff. sans dimension
n12	 <- 1
n14	 <- 2
A24	 <- 4
n24	 <- 2
A32	 <- 5
n31	 <- 1
n32	 <- 2
A43	 <- 2
n43	 <- 2

VS1	 <- 1			# Taux d'enzyme maximal en nM.Hr-1 : "Inferring dynamic architecture ... "
VS2	 <- 0.7
VS3	 <- 0.6
VS4	 <- 0.8
VD1	 <- 40
VD2	 <- 100
VD3	 <- 30
VD4	 <- 100

# load ("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\Solution_4G.rda")		# Solution_4G

F4n <- function(P,X)
   c(F1 = (P[1]*(1+A14*(X[4]/KA14)^n14)) / ((1+(X[4]/KA14)^n14)*(1+(X[2]/KI12)^n12)) - (VD1*X[1]) / (KD1+X[1]),
	 F2 = (P[2]*(1+A24*(X[4]/KA24)^n24)) /  (1+(X[4]/KA24)^n24) - (VD2*X[2]) / (KD2+X[2]),
	 F3 = (P[3]*(1+A32*(X[2]/KA32)^n32)) / ((1+(X[2]/KA32)^n32)*(1+(X[1]/KI31)^n31)) - (VD3*X[3]) / (KD3+X[3]),
	 F4 = (P[4]*(1+A43*(X[3]/KA43)^n43)) /  (1+(X[3]/KA43)^n43) - (VD4*X[4]) / (KD4+X[4]))

nbN		 	<- 4
Perturbs 	<- c(0.2, 0.9, 0.99)
P0		 	<- c(VS1, VS2, VS3, VS4)	
st 		 	<- c(0.1, 0.1, 0.1, 0.1)

MExp_4_02_09_099 	<- MExp (nbN, Perturbs, P0, F4n, st)$Exp			# Simulation de 3 perturbations appliquées à chacun des noeuds.
Pert_4_02_09_099 	<- MExp (nbN, Perturbs, P0, F4n, st)$Pert

#	On simule deux réplicas
nbReplic	<- 2
nbPert		<- length(Perturbs)
XMesNPMoy	<- mean(abs(MExp_4_02_09_099[1, ]))		# Moyenne des expressions non perturbées
Sd			<- 0.01*XMesNPMoy				# Ecart-type du bruit

MExp_		<- matrix(0, nrow=nbN, ncol=1)
set.seed(12345)
MExp_[ ,1]	<-	MExp_4_02_09_099[ ,1]
Pert_		<- c("Base")

for (iRep in 1:nbReplic) {
	MExp_4_2	<- MExp_4_02_09_099 + rnorm(nbN*(nbN*nbPert+1), mean=0, sd=Sd)
	MExp_		<- cbind(MExp_, MExp_4_2[ ,2:(nbPert*nbN+1)])
	Pert_		<- c(Pert_, Pert_4_02_09_099  [2:(nbPert*nbN+1)])
}

Res_4G		<- MRARegress(MExp_, Pert_, Relative=FALSE)
Res_O1_4G	<- MRARegress(MExp_4_02_09_099, Pert_4_02_09_099, Relative=FALSE)
Res_O2_4G	<- MRARegress(MExp_4_02_09_099, Pert_4_02_09_099, Relative=FALSE, Method="Order2")
round (Res_4G$ANOVA[[1]], 2)

# 	Panneau D : Réseau 6 noeuds 'Cascade MAPK'.
#	"Kinases MKK, MKKK et MAPK + formes bi-phosphorylatées, décrit dans l'article : 
#   "Untangling the wires: A strategy to trace functional interactions in signaling and gene networks"
#		(Boris N. Kholodenko et al, PNAS 2002
#	Voir Annexe 3 §2.3

# Constantes liées à la cinétique des réactions
KC1	 <- 1			# Taux catalytiques en s-1
KC2	 <- 15
KC5	 <- 1
KC6	 <- 15
KC9	 <- 1
KC10 <- 15

K11	 <- 300			# Constantes de Michaelis en nM
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

A	 <- 5			# Coeff. sans dimension

V3	 <- 18.8		# Taux d'enzyme maximal en nM.s-1
V4	 <- 16.4
V7	 <- 18.8
V8	 <- 16.4
V11	 <- 8.4
V12	 <- 7.3

MKKK0	<- 200		# X1+X7+X2	: concentration totale de la protéine MKKK
MKK0	<- 180		# X3+X8+X4	: concentration totale de la protéine MKK
MAPK0	<- 360		# X5+X9+X6	: concentration totale de la protéine MAPK
U	<- 20

# load ("C:\\Users\\jean-pierre.borg\\IRCM\\These\\Recherche\\Packages\\MRAregress\\data\\Solution_6K.rda")		# Solution_6K

F6n 	<- function(P,X)
   c(F1 = (KC1*U*X[1]) 	/ ((K11+X[1]+(MKKK0-X[1]-X[2])*K11/K12)*(1+X[6]/Ki)) - ((P[1]*(MKKK0-X[1]-X[2]))  / (K31+X[2]+(MKKK0-X[1]-X[2])*K31/K32+X[1]*K31/K33)),
	 F2 = (KC2*U*(MKKK0-X[1]-X[2]))    / ((K11+X[1]+(MKKK0-X[1]-X[2])*K11/K12)*(1+X[6]/Ki)) - ((P[2]*X[2])  / (K31+X[2]+(MKKK0-X[1]-X[2])*K31/K32+X[1]*K31/K33)),
	 F3 = (KC5*X[3]*X[2]) / (K51+X[3]+(MKK0-X[3]-X[4])*K51/K52) - ((P[3]*(MKK0-X[3]-X[4])*(1+A*X[6]/Kmp))   / ((K71+X[4]+(MKK0-X[3]-X[4])*K71/K72+X[3]*K71/K73)*(1+X[6]/Kmp))),
	 F4 = (KC6*(MKK0-X[3]-X[4])*X[2])  / (K51+X[3]+(MKK0-X[3]-X[4])*K51/K52) - ((P[4]*X[4]*(1+A*X[6]/Kmp))  / ((K71+X[4]+(MKK0-X[3]-X[4])*K71/K72+X[3]*K71/K73)*(1+X[6]/Kmp))),
	 F5 = (KC9*X[4]*X[5]) / (K91+X[5]+(MAPK0-X[5]-X[6])*K91/K92) - ((P[5]*(MAPK0-X[5]-X[6])) / (K111+X[6]+(MAPK0-X[5]-X[6])*K111/K112+X[5]*K111/K113)),
	 F6 = (KC10*X[4]*(MAPK0-X[5]-X[6]))/ (K91+X[5]*(MAPK0-X[5]-X[6])*K91/K92) - ((P[6]*X[6]) / (K111+X[6]+(MAPK0-X[5]-X[6])*K111/K112+X[5]*K111/K113)))

nbN			<-	6
st			<- c(130,45,52,105,135,27)		# Multiroot trouve les solutions cette fois
Perturbs	<- c(0.2, 0.5, 0.9, 0.99, 1.5)
P0			<- c(V4, V3, V8, V7, V12, V11)	# Les paramètres que l'on perturbe sont V4, V3, V8, V7, V12, V11
NName		<- c("MKKK", "ppMKKK", "MKK", "ppMKK", "MAPK", "ppMAPK")

MExp_6_02_05_09_099_15 	<- MExp (nbN, Perturbs, P0, F6n, st)$Exp
Pert_6_02_05_09_099_15 	<- MExp (nbN, Perturbs, P0, F6n, st)$Pert

#	On simule deux réplicas
nbReplic	<- 2
nbPert		<- length(Perturbs)
XMesNPMoy	<- mean(abs(MExp_6_02_05_09_099_15[1, ]))		# Moyenne des expressions non perturbées
Sd			<- 0.01*XMesNPMoy				# Ecart-type du bruit

MExp_		<- matrix(0, nrow=nbN, ncol=1)

set.seed(12345)
MExp_[ ,1]	<-	MExp_6_02_05_09_099_15[ ,1]
Pert_		<- c("Base")

for (iRep in 1:nbReplic) {
	MExp_6_2	<- MExp_6_02_05_09_099_15 + rnorm(nbN*(nbN*nbPert+1), mean=0, sd=Sd)
	MExp_		<- cbind(MExp_, MExp_6_2[ ,2:(nbPert*nbN+1)])
	Pert_		<- c(Pert_, Pert_6_02_05_09_099_15  [2:(nbPert*nbN+1)])
}

for (iNode in 1:nbN) {
	orig	<- paste("N", iNode, sep="")
	Pert_	<- str_replace_all(Pert_, orig, NName[iNode])
}

Res_6K		<- MRARegress(MExp_, Pert_, NodeName=NName, Relative=FALSE)		# ATTENTION, dans l'article dans Bioinformatics, on était en relatif dans cet exemple (fig.1)
Res_O1_6K	<- MRARegress(MExp_6_02_05_09_099_15, Pert_6_02_05_09_099_15, Relative=FALSE)
Res_O2_6K	<- MRARegress(MExp_6_02_05_09_099_15, Pert_6_02_05_09_099_15, Relative=FALSE, Method="Order2")
round(Res_6K$ANOVA[[1]], 2)



#	Figure 14
#	Représentation de réseaux dont la dynamique est connue

#	Panneau A	: 3 kinases network (Thomaseth et al, 2018)						-- Run the code for Table 1 first
Trash	<-	DrawGraph (Res_3G)			
						# In 'Cytoscape',  File > Export > Network to Image  (you can choose .png format or .svg format and with 'Inkscape', export the image as a .png file, 600 ppp

#	Panneau B	: Linear 3 genes network (Alon, 2006)							-- Run the code for Table 2 first
Trash	<-	DrawGraph (Res_3L)

#	Panneau C	: 4 genes network (Eduardo Sontag et al, 2004)					-- Run the code for SI § 2.3 first
Trash	<-	DrawGraph (Res_4G)

#	Panneau D	: MAPK cascade: 6 nodes network (B. N. Kholodenko et al, 2002)	-- Run the code for SI § 2.4 first
Trash	<-	DrawGraph (Res_6K)



#	Tableau 5
#	Distance euclidienne entre les matrices de connectivité exactes et approchées (ordre 1 et 2), en fonction du niveau de bruit

# Résultats sans bruit. Pour introduire le bruit, faire comme pour les réplicas.

ficSol	<- paste (vRoot, "Solution_3G.rda", sep="")
load (ficSol)

nbN		<- 3
MatDif	<-	matrix (0, nrow=2, ncol=nbN*nbN)
MatDif[1, ]		<- Solution_3G

MatDif[2, ]		<- Res_O1_3G$r
D_3G	<-	dist(MatDif)
print (round(D_3G,3))

MatDif[2, ]		<- Res_O2_3G$r
D_O2_3G	<-	dist(MatDif)
print (round(D_O2_3G,3))

ficSol	<- paste (vRoot, "Solution_4G.rda", sep="")
load (ficSol)

nbN		<- 4
MatDif	<-	matrix (0, nrow=2, ncol=nbN*nbN)
MatDif[1, ]		<- Solution_4G

MatDif[2, ]		<- Res_O1_4G$r
D_4G	<-	dist(MatDif)
print (round(D_4G,3))

MatDif[2, ]		<- Res_O2_4G$r
D_O2_4G	<-	dist(MatDif)
print (round(D_O2_4G,3))

ficSol	<- paste (vRoot, "Solution_6K.rda", sep="")
load (ficSol)

nbN		<- 6
MatDif	<-	matrix (0, nrow=2, ncol=nbN*nbN)
MatDif[1, ]		<- Solution_6K

MatDif[2, ]		<- Res_O1_6K$r
D_6K	<-	dist(MatDif)
print (round(D_6K,3))

MatDif[2, ]		<- Res_O2_6K$r
D_O2_6K	<-	dist(MatDif)
print (round(D_O2_6K,3))



#	Figure 15
#	Valeur moyenne de l'indice D2D en fonction du pourcentage d'informations connues 

#
#	Tst_Knowledge_1	(for parallel computing)
#	This function calculates DC4 InSilico_10_xx and InSilico_100_xx network detection scores for 1 file and 1 knowledge level: "unit function".
#	nbD random draws of the position of the known data are made, unless this percentage is 0 or 100% in which case only 1 calculation is made.
#	Parameter : iCalc = (iFic-1)*nbPcKV + iKV

#	Parallel computing is necessary, because otherwise, the computation time would be very long
#	MRARegress, Score and Classify must be prefixed by the package name, otherwise they will not be recognized.

Tst_Knowledge_1		<- function (iCalc, Pert) {
	iFic	<-	(iCalc-1) %/% nbPcKV +1				# Index of the file studied
	iKV		<-	(iCalc-1) %%  nbPcKV +1				# Index of the KV percentage studied

	nbKV 	<- round(PcKV[iKV]*nbN*(nbN-1) / 100)	# Nbr of known values

	MatExp		<- matrix(nrow=nbN, ncol=nbK*nbN+1)		# Unitary matrices
	Solution	<- matrix(nrow=nbN, ncol=nbN)
	Result1		<- vector(length=nbD+1)					# To store unit results, plus last seed
	KnlM		<- matrix(nrow=nbN, ncol=nbN)			# The known values
	
	MatExp[ , ]		<- MatExpG[ ,((iFic-1)*(nbK*nbN+1)+1) : (iFic*(nbK*nbN+1))]
	Solution[ , ]	<- SolG	  [ ,((iFic-1)*nbN+1) 		  : (iFic*nbN)]

	if (nbKV == 0)	{								# Only one trial in this case
		Ret		<- MRARegress::MRARegress (MatExp, Pert, NoPrint=TRUE)
		Res		<- MRARegress::Score (MRARegress::Classify(Ret, NoPrint=TRUE), Solution, NoPrint=TRUE)$Scores2$Dst	

		for (iDraw in 1:nbD)
			Result1[iDraw]	<- Res
		Result1[nbD+1]	<- 0						# No seed value in this case
	} else if (nbKV == nbN*(nbN-1)) { 				# No trial in this case
		for (iDraw in 1:nbD)
			Result1[iDraw]	<- 1					# Every ri,j is known
		Result1[nbD+1]	<- 0						# No seed value in this case
	} else {
		set.seed(12345)								# To get the same sequence of random numbers
		for (iDraw in 1:nbD) {						# nbD simulations
			#	Choice of values to indicate to the program
			KnlM[ , ]	<- "x"
			if (nbKV > 0) {
				A	<- sample (NDiag, nbKV)	
				for (idx in A) {
					KnlM[idx]	<- ifelse(Solution[idx] == 1, 1, 0)
				}
			}
	
			Ret		<- MRARegress::MRARegress (MatExp, Pert, KnlgMap=KnlM, Hyp_Cvx=HCvx, NoPrint=TRUE)
			Res		<- MRARegress::Score (MRARegress::Classify(Ret, NoPrint=TRUE), Solution, NoPrint=TRUE)$Scores2$Dst

			Result1[iDraw]	<- Res
		}		# Loop on simulations (iDraw)
		Result1[nbD+1]	<-	.Random.seed			# To allow for further simulations, if necessary
	}

	return (Result1)
}		# Tst_Knowledge_1

PcKV		<- seq(0, 100, by=20)				# 	Percentage of known values
nbPcKV		<- length(PcKV)
HCvx		<- 0.5								#	KnlgMap\[i,j\] =  "1" means r\[i,j\] >=  HCvx
nbK			<- 2								#   nbK corresponds to the number of perturbation types (KO or KD)
nbD			<- 10								# 	Number of trials

#	Panneau de gauche : 10 noeuds

nbN			<- 10													# 	Number of nodes
Fics		<- c("10_1", "10_2", "10_3", "10_4", "10_5")			# Files to study
nbFics		<- length(Fics)
Res.df		<- data.frame(pc=NULL, dd=NULL, ddP=NULL, ddM=NULL, Fic=NULL)	# 	To draw Figure "Dist to diagonal of the computed matrix = f(% known values)"
Result1G	<- matrix(0, nrow=nbD+1, ncol=nbFics*nbPcKV)			# To store unit results
Results		<- array(0, dim=c(2,nbPcKV,nbFics))						# To store averaged results

dimnames (Results)	<- list (c("Mean", "Std"), PcKV, Fics)
load (paste(vRoot, "Perturb_10.rda", sep=""))						# Perturb_10
MatExpG		<- matrix(0,   nrow=nbN, ncol=nbFics*(nbK*nbN+1))		# MatExp for all files
SolG		<- matrix(0,   nrow=nbN, ncol=nbFics*nbN)				# Solution for all files

Diag		<- NULL
for (i in 1:nbN) {
	Diag	<- cbind (Diag, (i-1)*nbN + i)		#	Position of the diagonal elements 
}
NDiag		<- seq (1, nbN*nbN, by=1)			# 	Index of terms not belonging to the diagonal
NDiag		<- NDiag[-Diag]

KnlM		<- matrix("x", nrow=nbN, ncol=nbN)	#	The known values

cat ("START 10 nodes networks !", as.character(Sys.time()), "\n")

for (iFic in 1:nbFics) {
	load (paste(vRoot, "MatExp_",   Fics[iFic], ".rda", sep=""))		#   MatExp
	load (paste(vRoot, "Solution_", Fics[iFic], ".rda", sep=""))		#   Solution

	MatExpG[ ,((iFic-1)*(nbK*nbN+1)+1) : (iFic*(nbK*nbN+1))]	<-	MatExp
	SolG[ 	 ,((iFic-1)*nbN+1)		   : (iFic*nbN)]			<-  Solution
}

Ncpus		<- parallel::detectCores()			# Number of cores : 20 in my PC
Ncpus		<- min(Ncpus, 15)					# To prevent the PC from overheating

cl			<- parallel::makeCluster(Ncpus)		# Start parallel computing
doParallel::registerDoParallel(cl)
Result1G	<- foreach (iCalc = 1:(nbFics*nbPcKV), .combine='cbind') %dopar% {
	Tst_Knowledge_1(iCalc, Perturb_10)
}
parallel::stopCluster(cl)

for(iFic in 1:nbFics) {
	for (iKV in 1:nbPcKV) {
		Results["Mean", iKV, iFic]		<- mean	(Result1G [1:nbD, (iFic-1)*nbPcKV+iKV])
		Results["Std",  iKV, iFic]		<- sd	(Result1G [1:nbD, (iFic-1)*nbPcKV+iKV])
		
		Res.tmp.df	<- data.frame(pc=PcKV[iKV], dd=Results["Mean",iKV,iFic], ddP=Results["Mean",iKV,iFic]+Results["Std",iKV,iFic], ddM=Results["Mean",iKV,iFic]-Results["Std",iKV,iFic], Fic=Fics[iFic])
		Res.df		<- rbind(Res.df, Res.tmp.df)		
	}		# Loop on percentage (iKV)
}			# Loop on Fics (iFic)

#	save(Results, file=paste(vRoot, "Fig15a.rda", sep=""))
#	save(Res.df,  file=paste(vRoot, "Fig15a_df.rda", sep=""))
#	save(Result1G, file=paste(vRoo, "Fig15a_Result1G.rda", sep="")) 	# To allow for further simulations, if necessary
#	
cat ("Done !", as.character(Sys.time()), "\n")

#	NomFic 	<- paste(vRoot, "Fig15a.pdf", sep="")
#	pdf(file=NomFic)
ggplot() +
	geom_line(data=Res.df, aes(x=pc, y=dd, colour=factor(Fic))) +
	xlim(0, 100) + ylim(0,1) +
	xlab("% known values") + ylab("Average distance to the diagonal") + ggtitle("Dist. to the diagonal, for the computed  matrix, InSilico_10_1 to 5") +
	scale_colour_manual(name="Fic", values=c("10_1"="blue", "10_2"="chartreuse1", "10_3"="brown3", "10_4"="darkgoldenrod3", "10_5"="gray7")) +
	theme(axis.text.x = element_text(color="black")) +
	theme(axis.text.y = element_text(color="black")) +
	theme(text = element_text(size = 10))
dev.off()

#	Panneau de droite : 100 noeuds

nbN			<- 100								# 	Number of nodes
Fics		<- c("100_1", "100_2", "100_3", "100_4", "100_5")			# Files to study
nbFics		<- length(Fics)
Res.df		<- data.frame(pc=NULL, dd=NULL, ddP=NULL, ddM=NULL, Fic=NULL)	# 	To draw Figure "Dist to diagonal of the computed matrix = f(% known values)"
Result1G	<- matrix(0, nrow=nbD+1, ncol=nbFics*nbPcKV)			# To store unit results
Results		<- array(0, dim=c(2,nbPcKV,nbFics))						# To store averaged results
dimnames (Results)	<- list (c("Mean", "Std"), PcKV, Fics)
load (paste(vRoot, "Perturb_100.rda", sep=""))						# Perturb_100
MatExpG		<- matrix(0,   nrow=nbN, ncol=nbFics*(nbK*nbN+1))		# MatExp for all files
SolG		<- matrix(0,   nrow=nbN, ncol=nbFics*nbN)				# Solution for all files

Diag		<- NULL
for (i in 1:nbN) {
	Diag	<- cbind (Diag, (i-1)*nbN + i)		#	Position of the diagonal elements 
}
NDiag		<- seq (1, nbN*nbN, by=1)			# 	Index of terms not belonging to the diagonal
NDiag		<- NDiag[-Diag]

KnlM		<- matrix("x", nrow=nbN, ncol=nbN)	#	The known values

cat ("START 100 nodes networks !", as.character(Sys.time()), "\n")

#	On peut utiliser un traitement analogue au cas 10 noeuds (long), ou récupérer les résultats enregistrés comme indiqué ci-dessous

load(paste(vRoot, "Fig15b_df.rda", sep=""))

ggplot() +
	geom_line(data=Res.df, aes(x=pc, y=dd, colour=factor(Fic))) +
	xlim(0, 100) + ylim(0,1) +
	xlab("% known values") + ylab("Average distance to the diagonal") + ggtitle("Dist. to the diagonal, for the computed  matrix, InSilico_100_1 to 5") +
	scale_colour_manual(name="Fic", values=c("100_1"="blue", "100_2"="chartreuse1", "100_3"="brown3", "100_4"="darkgoldenrod3", "100_5"="gray7")) +		
	theme(axis.text.x = element_text(color="black")) +
	theme(axis.text.y = element_text(color="black")) +
	theme(text = element_text(size = 10))
dev.off()



#	Tableau 6

#	On peut relancer les traitements précédents, ou relire les données enregistrées
load(paste(vRoot, "Fig15a.rda", sep=""))
print(Results[2,,])

load(paste(vRoot, "Fig15b.rda", sep=""))
print(Results[2,,])



#	Figure 16
#	Distance moyenne entre les matrices calculées et exactes pour des réseaux de 30, 60 et 100 nœuds (TA = 0), en fonction du pourcentage d'arêtes connues  'p'.

#	Tst_Knowledge_2
#	This function calculates FRANK network detection scores, for all files in a TF/TA file set and a level of knowledge: "unit function".
#	nbD random draws of the position of the known data are made, unless this percentage is 0 or 100% in which case only 1 calculation is made.
#	Parameter : iCalc = (iFic-1)*nbPcKV + iKV
#

#	Parallel computing is necessary, because otherwise, the computation time would be very long
#	MRARegress, Score and Classify must be prefixed by the package name, otherwise they will not be recognized.
#

Tst_Knowledge_2		<- function (iCalc) {
	iFic	<-	(iCalc-1) %/% nbPcKV +1				# Index of the file studied
	iKV		<-	(iCalc-1) %%  nbPcKV +1				# Index of the KV percentage studied

	nbKV 	<- round(PcKV[iKV]*nbN*(nbN-1) / 100)	# Nbr of known values

	MatExp		<- matrix(nrow=nbN, ncol=nbK*nbN+1)	# Unitary matrices
	Solution	<- matrix(nrow=nbN, ncol=nbN)
	Result1		<- vector(length=nbD+1)				# To store unit results, plus last seed
	KnlM		<- matrix(nrow=nbN, ncol=nbN)		# The known values
	MatDif		<- matrix(nrow=2, ncol=nbN*nbN)		# To compute the score
	
	MatExp[ , ]		<- MatExpG[ , ,iFic,iT]
	Solution[ , ]	<- SolG	  [ , ,iFic]
	MatDif[1, ]		<- Solution[ , ]

	if (nbKV == 0)	{								# Only one trial in this case
		Ret		<- MRARegress::MRARegress (MatExp, PertG, NoPrint=TRUE)
		MatDif[2, ]	<- Ret$r	
		for (iDraw in 1:nbD)
			Result1[iDraw]	<- dist(MatDif)
		Result1[nbD+1]	<- 0						# No seed value in this case
	} else if (nbKV == nbN*(nbN-1)) { 				# No trial in this case
		for (iDraw in 1:nbD)
			Result1[iDraw]	<- 0					# Every ri,j is known
		Result1[nbD+1]	<- 0						# No seed value in this case
	} else {
		set.seed(12345)								#	To get the same sequence of random numbers
		for (iDraw in 1:nbD) {						# 	nbD simulations
			#	Choice of the values to be indicated to the program
			KnlM[ , ]	<- "x"
			if (nbKV > 0) {
				A	<- sample (NDiag, nbKV)	
				for (idx in A) {
					if (Solution[idx] > 0)
						KnlM[idx]	<- 1
					else if (Solution[idx] < 0)
						KnlM[idx]	<- -1
					else
						KnlM[idx]	<- 0
				}
			}
	
			Ret		<- MRARegress::MRARegress (MatExp, PertG, KnlgMap=KnlM, Hyp_Cvx=HCvx, NoPrint=TRUE)
			MatDif[2, ]	<- Ret$r	
			Result1[iDraw]	<- dist(MatDif)
		}		# Loop on simulations (iDraw)
		Result1[nbD+1]	<-	.Random.seed			# To allow for further simulations, if necessary
	}

	return (Result1)
}		# Tst_Knowledge_2

Sets		<-	c(1, 2, 3)
nbSets		<- length(Sets)
nbFics		<-  5				# Nbr of files by set of values
nbK			<-	2				# Nbr of perturbations (KO, KD -50%)
nbD			<- 10				# Number of trials

Noises		<- c(0.1, 0.5)		# Error coefficients
nbT			<- length(Noises)

PcKV		<- c(0, 20, 40, 60, 70, 75, 80, 85, 90, 100)	# Percentage of known values
nbPcKV		<- length(PcKV)
HCvx		<- 0.1											# KnlgMap\[i,j\] =  "1" means r\[i,j\] >=  HCvx			vs 0.5

Result1G	<- array(0, dim=c(nbD+1, nbPcKV, nbSets, nbFics, nbT))		# To store unit results
Results		<- array(0, dim=c(2, 	 nbPcKV, nbSets, nbT))				# To store averaged results
dimnames (Results)	<- list (c("Mean", "Std"), PcKV, Sets, Noises)

Res.df		<- data.frame(pc=NULL, dd=NULL, ddP=NULL, ddM=NULL, Set=NULL, Noise=NULL)	# To draw Figure "Euclidian Distance between computed matrix and solution = f(% known values)"

val			<- paste(vRoot, "FileNbr.csv", sep="")
FileNbr		<- as.matrix(fread(val, data.table=F, header=T), ncol=nbFics+2)
					# Column name	: TF 	TA    1 	 2 	   3 	 4 	   5
					# First row		: Seed	 	12345 72090 87577 45648 16637
					# Following rows: TF val TA val File nbrs

Ncpus		<- parallel::detectCores()			# Number of cores : 20 in my PC
Ncpus		<- min(Ncpus, 15)					# To prevent the PC from overheating

cat ("START FRANK networks !", as.character(Sys.time()), "\n")

###		ATTENTION !   Le traitement suivant est très long (7h sur mon PC). On peut le sauter et récupérer les fichiers de résultats enregistrés à la place (après "Done !")
#					  ou n'en exécuter qu'une partie (diminuer le nombre d'ensembles et/ou le nb. de pc. de valeurs connues).

for (iSet in 1:nbSets) {
	Set			<- Sets[iSet]
	TF			<- as.numeric(FileNbr[Set, 1])
	TA			<- as.numeric(FileNbr[Set, 2])
	nbN			<- TF+TA

	MatExpG		<- array(0, dim=c(nbN, nbK*nbN+1, nbFics, nbT))		# MatExp, for all files of the set
	SolG		<- array(0, dim=c(nbN, nbN, nbFics))					# Solution, for all files of the set
	PertG		<- vector(length=nbK*nbN+1)								# Perturbation, for all files of the set

	Diag		<- NULL
	for (i in 1:nbN) {
		Diag	<- cbind (Diag, (i-1)*nbN + i)				# Position of the diagonal elements 
	}
	NDiag		<- seq (1, nbN*nbN, by=1)					# Index of terms not belonging to the diagonal
	NDiag		<- NDiag[-Diag]

	for (iFic in 1:nbFics) {
		valR	<- paste(vRoot, "Frank_TF", TF, "_TA", TA, "_", iFic, "_Sol.rda", sep="")
		
		load (valR)				# Solution
		SolG[ , ,iFic]				<-  Solution

		for (iT in 1:nbT) {
			valR	<- paste(vRoot, "Frank_TF", TF, "_TA", TA, "_", iFic, "_R", iT, ".rda", sep="")
			load (valR)				# MatRN2
			Res		<- MatR2MatExp	(MatRN2, nbN, nbK)		# software utility, to get the data in the format expected by MRARegress
			MatExpG[ , ,iFic,iT]	<- Res$Exp
			PertG  					<- Res$Pert				# PertG doesn't depend on iT or iFic (not useful to use an array)
		}	# Loop iT
	}		# loop iFic

	for (iT in 1:nbT)	{
		cat (" Set TF ", TF, " TA ", TA, " Noise ", Noises[iT]," at : ", as.character(Sys.time()), "\n")
		
		cl	<- parallel::makeCluster(Ncpus)		# Start parallel computing
		doParallel::registerDoParallel(cl)
		Result1G[ , ,iSet, ,iT]		<- foreach (iCalc = 1:(nbFics*nbPcKV), .combine='cbind') %dopar% {
			Tst_Knowledge_2(iCalc)
		}
		parallel::stopCluster(cl)

		for (iKV in 1:nbPcKV) {
			Results["Mean",iKV,iSet,iT]		<- mean	(Result1G [1:nbD,iKV,iSet, ,iT])
			Results["Std", iKV,iSet,iT]		<- sd	(Result1G [1:nbD,iKV,iSet, ,iT])
			
			moy		<- Results["Mean",iKV,iSet,iT]
			std		<- Results["Std", iKV,iSet,iT]
			Res.tmp.df	<- data.frame(pc=PcKV[iKV], dd=moy, ddP=moy+std, ddM=moy-std, Set=Sets[iSet], Noise=iT)
			Res.df		<- rbind(Res.df, Res.tmp.df)		
		}		# Loop on percentage (iKV)
	}			# Loop iT
}				# loop iSet

#	save(Results, file=paste(vRoot, "Fig16.rda", sep=""))
#	save(Res.df,  file=paste(vRoot, "Fig16_df.rda", sep=""))
#	save(Result1G, file=paste(vRoot, "Fig16_Result1G.rda", sep="")) 	# To allow for further simulations, if necessary

cat ("Done !", as.character(Sys.time()), "\n")			# Length of processing : 7 hours

load(paste(vRoot, "Fig16_df.rda", sep=""))

#	Panneau de gauche : bruit moyen (k = 0,1)

Res1.df		<- subset(Res.df, Noise == 1)
Res1_1.df	<- subset(Res1.df, Set == 1)			# TF30,  TA0
Res1_2.df	<- subset(Res1.df, Set == 2)			# TF60,  TA0
Res1_3.df	<- subset(Res1.df, Set == 3)			# TF100, TA0

ggplot(data=Res1.df) +
	geom_line(aes(x=pc, y=dd, colour=factor(Set))) + geom_point(aes(x=pc, y=dd, colour=factor(Set))) +
	xlab("% known values") + ylab("Average distance between Solution and Calculated Matrix") + ggtitle("Dist. between Solution and Calculated Matrix : \nFRANK  TF = 30, 60, 100, TA = 0, medium noise (k = 0.1)") +
	scale_colour_manual(name="Set", values=c("1"="blue", "2"="chartreuse1", "3"="brown3")) +
	geom_ribbon(data=Res1_1.df, aes(x=pc, ymin=ddM, ymax=ddP), fill="cadetblue1", 		alpha=0.3) +		# Ribbon around a "blue" line
	geom_ribbon(data=Res1_2.df, aes(x=pc, ymin=ddM, ymax=ddP), fill="darkolivegreen3", alpha=0.3) +		# Ribbon around a "chartreuse1" line
	geom_ribbon(data=Res1_3.df, aes(x=pc, ymin=ddM, ymax=ddP), fill="chocolate1", 		alpha=0.3) +		# Ribbon around a "brown3" line
	theme(axis.text.x = element_text(color="black")) +
	theme(axis.text.y = element_text(color="black")) +
	theme(text = element_text(size = 10))
dev.off()

#	Panneau de droite : bruit fort (k = 0,5)

Res2.df		<- subset(Res.df, Noise == 2)
Res2_1.df	<- subset(Res2.df, Set == 1)			# TF30,  TA0
Res2_2.df	<- subset(Res2.df, Set == 2)			# TF60,  TA0
Res2_3.df	<- subset(Res2.df, Set == 3)			# TF100, TA

ggplot(data=Res2.df) +
	geom_line(aes(x=pc, y=dd, colour=factor(Set))) + geom_point(aes(x=pc, y=dd, colour=factor(Set))) +
	xlab("% known values") + ylab("Average distance between Solution and Calculated Matrix") + ggtitle("Dist. between Solution and Calculated Matrix : \nFRANK  TF = 30, 60, 100, TA = 0, strong noise (k = 0.5)") +
	scale_colour_manual(name="Set", values=c("1"="blue", "2"="chartreuse1", "3"="brown3")) +
	geom_ribbon(data=Res2_1.df, aes(x=pc, ymin=ddM, ymax=ddP), fill="cadetblue1", 		alpha=0.3) +		# Ribbon around a "blue" line
	geom_ribbon(data=Res2_2.df, aes(x=pc, ymin=ddM, ymax=ddP), fill="darkolivegreen3", alpha=0.3) +		# Ribbon around a "chartreuse1" line
	geom_ribbon(data=Res2_3.df, aes(x=pc, ymin=ddM, ymax=ddP), fill="chocolate1", 		alpha=0.3) +		# Ribbon around a "brown3" line
	theme(axis.text.x = element_text(color="black")) +
	theme(axis.text.y = element_text(color="black")) +
	theme(text = element_text(size = 10))
dev.off()



#	Figure 17
#	Distance moyenne entre les matrices calculées et exactes pour des réseaux de 60, 120 et 200 nœuds (TF = TA), en fonction du pourcentage de gènes non régulateurs   'p'.

#	Tst_Knowledge_3
#	This function calculates FRANK network detection scores, for all files in a TF/TA file set and a level of knowledge: "unit function".
#	nbD tirages aleatoires de la position des noeuds non régulants connus sont réalisées, sauf si ce pourcentage vaut 0 auqiel cas 1 seul calcul est effectué.
#	nbD random draws of the position of known non-regulating nodes are performed, unless this percentage is 0 or 100% in which case only 1 calculation is made.
#	Parameter : iCalc = (iFic-1)*nbPcKV + iKV
#

#	Parallel computing is necessary, because otherwise, the computation time would be very long
#	MRARegress, Score and Classify must be prefixed by the package name, otherwise they will not be recognized.
#

Tst_Knowledge_3		<- function (iCalc) {
	iFic	<-	(iCalc-1) %/% nbPcKV +1				# Index of the file studied
	iKV		<-	(iCalc-1) %%  nbPcKV +1				# Index of the KV percentage studied

	nbKV 	<- round(PcKV[iKV]*TA / 100)			# Nbr of known nodes

	MatExp		<- matrix(nrow=nbN, ncol=nbK*nbN+1)	# Unitary matrices
	Solution	<- matrix(nrow=nbN, ncol=nbN)
	Result1		<- vector(length=nbD+1)				# To store unit results, plus last seed
	KnlM		<- matrix(nrow=nbN, ncol=nbN)		# The known values
	MatDif		<- matrix(nrow=2, ncol=nbN*nbN)		# To compute the score
	
	MatExp[ , ]		<- MatExpG[ , ,iFic,iT]
	Solution[ , ]	<- SolG	  [ , ,iFic]
	MatDif[1, ]		<- Solution[ , ]

	if (nbKV == 0)	{								# Only one trial in this case
		Ret		<- MRARegress::MRARegress (MatExp, PertG, NoPrint=TRUE)
		MatDif[2, ]	<- Ret$r	
		for (iDraw in 1:nbD)
			Result1[iDraw]	<- dist(MatDif)
		Result1[nbD+1]	<- 0						# No seed value in this case
	} else {
		set.seed(12345)								#	To get the same sequence of random numbers
		for (iDraw in 1:nbD) {						# 	nbD simulations
			#	Choice of the nodes to be indicated to the program
			KnlM[ , ]	<- "x"
			A	<- sample (1:TA, nbKV)	
			for (idx in A) {
				KnlM[ ,TF+idx]	<- 0
			}
	
			Ret		<- MRARegress::MRARegress (MatExp, PertG, KnlgMap=KnlM, Hyp_Cvx=HCvx, NoPrint=TRUE)
			MatDif[2, ]	<- Ret$r	
			Result1[iDraw]	<- dist(MatDif)
		}		# Loop on simulations (iDraw)
		Result1[nbD+1]	<-	.Random.seed			# To allow for further simulations, if necessary
	}

	return (Result1)
}		# Tst_Knowledge_3


#	The original data delivered by FRANK Network Generator are archived, in case more computations are needed. Files are named ""Frankxxxxnonmodified_network.csv",
#	where xxxx are :
#	5191, 5303, 3890, 9658, 5309	for TF=30,   TA=30,			Set = 9
#	7120, 8664,  239, 3835, 4999	for TF=50,	 TA=50,			Set = 10
#	1276, 3865, 7672, 2237, 7584	for TF=100,  TA=100,		Set = 11

Sets		<-	c(9, 10, 11)
nbSets		<- length(Sets)
nbFics		<-  5				# Nbr of files by set of values
nbK			<-	2				# Nbr of perturbations (KO, KD -50%)
nbD			<- 10				# Number of trials

Noises		<- c(0.1, 0.5)		# Error coefficients
nbT			<- length(Noises)

PcKV		<- seq(0, 100, by=20)							# Percentage of known TA nodes
nbPcKV		<- length(PcKV)
HCvx		<- 0.1											# KnlgMap\[i,j\] =  "1" means r\[i,j\] >=  HCvx  -- not used here

Result1G	<- array(0, dim=c(nbD+1, nbPcKV, nbSets, nbFics, nbT))		# To store unit results
Results		<- array(0, dim=c(2, 	 nbPcKV, nbSets, nbT))				# To store averaged results
dimnames (Results)	<- list (c("Mean", "Std"), PcKV, Sets, Noises)

Res.df		<- data.frame(pc=NULL, dd=NULL, ddP=NULL, ddM=NULL, Set=NULL, Noise=NULL)	# To draw Figure "Euclidian Distance between computed matrix and solution = f(% known TA nodes)"

val			<- paste(vRoot, "FileNbr.csv", sep="")
FileNbr		<- as.matrix(fread(val, data.table=F, header=T), ncol=nbFics+2)
					# Column name	: TF 	TA    1 	 2 	   3 	 4 	   5
					# First row		: Seed	 	12345 72090 87577 45648 16637
					# Following rows: TF val TA val File nbrs

Ncpus		<- parallel::detectCores()			# Number of cores : 20 in my PC
Ncpus		<- min(Ncpus, 15)					# To prevent the PC from overheating

cat ("START FRANK networks !", as.character(Sys.time()), "\n")

###		ATTENTION !   Le traitement suivant est très long (19h sur mon PC). On peut le sauter et récupérer les fichiers de résultats enregistrés à la place (après "Done !")
#					  ou n'en exécuter qu'une partie (diminuer le nombre d'ensembles et/ou le nb. de pc. de valeurs connues).

for (iSet in 1:nbSets) {
	Set			<- Sets[iSet]
	TF			<- as.numeric(FileNbr[Set, 1])
	TA			<- as.numeric(FileNbr[Set, 2])
	nbN			<- TF+TA

	MatExpG		<- array(0, dim=c(nbN, nbK*nbN+1, nbFics, nbT))		# MatExp, for all files of the set
	SolG		<- array(0, dim=c(nbN, nbN, nbFics))					# Solution, for all files of the set
	PertG		<- vector(length=nbK*nbN+1)								# Perturbation, for all files of the set

	for (iFic in 1:nbFics) {
		valR	<- paste(vRoot, "Frank_TF", TF, "_TA", TA, "_", iFic, "_Sol.rda", sep="")
		load (valR)				# Solution
		SolG[ , ,iFic]				<-  Solution

		for (iT in 1:nbT) {
			valR	<- paste(vRoot, "Frank_TF", TF, "_TA", TA, "_", iFic, "_R", iT, ".rda", sep="")
			load (valR)				# MatRN2
			Res		<- MatR2MatExp	(MatRN2, nbN, nbK)		# software utility, to get the data in the format expected by MRARegress
			MatExpG[ , ,iFic,iT]	<- Res$Exp
			PertG  					<- Res$Pert				# PertG doesn't depend on iT or iFic (not useful to use an array)
		}	# Loop iT
	}		# loop iFic

	for (iT in 1:nbT)	{
		cat (" Set TF ", TF, " TA ", TA, " Noise ", Noises[iT]," at : ", as.character(Sys.time()), "\n")
		
		cl	<- parallel::makeCluster(Ncpus)		# Start parallel computing
		doParallel::registerDoParallel(cl)
		Result1G[ , ,iSet, ,iT]		<- foreach (iCalc = 1:(nbFics*nbPcKV), .combine='cbind') %dopar% {
			Tst_Knowledge_3(iCalc)
		}
		parallel::stopCluster(cl)

		for (iKV in 1:nbPcKV) {
			Results["Mean",iKV,iSet,iT]		<- mean	(Result1G [1:nbD,iKV,iSet, ,iT])
			Results["Std", iKV,iSet,iT]		<- sd	(Result1G [1:nbD,iKV,iSet, ,iT])
			
			moy		<- Results["Mean",iKV,iSet,iT]
			std		<- Results["Std", iKV,iSet,iT]
			Res.tmp.df	<- data.frame(pc=PcKV[iKV], dd=moy, ddP=moy+std, ddM=moy-std, Set=Sets[iSet], Noise=iT)
			Res.df		<- rbind(Res.df, Res.tmp.df)		
		}		# Loop on percentage (iKV)
	}			# Loop iT
}				# loop iSet

#	save(Results, file=paste(vRoot, "Fig17.rda", sep=""))
#	save(Res.df,  file=paste(vRoot, "Fig17_df.rda", sep=""))
#	save(Result1G, file=paste(vRoot, "Fig17_Result1G.rda", sep="")) 	# To allow for further simulations, if necessary

cat ("Done !", as.character(Sys.time()), "\n")			# Length of processing : 19 hours

load(paste(vRoot, "Fig17_df.rda", sep=""))

#	Panneau de gauche : bruit moyen (k = 0,1)

Res1.df		<- subset(Res.df, Noise == 1)
Res1_1.df	<- subset(Res1.df, Set == 9)			# TF30,  TA30
Res1_2.df	<- subset(Res1.df, Set == 10)			# TF50,  TA50
Res1_3.df	<- subset(Res1.df, Set == 11)			# TF100, TA100

ggplot(data=Res1.df) +
	geom_line(aes(x=pc, y=dd, colour=factor(Set))) + geom_point(aes(x=pc, y=dd, colour=factor(Set))) +
	xlab("% known TA nodes") + ylab("Average distance between Solution and Calculated Matrix") + ggtitle("Dist. between Solution and Calculated Matrix : \nFRANK  TA = TF = 30, 50, 100, medium noise (k = 0.1)") +
	scale_colour_manual(name="Set", values=c("9"="blue", "10"="chartreuse1", "11"="brown3")) +
	geom_ribbon(data=Res1_1.df, aes(x=pc, ymin=ddM, ymax=ddP), fill="cadetblue1", 		alpha=0.3) +		# Ribbon around a "blue" line
	geom_ribbon(data=Res1_2.df, aes(x=pc, ymin=ddM, ymax=ddP), fill="darkolivegreen3", alpha=0.3) +		# Ribbon around a "chartreuse1" line
	geom_ribbon(data=Res1_3.df, aes(x=pc, ymin=ddM, ymax=ddP), fill="chocolate1", 		alpha=0.3) +		# Ribbon around a "brown3" line
	theme(axis.text.x = element_text(color="black")) +
	theme(axis.text.y = element_text(color="black")) +
	theme(text = element_text(size = 10))
dev.off()

#	Panneau de droite : bruit fort (k = 0,5)

Res2.df		<- subset(Res.df, Noise == 2)
Res2_1.df	<- subset(Res2.df, Set == 9)			# TF30,  TA30
Res2_2.df	<- subset(Res2.df, Set == 10)			# TF50,  TA50
Res2_3.df	<- subset(Res2.df, Set == 11)			# TF100, TA100

ggplot(data=Res2.df) +
	geom_line(aes(x=pc, y=dd, colour=factor(Set))) + geom_point(aes(x=pc, y=dd, colour=factor(Set))) +
	xlab("% known TA nodes") + ylab("Average distance between Solution and Calculated Matrix") + ggtitle("Dist. between Solution and Calculated Matrix : \nFRANK  TA = TF = 30, 50, 100, strong noise (k = 0.5)") +
	scale_colour_manual(name="Set", values=c("9"="blue", "10"="chartreuse1", "11"="brown3")) +
	geom_ribbon(data=Res2_1.df, aes(x=pc, ymin=ddM, ymax=ddP), fill="cadetblue1", 		alpha=0.3) +		# Ribbon around a "blue" line
	geom_ribbon(data=Res2_2.df, aes(x=pc, ymin=ddM, ymax=ddP), fill="darkolivegreen3", alpha=0.3) +		# Ribbon around a "chartreuse1" line
	geom_ribbon(data=Res2_3.df, aes(x=pc, ymin=ddM, ymax=ddP), fill="chocolate1", 		alpha=0.3) +		# Ribbon around a "brown3" line
	theme(axis.text.x = element_text(color="black")) +
	theme(axis.text.y = element_text(color="black")) +
	theme(text = element_text(size = 10))
dev.off()
