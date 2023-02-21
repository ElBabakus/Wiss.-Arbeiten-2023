# *STATISTISCHE AUSWERTUNG Aufgabe 2 Skript A*                                  #
################################################################################
# Statistische Auswertung fuer die die Gruppenarbeit im Modul                  #
# Wissenschaftliches Arbeiten.                                                 #
# Skript A enthaelt folgende Funktionen:                                       #
# (a) Eine Funktion, die verschiedene geeignete deskriptive Statistike         #
#     für metrische Variablen berechnet und ausgibt                            #
# (b) Eine Funktion, die verschiedene geeignete deskriptive Statistiken        #
#     für kategoriale Variablen berechnet und ausgibt                          #
# (c) Eine Funktion, die geeignete deskriptive bivariate Statistiken für       #
#     den Zusammenhang zwischen zwei kategorialen Variablen                    #
#     berechnet ausgibt                                                        #
# (d) Eine Funktion, die geeignete deskriptive bivariate Statistiken für       #
#     den Zusammengang zwischen einer metrischen und einer                     #
#     dichotomen Variablen berechnet und ausgibt                               #
# (e) Eine Funktion, die eine mindestens ordinal skalierte Variable            #
#     quantilbasiert kategorisiert (z.B. in „niedrig“, „mittel“, „hoch“)       #
# (f) Eine Funktion, die eine geeignete Visualisierung von drei oder vier      #
#     kategorialen Variablen erstellt                                          #
# Freiwillig: weitere zur Deskription und Visualisierung geeignete             #
#     Funktionen                                                               #
#                                                                              #
# Authoren: Tim Burkholder, Florian Haschke, Ferry Heinzelmann, Eva Noether,   #
#           Rafael Slodzinski                                                   #
################################################################################

# 00 Packages und Datei laden --------------------------------------------------

# Environment leeren
rm(list = ls()) 

# 00a PACKAGES LADEN -----------------------------------------------------------
library(dplyr)
library(readr)
library(reshape)
library(ggplot2)  # plotten
library(qqplotr)  # Ergaenzung zu qq Plot
library(svglite)
library(ggpmisc)  # Minima und Maxima finden
library(Rmisc )   # Konfidentintervall


# 00b DATEN LADEN --------------------------------------------------------------
# Einlesen der Daten (als data.frame)
# TODO: relativen Pfad anlegen 
daten <- read_csv(file = "\\Datensatz_Aufgabe1.csv")


# 01 DESKRIPTIVE STATISTIK - metr. Variablen -----------------------------------
# Eine Funktion, die verschiedene geeignete deskriptive Statistiken         #
# für metrische Variablen berechnet und ausgibt                                #




# 02 DESKRIPTIVE STATISTIK - kategor. Variablen --------------------------------
# Eine Funktion, die verschiedene geeignete deskriptive Statistiken            #
# für kategoriale Variablen berechnet und ausgibt                              #





# 03 DESKRIPTIVE BIV. STATISTIK - zwei kategor. Variablen ----------------------
# Eine Funktion, die geeignete deskriptive bivariate Statistiken für           #
# den Zusammenhang zwischen zwei kategorialen Variablen                        #  
# berechnet ausgibt                                                            #

# Chi2 - Koeffizient #
# Kann fuer nominale Merkmale gewaehlt werden:
# Bsp.: X = x1, x2 (MatheLK);   Y = y1,y2,y3,y4 (Studiengang)
# Ist noetig zur Berechnung anderer Koeff. (z.B. Cramer, Phi, etc.)
chi2_function <- function(X,Y){
  n <- length(X)
  X <- factor(X)
  Y <- factor(Y)
  # Erzeuge Tabelle (in Form einer Matrix) zur Berechnung von Chi2 Koeff.
  H <- matrix(0, length(levels(X)), length(levels(Y)))
  rownames(H) <- levels(X)
  colnames(H) <- levels(Y)
  for(i in 1:length(levels(X))){
    for(j in 1:length(levels(Y))){
      H[i,j] <- sum (X == levels(X)[i] & Y == levels(Y)[j])
    }
  }
  # Erzeuge Tabelle (in Form einer Matrix) mit den Erwartungshaeufigkeiten.
  E <- matrix(0, length(levels(X)), length(levels(Y)))
  rownames(E) <- levels(X)
  colnames(E) <- levels(Y)
  for(i in 1:length(levels(X))){
    for(j in 1:length(levels(Y))){
      E[i,j] <- n * (sum(H[i,])/n) * (sum(H[,j])/n)
    }
  }
  # Berechne mit Hilfe von H und E nun chi2
  chi2 <- 0
  for(i in 1:length(levels(X))){
    for(j in 1:length(levels(Y))){
      chi2 <- chi2 + ((H[i,j]-E[i,j])^2/E[i,j])
    }
  }
  return((chi2))
}

# deskriptive bivariate Statistiken f�r den Zusammenhang zwischen zwei kategorialen Variablen
# Matheinteresse und Programmierinteresse mit Kontingenztafel:
f <- function(){
  A <- matrix(0,7,7)
  for (i in 1:100){
    A[Daten$Matheinteresse[i], Daten$Programmierinteresse[i]] <- A[Daten$Matheinteresse[i], Daten$Programmierinteresse[i]] + 1
  }
  return(A)
}
A <- f()
mosaicplot(A, xlab = "Programmierinteresse", ylab = "Matheinteresse", main = "")



# 04 DESKRIPTIVE BIV. STATISTIK - metr. & dichot. Variable ---------------------
#  Eine Funktion, die geeignete deskriptive bivariate Statistiken für          #
# den Zusammengang zwischen einer metrischen und einer                         #
# dichotomen Variablen berechnet und ausgibt                                   #




# 05 KATRGORISIEREN ORD. SKAL. VARIABLE ----------------------------------------
# Eine Funktion, die eine mindestens ordinal skalierte Variable                #
# quantilbasiert kategorisiert (z.B. in „niedrig“, „mittel“, „hoch“)           #




# 06 VISUALISIERUNG KATEGOR. VARIABLEN -----------------------------------------
# Eine Funktion, die eine geeignete Visualisierung von drei oder vier          #
# kategorialen Variablen erstellt                                              #




# 07 DESKRIPT. & VISULAISIERUNG - Freiwilli ------------------------------------
# Freiwillig: weitere zur Deskription und Visualisierung geeignete             #
# Funktionen                                                                   #
#










#
