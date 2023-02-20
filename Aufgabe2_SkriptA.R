# *STATISTISCHE AUSWERTUNG Aufgabe 2 Skript A*                                 #
################################################################################
# Statistische Auswertung fuer die die Gruppenarbeit im Modul                  #
# Wissenschaftliches Arbeiten.                                                 #
# Skript A enthaelt folgende Funktionen:                                       #
# (a) Eine Funktion, die verschiedene geeignete deskriptive Statistike         #
#     fuer metrische Variablen berechnet und ausgibt                           #
# (b) Eine Funktion, die verschiedene geeignete deskriptive Statistiken        #
#     fuer kategoriale Variablen berechnet und ausgibt                         #
# (c) Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer      #
#     den Zusammenhang zwischen zwei kategorialen Variablen                    #
#     berechnet ausgibt                                                        #
# (d) Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer      #
#     den Zusammengang zwischen einer metrischen und einer                     #
#     dichotomen Variablen berechnet und ausgibt                               #
# (e) Eine Funktion, die eine mindestens ordinal skalierte Variable            #
#     quantilbasiert kategorisiert (z.B. in niedrig, mittel, hochâ)            #
# (f) Eine Funktion, die eine geeignete Visualisierung von drei oder vier      #
#     kategorialen Variablen erstellt                                          #
# Freiwillig: weitere zur Deskription und Visualisierung geeignete             #
#     Funktionen                                                               #
#                                                                              #
# Authoren: Tim Burkholder, Florian Haschke, Ferry Heinzelmann, Eva Noether,   #
#           Rafael Slodzinski                                                  #
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
daten <- read_csv(file = "./Datensatz_Aufgabe1.csv")


# 01 DESKRIPTIVE STATISTIK - metr. Variablen -----------------------------------
# Eine Funktion, die verschiedene geeignete deskriptive Statistiken            #
# fuer metrische Variablen berechnet und ausgibt                               #

# Berechnung anhand des Alters

# 01a HAUPTFUNKTION ------------------------------------------------------------
# Hauptfunktion beinhaltet: min, max, median, arothm. Mittel, 1 & 3-Quartil,   
# Modalwert, Spannweite, IQR, MAD, Varianz, SD
calculate_metrParam <- function(daten){
  calculate_mean(daten$Alter)     # arithm Mittel
  calculate_median(daten$Alter)   # Median
  calculate_max(daten$Alter)      # Maximum
  calculate_min(daten$Alter)      # Minimum
  calculate_firstQ(daten$Alter)   # Erstes Quartil
  calculate_thirdQ(daten$Alter)   # Drittes Quartil
  calculate_mod(daten$Alter)      # Modalwert
  calculate_range(daten$Alter)    # Spannweite
  calculate_iqr(daten$Alter)      # Interquartilsabstand
  calculate_mad(daten$Alter)      # Mean Absolute Deviation
  calculate_var(daten$Alter)      # Varianz
  calculate_sd(daten$Alter)       # Standardabweichung
}

# 01b UNTERFUNKTIONEN ----------------------------------------------------------
# Arithmetisches Mittel
calculate_mean <-function(x){
  mean(x)
}

# Median
calculate_median <- function(x){
  median(x)
}

# Maximum
calculate_max <- function(x){
  max(x)
}

# Minimum
calculate_min <- function(x){
  min(x)
}

# 1. Qartil
calculate_firstQ <- function(x){
  quantile(x, 0.25)
}

# 3. Quartil
calculate_thirdQ <- function(x){
  quantile(x, 0.75)
}

# Modalwert
calculate_mod <- function(x){
  names(x)[which.max(x)]
}

# Spannweite
calcualte_range <- function(x){
  range(x)
}

# Interquartilsabstand
calculate_iqr <- function(x){
  IQR(x)
}

# Mean Absolute Devaition
calculate_mad <- function(x){
  mad(x)
}

# Varianz
calculate_var <- function(x){
  var(x)
}

# Standardabweichung
calculate_sd <- function(x){
  sd(x)
}



# 02 DESKRIPTIVE STATISTIK - kategor. Variablen --------------------------------
# Eine Funktion, die verschiedene geeignete deskriptive Statistiken            #
# fuer kategoriale Variablen berechnet und ausgibt                             #





# 03 DESKRIPTIVE BIV. STATISTIK - zwei kategor. Variablen ----------------------
# Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer          #
# den Zusammenhang zwischen zwei kategorialen Variablen                        #  
# berechnet ausgibt                                                            #

# deskriptive bivariate Statistiken für den Zusammenhang zwischen zwei kategorialen Variablen
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
#  Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer         #
# den Zusammengang zwischen einer metrischen und einer                         #
# dichotomen Variablen berechnet und ausgibt                                   #




# 05 KATRGORISIEREN ORD. SKAL. VARIABLE ----------------------------------------
# Eine Funktion, die eine mindestens ordinal skalierte Variable                #
# quantilbasiert kategorisiert (z.B. in niedrig, mittel, hoch)                 #




# 06 VISUALISIERUNG KATEGOR. VARIABLEN -----------------------------------------
# Eine Funktion, die eine geeignete Visualisierung von drei oder vier          #
# kategorialen Variablen erstellt                                              #




# 07 DESKRIPT. & VISULAISIERUNG - Freiwilli ------------------------------------
# Freiwillig: weitere zur Deskription und Visualisierung geeignete             #
# Funktionen                                                                   #
#










#
