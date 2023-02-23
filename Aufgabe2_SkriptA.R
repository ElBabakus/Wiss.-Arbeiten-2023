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
# fÃ¼r metrische Variablen berechnet und ausgibt                                #
calculate_metrParam <- function(daten){
  cal_mean <- calculate_mean(daten$Alter)       # arithm Mittel
  cal_med <- calculate_median(daten$Alter)      # Median
  cal_max <- calculate_max(daten$Alter)         # Maximum
  cal_min <- calculate_min(daten$Alter)         # Minimum
  cal_fristQ <- calculate_firstQ(daten$Alter)   # Erstes Quartil
  cal_thirdQ <- calculate_thirdQ(daten$Alter)   # Drittes Quartil
  cal_mod <- calculate_mod(daten$Alter)         # Modalwert
  cal_range <- calculate_range(daten$Alter)     # Spannweite
  cal_irq <- calculate_iqr(daten$Alter)         # Interquartilsabstand
  cal_mad <- calculate_mad(daten$Alter)         # Mean Absolute Deviation
  cal_var <- calculate_var(daten$Alter)         # Varianz
  cal_sd <- calculate_sd(daten$Alter)           # Standardabweichung
  
  # Erzeugen einer Liste, die dann ausgegeben werden soll
  result <- list(mean = cal_mean, 
                 med = cal_med, 
                 max = cal_max, 
                 min = cal_min, 
                 firstQuartil = cal_fristQ,
                 thirdQuartil = cal_thirdQ, 
                 modal = cal_mod, 
                 range = cal_range, 
                 irq = cal_irq, 
                 mad = cal_mad,
                 varianz = cal_var, 
                 sdt = cal_sd)
  
  
  # Ausgabe aller Ergebnisse dieser Funktion als Liste
  return(result)
  
}

# Aufruf und Ausgabe der Funktion calculate_metrParam
ergebnis <- calculate_metrParam(daten)
print(ergebnis)

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
calculate_range <- function(x){
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
# für kategoriale Variablen berechnet und ausgibt                              #





# 03 DESKRIPTIVE BIV. STATISTIK - zwei kategor. Variablen ----------------------
# Eine Funktion, die geeignete deskriptive bivariate Statistiken für           #
# den Zusammenhang zwischen zwei kategorialen Variablen                        #  
# berechnet ausgibt                                                            #

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
