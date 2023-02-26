# *STATISTISCHE AUSWERTUNG Skript 4 Analyse (Desktiption & Visualisierung)*    #
################################################################################
# Statistische Auswertung fuer die die Gruppenarbeit im Modul                  #
# Wissenschaftliches Arbeiten.                                                 #
# Skript 4:                                                                    #
# Die mit Hilde von Skript A und dem R-Helper-Skript erstellten Funktionen     #
# sollen genutzt werden, um eine Analyse (Deskription und Visualisuerung)      #
# der simulierten Daten durchzuf�hren.                                         #
#                                                                              #
# Authoren: Florian Haschke, Eva Noether, Rafael Slodzinski                    #  
#                                                                              #
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
library(tibble)   # Datenstruktur


# Laden des Skriptes-A
source("./Aufgabe2_SkriptA.R")


# 00b DATEN LADEN --------------------------------------------------------------
# Einlesen der Daten (als data.frame)
daten <- read.csv2(file = "./Datensatz_Aufgabe1.csv")
Datensatz_Aufgabe1 <- lade_Datensatz("Datensatz_Aufgabe1.csv", delim = ";")
daten = Datensatz_Aufgabe1

# Ueberpruefen, ob Daten als DataFrame vorliegen, und wenn nicht Umwandlung in ein
# solches mit Hilde einer FUnktion aus dem R-Helper-Skript
CheckDataFrame(daten)



# 01 DESKRIPTION DER DATEN -----------------------------------------------------
# 01a BETRACHTUNG LAGE ANHAND VON MEDIAN, MITTELWERT UND MODALWERT -------------
# Modalwert, Median und Mittelwert ueber die Funktion calculate_metrParam aus 
# dem Skript A suchen, um sie dann zu vergleichen
calculate_metrParam(daten)
Param_mean <- metrParam[["mean"]]
Param_med <- metrParam[["med"]]
Param_mod <- metrParam[["modal"]]

# Vergleich der drei Werte und Ausgabe, wwelche Verteilung vorliegt
  # Anmerkung: all.equal betrachtet dabei, ob die Werte ungef�hr gleich sind
  # all.equals vergleicht standardmaessig bis auf sieben Dezimalstellen
  # es kann aber auch eine eigene Toleranz festgelegt werden
if(Param_mod < Param_med & Param_med < Param_mean){
  print(paste0("Es handelt sich um eine rechtsschiefe Verteilung, da Modalewert: ",
         Param_mod, " < Median: ", Param_med, " < Mittelwert: ", Param_mean, " ist."))
} else if(Param_mod > Param_med & Param_med > Param_mean){
  print(paste0("Es handelt sich um eine linksschiefe Verteilung, da Modalewert: ",
         Param_mod, " > Median: ", Param_med, " > Mittelwert: ", Param_mean, " ist."))
} else if(all.equal(Param_mod,Param_med,Param_mean)){
  print(paste0("Es handelt sich um eine symmetrische Verteilung, da Modalewert: ",
         Param_mod, " = Median: ", Param_med, " = Mittelwert: ", Param_mean, " ist."))
} else{
  print("Es kann keine eindeitige Verteilung festgestellt werden.")
}
  

# Betrachtung der Variable Alter 
print("Betrachtung der Variable Alter") 
print("Lageparameter der Variable Alter" )
gebeAus("Der Interquantilsabstand beträgt: ",calculate_sd(daten$Alter))
gebeAus("Die Standardabweichung beträgt:",calculate_iqr(daten$Alter))
print("-------------------------------------------")

# Betrachtung der Variable Studienfach 
print("Betrachtung der Variable Studienfach") 
print("absolute Häufigkeiten")
print(abs_Haeufigkeiten(Daten$Studienfach))
gebeAus("Entropie der Variable Studienfach: ",entropie(Datensatz_Aufgabe1$Studienfach))
gebeAus("Normierte Entropie der Variable Studienfach: ",normierte_Entropie(Datensatz_Aufgabe1$Studienfach))


# Betrachtung der Variable Programmierinteresse 


# Betrachtung der Variable Matheinteresse 

# Betrachtung der Variable MatheLK 


######################### Bivariate Betachtung der Variablen #####################

print("Kontingenztafel zu Studienfach und Matheinteresse")
print(Kreuztabelle(Datensatz_Aufgabe1$Studienfach,Datensatz_Aufgabe1$Matheinteresse))
print(chi2_function(Datensatz_Aufgabe1$Studienfach,Datensatz_Aufgabe1$Matheinteresse))
print("Kontingenztafel zu Studienfach und Programmierinteresse")
print(Kreuztabelle(Datensatz_Aufgabe1$Studienfach,Datensatz_Aufgabe1$Programmierinteresse))
print(chi2_function(Datensatz_Aufgabe1$Studienfach,Datensatz_Aufgabe1$Programmierinteresse))
print("Kontingenztafel zu Studienfach und MatheLK")
print(Kreuztabelle(Datensatz_Aufgabe1$Studienfach,Datensatz_Aufgabe1$MatheLK))
print(chi2_function(Datensatz_Aufgabe1$Studienfach,Datensatz_Aufgabe1$MatheLK))

print("-------------------------------------------")


# 02 VISUALISIERUNG DER DATEN --------------------------------------------------

print("Visualisierung der Daten zur Variable Alter") 
print("Ausgabe: Histogram der Variable Alter " )
hist(Daten$Alter)
print("Ausgabe: Boxplot der Variable Alter" )
boxplot(Datensatz_Aufgabe1$Alter)

print("-------------------------------------------")
print("Visualisierung der bivariater Zusammenhänge ? ") 







#
