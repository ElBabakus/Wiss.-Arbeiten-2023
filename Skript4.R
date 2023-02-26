# *STATISTISCHE AUSWERTUNG Skript 4 Analyse (Desktiption & Visualisierung)*    #
################################################################################
# Statistische Auswertung fuer die die Gruppenarbeit im Modul                  #
# Wissenschaftliches Arbeiten.                                                 #
# Skript 4:                                                                    #
# Die mit Hilde von Skript A und dem R-Helper-Skript erstellten Funktionen     #
# sollen genutzt werden, um eine Analyse (Deskription und Visualisuerung)      #
# der simulierten Daten durchzuführen.                                         #
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


# 00c EINBINDEN DES R-HELPER-SKRIPTES & SKRIPT A -------------------------------
# Ladend des Helfer-Skriptes
source("./Aufgabe2_RHelperSkript.R")

# Ladend des Skriptes-A
source("./Aufgabe2_SkriptA.R")


# 00b DATEN LADEN --------------------------------------------------------------
# Einlesen der Daten (als data.frame)
daten <- read.csv2(file = "./Datensatz_Aufgabe1.csv")

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
  # Anmerkung: all.equal betrachtet dabei, ob die Werte ungefähr gleich sind
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
  


# 02 VISUALISIERUNG DER DATEN --------------------------------------------------









#
