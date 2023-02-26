# *STATISTISCHE AUSWERTUNG Skript 4 Analyse (Desktiption & Visualisierung)*    #
################################################################################
# Statistische Auswertung fuer die die Gruppenarbeit im Modul                  #
# Wissenschaftliches Arbeiten.                                                 #
# Skript 4:                                                                    #
# Die mit Hilde von Skript A und dem R-Helper-Skript erstellten Funktionen     #
# sollen genutzt werden, um eine Analyse (Deskription und Visualisuerung)      #
# der simulierten Daten durchzufuehren.                                         #
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
# Dieses Skript enthaelt Helferfunktionen. Folgende Funktionen sind enthalten: #
# Funktion 1: Abfrage und ggf Umwandlung Dataframe                             #
# Funktion 2: Umwandlung von Variablen in numerische Variablen                 #

# Laden des Helfer-Skriptes


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

# Betrachtung der Variable Alter 
print("Betrachtung der Variable Alter") 
print("Lageparameter der Variable Alter" )
print(summary(Datensatz_Aufgabe1$Alter))
print("Der Interquantilsabstand beträgt: ") 
print(calculate_sd(daten$Alter))
print("Die Standardabweichung beträgt:")
print(calculate_iqr(daten$Alter))
print("-------------------------------------------")

# Betrachtung der Variable Studienfach 
print("Betrachtung der Variable Studienfach") 
print("absolute Häufigkeiten")
print(abs_Haeufigkeiten(Daten$Studienfach))
print("Entropie der Variable Studienfach: ")
print(entropie(Datensatz_Aufgabe1$Studienfach))
print("Normierte Entropie der Variable Studienfach: ")
print(normierte_Entropie(Datensatz_Aufgabe1$Studienfach))


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
