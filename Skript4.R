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
library(tidyr)    # Um Funktion gather() zu nutzen (Verbindet zwei Spalten zu einer)


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


################################################################################
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
  
# 01b ERSTELLUNG HILFSFUNKTION -------------------------------------------------
# Funktion zur Zusammenfassung der Daten (Verwendung fuer Plots -> siehe weiter unten)
# Berechnung von Mittelwert und Standardabweichung 
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

# 01c BERECHNUNG MEDIAN & SD VOM ALTER IN ABHAENIGKEIT DES STUDIENFACHES ------- 
# Tabel erstellen, um die Laenge fuer die leeren Data Frames zu ermitteln
Anzahl_Studienfach <- table(daten$Studienfach)

# leere DataFrames erstellen
df_D <- data.frame(matrix(ncol=Anzahl_Studienfach[1], nrow=1))   # DataFrame fuer Data Sience
df_I <- data.frame(matrix(ncol=Anzahl_Studienfach[2], nrow=1))   # DataFrame fuer Informatik
df_M <- data.frame(matrix(ncol=Anzahl_Studienfach[3], nrow=1))   # DataFrame fuer Mathe
df_S <- data.frame(matrix(ncol=Anzahl_Studienfach[4], nrow=1))   # DataFrame fuer Statistik

# Zaehlvariablen (als Hilfe fuer die folgende for-Schleife)
n <- 1
k <- 1
t <- 1
s <- 1

# for-Schleife, um in die vier leeren Data Frames das Alter fuer die jeweiligen
# Studienfaecher zu schreiben
for(i in 1:nrow(daten)){
  if(daten$Studienfach[i] == "Data Science"){
    df_D[n] <- daten$Alter[i]
    n <- n+1
  } else if(daten$Studienfach[i] == "Statistik"){
    df_S[k] <- daten$Alter[i]
    k <- k+1
  } else if(daten$Studienfach[i] == "Mathe"){
    df_M[t] <- daten$Alter[i]
    t <- t+1
  } else if(daten$Studienfach[i] == "Informatik"){
    df_I[s] <- daten$Alter[i]
    s <- s+1
  }
}

# Convertierung in Num aus dem R-Helper-Skript, um dann die data_summary 
# Funktion zu nutzen (benoetigt Eingabe von numerischen Variablen)
df_D <- convert_ToNum2(df_D)
df_I <- convert_ToNum2(df_I)
df_M <- convert_ToNum2(df_M)
df_S <- convert_ToNum2(df_S)

summary_D <- data_summary(df_D)
summary_I <- data_summary(df_I)
summary_M <- data_summary(df_M)
summary_S <- data_summary(df_S)

# Zusammenfassung als ein Data Frame und Ausgabe
summary <- data.frame(summary_D, summary_I, summary_M, summary_S)
print(paste0("Es liegt folgende Zusammenfassung fuer Median und Standardabweichungen vor: "))
print(summary)

# 01d BETRACHTUNG VON VARIABLEN ------------------------------------------------
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
print######################## Bivariate Betachtung der Variablen #####################")
print("Kontingenztafel zu Studienfach und Matheinteresse")
print(Kreuztabelle(Datensatz_Aufgabe1$Studienfach,Datensatz_Aufgabe1$Matheinteresse))
print(chi2_function(Datensatz_Aufgabe1$Studienfach,Datensatz_Aufgabe1$Matheinteresse))
print("-------------Kontingenztafel zu Studienfach und Programmierinteresse-------------")
print(Kreuztabelle(Datensatz_Aufgabe1$Studienfach,Datensatz_Aufgabe1$Programmierinteresse))
print(chi2_function(Datensatz_Aufgabe1$Studienfach,Datensatz_Aufgabe1$Programmierinteresse))
print("-------------Kontingenztafel zu Studienfach und MatheLK-------------")
print(Kreuztabelle(Datensatz_Aufgabe1$Studienfach,Datensatz_Aufgabe1$MatheLK))
print(chi2_function(Datensatz_Aufgabe1$Studienfach,Datensatz_Aufgabe1$MatheLK))
print("-------------------------------------------")



################################################################################
# 02 VISUALISIERUNG DER DATEN --------------------------------------------------
# 02a VISUALISIERUNG IN BEZUG AUF DAS ALTER ------------------------------------
print("Visualisierung der Daten zur Variable Alter") 
print("Ausgabe: Histogram der Variable Alter " )
hist(Daten$Alter)
print("Ausgabe: Boxplot der Variable Alter" )
boxplot(Datensatz_Aufgabe1$Alter)

# 02b VISUALISERUNG ALTER UND STUDIENFACH --------------------------------------
# VIOLINPLOT
# mittels ggplot2 und geom-violin (Violinplot)
vio1 <- ggplot(daten, aes(x = daten$Studienfach, y = daten$Alter)) + geom_violin(trim=FALSE)
# Median und Range mit hinzufuegen (Nutzung der Funktion data_summary)
vio1 + stat_summary(fun.data=data_summary, geom="pointrange")
print(vio1)

# 02c VISUALISERUNG ALTER UND INTERESSEN ---------------------------------------
# VIOLINPLOT
# mittels ggplot2 und geom-violin (Violinplot)
# Zunaechst muessen das Matheinteresse und das Programmierinteresse zu einer 
# Spalte zusammengefasst werden
df_interesse <- gather(daten, key = "Interesseart", value = "Interesse", -Studienfach, -...1,-MatheLK,-Alter)
vio2 <- ggplot(df_interesse, aes(x = Studienfach, y = Interesse, fill = Interesseart)) + 
    geom_violin(scale = "width", trim = FALSE) +
    facet_grid(rows = vars(Interesseart))
# Median und Range mit hinzufuegen (Nutzung der Funktion data_summary)
vio2 + stat_summary(fun.data=data_summary, geom="pointrange")
print(vio2)

# 02d VISUALISIERUNG VON 3 VARIABLEN -------------------------------------------
# Visualisierung von dem Programmier- sowie Matheinteresse und dem Studienfach
print("-------------------------------------------")
print("Visualisierung von 3 Variablen in einer Graphik")
testplot <- visualisiere3Variablen(Datensatz_Aufgabe1$Programmierinteresse,Datensatz_Aufgabe1$Matheinteresse,"Programmieren","MatheInteresse",Datensatz_Aufgabe1$Studienfach)
plot(testplot)
print("-------------------------------------------")








#
