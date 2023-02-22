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
# daten <- read_csv(file = "\\Datensatz_Aufgabe1.csv")
library(readr)

# daten <- read_csv(file = "./Datensatz_Aufgabe1.csv")
 Datensatz_Aufgabe1 <- read_delim("Datensatz_Aufgabe1.csv", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)

daten <- Datensatz_Aufgabe1
Daten <- Datensatz_Aufgabe1

# 01 DESKRIPTIVE STATISTIK - metr. Variablen -----------------------------------
# Eine Funktion, die verschiedene geeignete deskriptive Statistiken         #
# für metrische Variablen berechnet und ausgibt                                #




# 02 DESKRIPTIVE STATISTIK - kategor. Variablen --------------------------------
# Eine Funktion, die verschiedene geeignete deskriptive Statistiken            #
# für kategoriale Variablen berechnet und ausgibt                              #

# Funktion berechnet die absoluten Häufigkeiten 
# der jeweiligen Ausprägungen
abs_Haeufigkeiten <- function(x){
  table(x)
}

# Funktion berechnet die relativen Häufigkeiten 
# der jeweiligen Ausprägungen
rel_Haeufigkeiten <- function(x){
  prop.table(abs_Haeufigkeiten(x))
}

## Hilfsmethode um aus einer Variablen einen Vektor zu machen 
erzeuge_Vekor<- function(input){
   as.data.frame(input)[,2]
 }

# Funktion Berechnet den Modalwert zu einem Merkmal 
# Es wird zunächst der Vektor gegildet dann 
# die Ausprägung mit dem Größten Vorkommen bestimmt und zurückgegeben
berechne_modalwert <- function(x){
  test <- abs_Haeufigkeiten(x)
  names(test )[which.max(test)]
}

# Funktion berechnet die Entropie eines Merkmals 
# Siehe Skript WS22/23 Seite 72 Kap. 5.2

entropy <- function(Variable) {
  # Berechne Absolute und relative Häufigkeiten 
  absolute <-abs_Haeufigkeiten(Variable)
  freq <- rel_Haeufigkeiten(Variable)
  vec_abs <- erzeuge_Vekor(absolute)
  vec_freq <- erzeuge_Vekor(freq)
  # entferne leere Felder auch null wegen der kommenden Log funktion 
  vec_abs<-vec_abs[vec_abs>0]    
  vec_freq<-vec_freq[vec_freq>0] 
  
  # berechne die Entropie als : 
  # LOG von Anzahl_Objekte minus  Summe der gewichteten Log-Werte der Absoluten Klassenhäufigkeiten
  log2(length(Variable)) - sum(vec_freq * log2(vec_abs)) 
}

# Funktion berechnet die normierte Entropie eines Merkmals 
# Siehe Skript WS22/23 Seite 73 Kap. 5.2
normierte_Entropie<- function(Variable){
  absolute <-abs_Haeufigkeiten(Variable)
  vec_abs <- erzeuge_Vekor(absolute)
  entropy(Variable)/log2(length(vec_abs))
}

# 03 DESKRIPTIVE BIV. STATISTIK - zwei kategor. Variablen ----------------------
# Eine Funktion, die geeignete deskriptive bivariate Statistiken für           #
# den Zusammenhang zwischen zwei kategorialen Variablen                        #  
# berechnet ausgibt                                                            #

# deskriptive bivariate Statistiken f?r den Zusammenhang zwischen zwei kategorialen Variablen
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
