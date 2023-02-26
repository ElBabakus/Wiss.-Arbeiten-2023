# *STATISTISCHE AUSWERTUNG Aufgabe 2 Skript A*                                  #
################################################################################
# Statistische Auswertung fuer die die Gruppenarbeit im Modul                  #
# Wissenschaftliches Arbeiten.                                                 #
# Skript A enthaelt folgende Funktionen:                                       #
# (a) Eine Funktion, die verschiedene geeignete deskriptive Statistike         #
#     fÃ¼r metrische Variablen berechnet und ausgibt                            #
# (b) Eine Funktion, die verschiedene geeignete deskriptive Statistiken        #
#     fÃ¼r kategoriale Variablen berechnet und ausgibt                          #
# (c) Eine Funktion, die geeignete deskriptive bivariate Statistiken fÃ¼r       #
#     den Zusammenhang zwischen zwei kategorialen Variablen                    #
#     berechnet ausgibt                                                        #
# (d) Eine Funktion, die geeignete deskriptive bivariate Statistiken fÃ¼r       #
#     den Zusammengang zwischen einer metrischen und einer                     #
#     dichotomen Variablen berechnet und ausgibt                               #
# (e) Eine Funktion, die eine mindestens ordinal skalierte Variable            #
#     quantilbasiert kategorisiert (z.B. in â€žniedrigâ€œ, â€žmittelâ€œ, â€žhochâ€œ)       #
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
source("./Aufgabe2_RHelperSkript.R")
# daten <- read_csv(file = "\\Datensatz_Aufgabe1.csv")
library(readr)

# daten <- read_csv(file = "./Datensatz_Aufgabe1.csv")
 Datensatz_Aufgabe1 <- read_delim("Datensatz_Aufgabe1.csv", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)

daten <- Datensatz_Aufgabe1
Daten <- Datensatz_Aufgabe1


# 01 DESKRIPTIVE STATISTIK - metr. Variablen -----------------------------------
# Eine Funktion, die verschiedene geeignete deskriptive Statistiken         #
# fÃ¼r metrische Variablen berechnet und ausgibt                                #




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

#ergebnis <- calculate_metrParam(daten)
#print(ergebnis)

# 02 DESKRIPTIVE STATISTIK - kategor. Variablen --------------------------------
# Eine Funktion, die verschiedene geeignete deskriptive Statistiken            #
# fÃ¼r kategoriale Variablen berechnet und ausgibt                              #


# Funktion berechnet die absoluten HÃ¤ufigkeiten 
# der jeweiligen AusprÃ¤gungen
abs_Haeufigkeiten <- function(x){
  table(x)
}

# Funktion berechnet die relativen HÃ¤ufigkeiten 
# der jeweiligen AusprÃ¤gungen
rel_Haeufigkeiten <- function(x){
  prop.table(abs_Haeufigkeiten(x))
}

# Funktion Berechnet den Modalwert zu einem Merkmal 
# Es wird zunÃ¤chst der Vektor gegildet dann 
# die AusprÃ¤gung mit dem GrÃ¶ÃŸten Vorkommen bestimmt und zurÃ¼ckgegeben
berechne_modalwert <- function(x){
  test <- abs_Haeufigkeiten(x)
  names(test )[which.max(test)]
}

# Funktion berechnet die Entropie eines Merkmals 
# Siehe Skript WS22/23 Seite 72 Kap. 5.2

entropie <- function(Variable) {
  # Berechne Absolute und relative HÃ¤ufigkeiten 
  absolute <-abs_Haeufigkeiten(Variable)
  freq <- rel_Haeufigkeiten(Variable)
  vec_abs <- erzeuge_Vekor(absolute)
  vec_freq <- erzeuge_Vekor(freq)
  # entferne leere Felder auch null wegen der kommenden Log funktion 
  vec_abs<-vec_abs[vec_abs>0]    
  vec_freq<-vec_freq[vec_freq>0] 
  
  # berechne die Entropie als : 
  # LOG von Anzahl_Objekte minus  Summe der gewichteten Log-Werte der Absoluten KlassenhÃ¤ufigkeiten
  log2(length(Variable)) - sum(vec_freq * log2(vec_abs)) 
}

# Funktion berechnet die normierte Entropie eines Merkmals 
# Siehe Skript WS22/23 Seite 73 Kap. 5.2
normierte_Entropie<- function(Variable){
  absolute <-abs_Haeufigkeiten(Variable)
  vec_abs <- erzeuge_Vekor(absolute)
  entropie(Variable)/log2(length(vec_abs))
}

# 03 DESKRIPTIVE BIV. STATISTIK - zwei kategor. Variablen ----------------------
# Eine Funktion, die geeignete deskriptive bivariate Statistiken fÃ¼r           #
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
  cat("Chi2-Koeff.:", chi2,"\n")
  cat("Cramer-Koeff.:", sqrt(chi2/(n*(min(length(levels(X)), length(levels(Y)))-1))),"\n")
  cat("Pearson-Koeff.:", sqrt(chi2/(chi2+n)))
  
}

# deskriptive bivariate Statistiken für den Zusammenhang zwischen zwei kategorialen Variablen
# Matheinteresse und Programmierinteresse mit Kontingenztafel:
f <- function(){
  A <- matrix(0,7,7)
  for (i in 1:100){
    A[Daten$Matheinteresse[i], Daten$Programmierinteresse[i]] <- A[Daten$Matheinteresse[i], Daten$Programmierinteresse[i]] + 1
  }
  return(A)
}

# Funktion erzeugt für zwei übergebene Variablen 
# eine Kontingenztafel : eine Zelle der Tabelle enthält 
# die Anzahl wie häufig eine Kombination der Ausprägungen gemeinsam vorkommt
Kreuztabelle <-function(variable1, variable2){
  test <- table(variable1,variable2)
  return(test)
} 

# Funktion liefert die Verteilung der Veriable2 bei gegebenen (fixierter) Ausprägung der Variable1 
# Die Ausprägung des dritten  Parameters ZeileAuspraegung ist in Hochkomata "" zu setzen
# Beispiel : KreuzTabelle_ZeilenAuspraegungFixiert(Datensatz_Aufgabe1$Studienfach,Datensatz_Aufgabe1$Programmierinteresse,"informatik")
#            liefert die Verteilung des Programmierinteresse bei fixiertem Studiengang = "Informatik"  
KreuzTabelle_ZeilenAuspraegungFixiert <- function(variable1,variable2,ZeileAuspraegung){
  test <- table(variable1,variable2)
  return(test[ZeileAuspraegung,])
}

# Funktion liefert die Verteilung der Veriable1 bei gegebenen (fixierter) Ausprägung der Variable2 
# Die Ausprägung des dritten  Parameters SpaltenAuspraegung ist in Hochkomata "" zu setzen
# Beispiel : KreuzTabelle_ZeilenAuspraegungFixiert(Datensatz_Aufgabe1$Studienfach,Datensatz_Aufgabe1$Programmierinteresse,"informatik")
#            liefert die Verteilung des Programmierinteresse bei fixiertem Studiengang = "Informatik"
KreuzTabelle_SpaltenAupPraegungFixiert <- function(variable1,variable2,SpaltenAuspraegung){
  test <- table(variable1,variable2)
  return(test[,SpaltenAuspraegung])
}

# 04 DESKRIPTIVE BIV. STATISTIK - metr. & dichot. Variable ---------------------
#  Eine Funktion, die geeignete deskriptive bivariate Statistiken fÃ¼r          #
# den Zusammengang zwischen einer metrischen und einer                         #
# dichotomen Variablen berechnet und ausgibt                                   #




# 05 KATRGORISIEREN ORD. SKAL. VARIABLE ----------------------------------------
# Eine Funktion, die eine mindestens ordinal skalierte Variable                #
# quantilbasiert kategorisiert (z.B. in â€žniedrigâ€œ, â€žmittelâ€œ, â€žhochâ€œ)           #

categorize <- function(variable){
  if(is.numeric(variable)) # Abfrage ob eigegebene Variable numerisch ist
  {
    variable_sorted <- sort(variable) # gewählte Variable wird sortiert
    rbind(quantile(variable_sorted,
                   probs = seq(0, 1, 1/3), names = FALSE), c("erster Wert",
                                                            "kleinstes Drittel",
                                                            "mittleres Drittel",
                                                            "höchstes Drittel"))
    # Erzeuge Matrix welche die Quantilwerte in drittel schritten ausgibt.
    # Schrittgröße is auf 1/3 festgesetzt damit es Übersichtlich bleibt.
  }
  else{
    return("Variable is not ordinal scaled") 
    # Fehlermeldung bei nicht numerischer Eingabe
  }
}



# 06 VISUALISIERUNG KATEGOR. VARIABLEN -----------------------------------------
# Eine Funktion, die eine geeignete Visualisierung von drei oder vier          #
# kategorialen Variablen erstellt                                              #




# 07 DESKRIPT. & VISULAISIERUNG - Freiwillig ------------------------------------
# Freiwillig: weitere zur Deskription und Visualisierung geeignete             #
# Funktionen                                                                   #
#
erzeuge_Mosaicplot<-function(){
  A <- f()
  mosaicplot(A, xlab = "Programmierinteresse", ylab = "Matheinteresse", main = "")  
}

#
