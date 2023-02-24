# *STATISTISCHE AUSWERTUNG Aufgabe 2 Skript B*                                 #
################################################################################
# Statistische Auswertung fuer die die Gruppenarbeit im Modul                  #
# Wissenschaftliches Arbeiten.                                                 #
# Skript B (R-Helper-Skript) soll folgendes machen:                            #
# Funkionen-R-Skript 2 soll Helfer-Funktionen enthalten, die nicht selbst zur  #
# Deskription und Visualisierung der Daten verwendet werden, sondern die nur   #
# in Funktionen-Skript 1 Anwendung finden ( interne Funktionen).               #
# Funktionen-R-Skript 2 muss mindestens eine Funktion enthalten.               #
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
library(tibble)   # Datenstruktur


# 00b DATEN LADEN --------------------------------------------------------------
# Einlesen der Daten (als data.frame)
# TODO: relativen Pfad anlegen 
daten <- read_csv(file = "./Datensatz_Aufgabe1.csv")


# 01 FUNKTION - Abfrage ob Dataframe -------------------------------------------
CheckDataFrame <- function(x){      # Daten/Tabelle werden in Funktion uebergeben
  # Kontrolle, ob es schon ein Dataframe ist
  if(is.data.frame(x)){             # Ueberpruefung, ob Data Frame
    return(x);                      # Wenn Dataframe, soll dieses zurueckgegebn werden
  } else if(is.list(x)){
    x <- data.frame(x);          # Wenn nicht, soll es in Dataframe umgewandelt werden
    return(x);
  } else if(is.matrix(x)){
    x <- as.data.frame(x);
    return(x);
  } else if(is_tibble(x)){
    x <- as.data.frame(x);
    return(x);
  }
    
  
}

# 02 FUNKTION - Umwandlung in num Variablen ------------------------------------
convert_ToNum <- function(x){
  # Ueberpruefung, ob schon numerische Variable
  if(is.numeric(x)){
    return(x);                      # Wenn schon num. Variablen, dann diese zurueckgeben
  } else{                           # Wenn nicht num. Variablen:
    x <- as.numeric(x);             # Umwandlung in num. Variablen
    return(x);                      # Rueckgabe d. num. Variablen
  }
    
}

convert_ToNum2 <- function(input){
  # Ueberpruefung, ob schon numerische Variable
  if(is.numeric(input)){
    return(input);                        # Wenn schon num. Variablen, dann diese zurueckgeben
  } else if(is.character(input)){       
    # Es wird zunaechst der Datensatz, welcher als "chr" vorliegt, faktorisiert
    # Dadurch kann er in Numerische Werte ueberfuehrt werden, welche durch die
    # alphabetische Reihenfolge nummeriert werden.
    # Hierzu wurden zusaetzlich die Levels ausgegeben, welche als Legende dienen.
    x <- factor(input)
    y <- as.numeric(x)
    z <- (levels(x))
    list <- list("Umcodierte Daten" = y, "Legende" = z)
    return(list);                      
  } else if(length(unique(input)) == 2){  # Ueberpruefung, ob dichotom
    # Funktioniert auch mit jeder anderen dichotomen Variable.
    return(as.numeric(input=="ja"));
  } else{
    x <- as.numeric(input);               # Umwandlung in num. Variablen
    return(x);                            # Rueckgabe d. num. Variablen
  }
  
}


# 03 FUNKTION - Umwandlung in dichotome Variablen ------------------------------

convert_dichotom <- function(input, merkmal){
  factor(ifelse(input == merkmal,"ja", "nein"))
}
  # Einfache ifelse Abfrage, ob Merkmal des Inputs TRUE/FALSE ist und Änderung
  # in ja/nein. 
  # Bsp.: input = Daten$Studienfach, merkmal = "Statistik"




#04  Hilfsmethode um aus einer Variable einen Vektor zu machen 
erzeuge_Vekor<- function(input){
   as.data.frame(input)[,2]
 }



#05 Umcodierung der Studienfaecher in numerische Variablen

convert_StudyToNum <- function(input){
  x <- factor(input)
  y <- as.numeric(x)
  z <- (levels(x))
  list <- list("Umcodierte Daten" = y, "Legende" = z)
  return(list)
}

  # Es wird zunaechst der Datensatz, welcher als "chr" vorliegt, faktorisiert
  # Dadurch kann er in Numerische Werte ueberfuehrt werden, welche durch die
  # alphabetische Reihenfolge nummeriert werden.
  # Hierzu wurden zusaetzlich die Levels ausgegeben, welche als Legende dienen.


#06 Umcodierung des Mathe LK in numerische Variablen
convert_LKToNum <- function(input){
  return(as.numeric(input=="ja"))
}
  # Funktioniert auch mit jeder anderen dichotomen Variable.