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


# 03 FUNKTION - Umwandlung in dichotome Variablen ------------------------------









#