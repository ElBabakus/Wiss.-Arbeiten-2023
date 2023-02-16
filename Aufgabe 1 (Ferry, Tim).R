# Alter
  Alter <- round(rnorm(100, mean = 25, sd = 2),0)

# Studienfach
Studienfach <- sample(c("Statistik", "Data Science", "Mathe", "Informatik"), 100, replace = TRUE, prob = c(0.35,0.35,0.20,0.10))

# Matheinteresse
Matheinteresse <- rep(0,100)
Matheinteresse[which(Studienfach =="Statistik")] <- sample(1:7, length(which(Studienfach=="Mathe")), replace = TRUE, prob = c(0,0,0,0.1,0.15,0.25,0.5))
Matheinteresse[which(Studienfach =="Data Science")] <- sample(1:7, length(which(Studienfach=="Mathe")), replace = TRUE, prob = c(0,0.05,0.05,0.1,0.2,0.2,0.4))
Matheinteresse[which(Studienfach =="Mathe")] <- sample(1:7, length(which(Studienfach=="Mathe")), replace = TRUE, prob = c(0,0,0,0.1,0.1,0.2,0.6))
Matheinteresse[which(Studienfach =="Informatik")] <- sample(1:7, length(which(Studienfach=="Mathe")), replace = TRUE, prob = c(0.05,0.1,0.1,0.1,0.1,0.15,0.4))

# Programmierinteresse
Programmierinteresse <- rep(0,100)
Programmierinteresse[which(Studienfach =="Statistik")] <- sample(1:7, length(which(Studienfach=="Mathe")), replace = TRUE, prob = c(0,0,0.05,0.1,0.15,0.3,0.4))
Programmierinteresse[which(Studienfach =="Data Science")] <- sample(1:7, length(which(Studienfach=="Mathe")), replace = TRUE, prob = c(0,0,0,0.1,0.1,0.3,0.5))
Programmierinteresse[which(Studienfach =="Mathe")] <- sample(1:7, length(which(Studienfach=="Mathe")), replace = TRUE, prob = c(0.5,0.05,0.1,0.1,0.2,0.2,0.3))
Programmierinteresse[which(Studienfach =="Informatik")] <- sample(1:7, length(which(Studienfach=="Mathe")), replace = TRUE, prob = c(0,0,0,0.05,0.1,0.25,0.6))

# MatheLK
MatheLK <- sample(c("ja","nein"), 100, replace=TRUE, prob = c(0.75,0.25))


#Daten <- function(){
#  Alter <- round(rnorm(100, mean = 25, sd = 2),0)
#  Studienfach <- sample(c("Statistik", "Data Science", "Mathe", "Informatik"), 100, replace = TRUE, prob = c(0.35,0.35,0.20,0.10))
#  Matheinteresse <- rep(0,100)
#  Matheinteresse[which(Studienfach =="Statistik")] <- sample(1:7, length(which(Studienfach=="Mathe")), replace = TRUE, prob = c(0,0,0,0.1,0.15,0.25,0.5))
#  Matheinteresse[which(Studienfach =="Data Science")] <- sample(1:7, length(which(Studienfach=="Mathe")), replace = TRUE, prob = c(0,0.05,0.05,0.1,0.2,0.2,0.4))
#  Matheinteresse[which(Studienfach =="Mathe")] <- sample(1:7, length(which(Studienfach=="Mathe")), replace = TRUE, prob = c(0,0,0,0.1,0.1,0.2,0.6))
#  Matheinteresse[which(Studienfach =="Informatik")] <- sample(1:7, length(which(Studienfach=="Mathe")), replace = TRUE, prob = c(0.05,0.1,0.1,0.1,0.1,0.15,0.4))
#  Programmierinteresse <- rep(0,100)
#  Programmierinteresse[which(Studienfach =="Statistik")] <- sample(1:7, length(which(Studienfach=="Mathe")), replace = TRUE, prob = c(0,0,0.05,0.1,0.15,0.3,0.4))
#  Programmierinteresse[which(Studienfach =="Data Science")] <- sample(1:7, length(which(Studienfach=="Mathe")), replace = TRUE, prob = c(0,0,0,0.1,0.1,0.3,0.5))
#  Programmierinteresse[which(Studienfach =="Mathe")] <- sample(1:7, length(which(Studienfach=="Mathe")), replace = TRUE, prob = c(0.5,0.05,0.1,0.1,0.2,0.2,0.3))
#  Programmierinteresse[which(Studienfach =="Informatik")] <- sample(1:7, length(which(Studienfach=="Mathe")), replace = TRUE, prob = c(0,0,0,0.05,0.1,0.25,0.6))
#  LK <- sample(c("ja", "nein"), 100, replace = TRUE, prob = c(0.75,0.25))
#  
#     
# Daten <- data.frame(matrix(ncol = 6, nrow = 100))
# colnames(Daten) <- c("ID","Alter","Studienfach","Matheinteresse","Programmierinteresse","LK")
# Daten$ID <- 1:100
# Daten$Alter <- Alter
# Daten$Studienfach <- Studienfach
# Daten$Matheinteresse <- Matheinteresse
# Daten$Programmierinteresse <- Programmierinteresse
# Daten$LK <- LK
# return(Daten)
#}

Daten <- data.frame(Alter,MatheLK,Matheinteresse,Programmierinteresse, Studienfach)
write.csv2(Daten, file = "C:/Users/ferry/OneDrive/Dokumente/__UNI/3. Semester/Wiss. Arbeiten/Datensatz_Aufgabe1.csv")

