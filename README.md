# Oceano_NOAA
Projet BECOMING, calcul données NOAA sondes in situ

# SUIVI SONDES CDT 
# COMPARTIMENT : Récifs coralliens

# Setting working environment 
setwd("C:/Users/naomi.scholten/Documents/R_Corail")

# packages installation
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("reshape2")
install.packages("reshape")

# Data set
Data <- read.csv2("Tanaraki_nao.csv") # importation des data
Data$Prof <- as.numeric(as.character(Data$Prof))
Data$Temp <- as.numeric(as.character(Data$Temp))
Data$Semaine <- as.numeric(as.character(Data$Semaine))
Data$Mois <- factor(Data$Mois, levels=c(1,2,3,4,5,6,7,8,9,10,11,12), labels=c("Jan","Fev","Mars","Avr","Mai","Juin","Juil","Aou","Sept","Oct","Nov","Dec"))

# Calcul moyennes températures hebdomadaires, mensuelles
temp_hebdo <- Data %>% group_by(Annee,Semaine) %>% select(Annee,Mois,Jour,Semaine,Temp) %>% mutate(Temp_hebdo = mean(Temp, na.rm =T))
#temp_hedbo$Temp_hebdo <- round(temp_hedbo$Temp_hebdo,1)
temp_mens <- Data %>% group_by(Annee,Mois) %>% select(Annee,Mois,Jour,Semaine,Temp) %>% mutate(Temp_mens = mean(Temp, na.rm=T))
#temp_mens$Temp_mens <- round(temp_mens$Temp_mens,1)
Total_Data <- cbind(temp_hebdo,temp_mens)
Total_Data <- Total_Data[,-c(7:11)]

# DATA SETTING for analysis
Moy_Temp_mens <- round(mean(C$Temp_mens),1)
Temp_critique <- 29.2
Total_Data$Temp_anomalie <- Total_Data$Temp_hebdo - Temp_critique
Total_Data$Temp_anomalie <- round(Total_Data$Temp_anomalie,1)

################### DEMANDE HELP JOJO
# Boucle à créer pour calculer le Degree Heat Week (accumulation d anomalies de températures positives)
# Info sur le DHW : Mass coral bleaching has been shown to be caused by prolonged periods of thermal stress. The DHW product accumulates # any HotSpots greater than 1 °C over a 12- week window, thus showing how stressful conditions have been for corals in the last three 
# months. It is a cumulative measurement of the intensity and duration of thermal stress, and is expressed in the unit °C-weeks.

Total_Data$DHW <- 0
for (i in 1:nrow(Total_Data)) {
  if (Temp_anomalie >= 1) {
    Temp_anomalie[i] = Temp_anomalie[i]+ Temp_anomalie[i+1]
  }
  else {
    print("ok")
  }
}

Total_Data$DHW <- 0
for (i in 1:nrow(Total_Data)) {
  if (Temp_anomalie >= 1) {
    DHW[i] = Temp_anomalie[i]+ Temp_anomalie[i+1]
  }
  else {
    print("ok")
  }
}
