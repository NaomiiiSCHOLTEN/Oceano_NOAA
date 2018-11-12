# setwd("C:/Users/ULG/ULiege/RStudio/DMSP-DMSO_ts") # to be able to compile report from File - Compile report
Sys.setenv(TZ = "UTC") # to define cfr you wrk with date
Sys.setlocale("LC_TIME", "English") # English as local language

# install package if required only
if(!require(readr)){install.packages("readr")}
if(!require(dplyr)){install.packages("dplyr")} 
if(!require(lubridate)){install.packages("lubridate")} 


##################### tanaraki ##################### 

# Data set
Data <- read_delim("Tanaraki_nao.csv", delim = ";", trim_ws = TRUE) # importation des data avec red_delim te permet d'avoir directement les varaibles au bon format

Data$Mois <- factor(Data$Mois, levels=c(1,2,3,4,5,6,7,8,9,10,11,12), labels=c("Jan","Fev","Mars","Avr","Mai","Juin","Juil","Aou","Sept","Oct","Nov","Dec"))

# Calcul moyennes températures hebdomadaires, mensuelles
temp_hebdo <- Data %>% group_by(Annee,Semaine) %>% summarize(Temp_hebdo = mean(Temp, na.rm=T), sd.Temp_hebdo = sd(Temp, na.rm=T), nb = n())
temp_mens <- Data %>% group_by(Annee,Mois) %>% summarize(Temp_mens = mean(Temp, na.rm=T), sd.Temp_mens = sd(Temp, na.rm=T), nb = n())

# DATA SETTING for analysis
Moy_Temp_mens <- round(mean(temp_mens$Temp_mens),1)
Temp_critique <- 29.2
temp_hebdo$Temp_anomalie <- round(temp_hebdo$Temp_hebdo - Temp_critique,1)

# for loop
temp_hebdo$DHW <- NA
for (i in 1:nrow(temp_hebdo)) {
  if (temp_hebdo$Temp_anomalie[i] >= 1) {
    temp_hebdo$DHW[i] = temp_hebdo$Temp_anomalie[i] + temp_hebdo$Temp_anomalie[i+1]
  }
  else {
    temp_hebdo$DHW[i] = "ok" # if you want Temp_anamalie result, then replace by: temp_hebdo$DHW[i] = temp_hebdo$Temp_anomalie[i]
  }
}
