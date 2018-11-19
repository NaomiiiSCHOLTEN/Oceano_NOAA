# setwd("C:/Users/ULG/ULiege/RStudio/DMSP-DMSO_ts") # to be able to compile report from File - Compile report
Sys.setenv(TZ = "UTC") # to define cfr you wrk with date
Sys.setlocale("LC_TIME", "English") # English as local language

# install package if required only
if(!require(readr)){install.packages("readr")}
if(!require(dplyr)){install.packages("dplyr")} 
if(!require(lubridate)){install.packages("lubridate")} 
if(!require(zoo)){install.packages("zoo")} 
if(!require(pastecs)){install.packages("pastecs")} 
if(!require(xlsx)){install.packages("xlsx")} 


##################### tanaraki ##################### 

# Data set
Data <- read_delim("Tanaraki_nao.csv", delim = ";", trim_ws = TRUE) # importation des data avec red_delim te permet d'avoir directement les variables au bon format
Data$Month <- factor(Data$Month, levels=c(1,2,3,4,5,6,7,8,9,10,11,12), labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Dec"))

# Data summary
summary(Data)
boxplot(Temp~Week, data = Data)
boxplot(Temp~Month, data = Data)

# Mean temperature : daily, weekly, monthly
temp_week <- Data %>% group_by(Year,Week) %>% summarize(Temp_week = mean(Temp, na.rm=T), sd.Temp_week = sd(Temp, na.rm=T), nb = n())
Tmin.week <- round(min(temp_week$Temp_week), digits = 2)
Tmax.week <- round(max(temp_week$Temp_week), digits = 2)
subset(temp_week, Temp_week == Tmin.week, c(Year,Week,Temp_week))
subset(temp_week, Temp_week == Tmax.week, c(Year,Week,Temp_week))
temp_month <- Data %>% group_by(Year,Month) %>% summarize(Temp_month = mean(Temp, na.rm=T), sd.Temp_month = sd(Temp, na.rm=T), nb = n())
Tmin.month <- round(min(temp_month$Temp_month), digits = 2)
Tmax.month <- round(max(temp_month$Temp_month), digits = 2)
subset(temp_month, Temp_month == Tmin.month, c(Year,Month,Temp_month))
subset(temp_month, Temp_month == Tmax.month, c(Year,Month,Temp_month))

# DATA SETTING for analysis
Moy_Temp_mens <- round(mean(temp_week$Temp_week),1)
Temp_critique <- 29.2 # a revoir pour cette valeur 
temp_week$Temp_anomaly <- round(temp_week$Temp_week - Temp_critique,1)
#saveRDS(temp_week,"Results/resultats.rds")
#write.csv2(temp_week, file = "MyData.csv",sep="\t", dec=".")

# Bleaching threshold operation : DHW
# 1. Replace negative value by 0
temp_week$SetValue <- ifelse(temp_week$Temp_anomaly<0, 0, temp_week$Temp_anomaly)
# 2. Start adding anomalies when reaching the first temp_anomaly >= 1
seuil <- temp_week[temp_week$Temp_anomaly>=1,]
GoForIT <- unique(rbind(seuil[1,],temp_week[-c(1:6),]))
# 3. Creating a "somme glissante", so the DHW is based on 12 weeks anomalies temperatures
GoForIT$DHW <- cumsum(GoForIT$SetValue) # addition glissante
filter(x, rep(1, n))[-c(1, length(x))]
GoForIT %>%
  filter(SetValue[c(1:11),]) %>%
  mutate(DHW <- cumsum(SetValue))


GoForIT$DHW <- aggregate(GoForIT[c(1:12),by = list(GoForIT$DHW),c("SetValue")], FUN = cumsum) 


rowCumSums <- function(DHW) {
  for(i in GoForIT(dim(DHW)[1:12])) { DHW[i,] <- cumsum(x[i,]) }
  }

DHW <- matrix(ncol=0,nrow=0)
for (i in 1:nrow(DHW)) {
  while (i < i + 11) {
    DHW[i] <- cumsum(GoForIT$SetValue)
  }
}

GoForIT %>%
  group_by(SetValue) %>%
  mutate(roll_sum = rollapply(GoForIT$SetValue, 12, sum))
# A tester
#rollsum()
#DHW <- rollapply(GoForIT$SetValue, 12, sum)
#rollapply(GoForIT$SetValue, width = 12, FUN = sum, minimum = )
#rsum
#rsum.filter <- function(x, n = 3L) filter(x, rep(1, n))[-c(1, length(x))]





