+
  
  ## THIS IS A SCRIPT TO RUN WEEKLY DIAGNOSTICS ON THE METEO DATA ##
  
  
  ## LOAD LIBRARIES ##
  
library(anytime)
library(lubridate) 
library(reshape) 
library(ggplot2) 
library(doBy)
library(grid)
library(dplyr)
library(readr)
library(hms)

## IMPORT DATA ##

meteo <- read_csv("C:/Users/np658/Dropbox (YSE)/Nathalia/Eddy Flux/Meteo/ctrl_Meteo_EP.csv")
meteovar <- read_csv("C:/Users/np658/Dropbox (YSE)/Nathalia/Eddy Flux/Meteo/ctrl_Meteo_EP_Var.csv")
colnames(meteo) <- colnames(meteovar)

## CHANGE DATE ##

meteo$month <- month(as.Date(paste(meteo$TIMESTAMP_1, meteo$TIMESTAMP_2, sep = "-"), format = "%Y-%j"))
meteo$day <- day(as.Date(paste(meteo$TIMESTAMP_1, meteo$TIMESTAMP_2, sep = "-"), format = "%Y-%j"))
meteo$Date <- ymd(with(meteo,paste(TIMESTAMP_1,month,day,sep="-")))

meteo$TIMESTAMP_3 <- as.numeric(meteo$TIMESTAMP_3)
meteo$TIMESTAMP_4 <- as.numeric(meteo$TIMESTAMP_4)
meteo$time <- hms(hours = meteo$TIMESTAMP_3, minutes = meteo$TIMESTAMP_4)

meteo = meteo %>%
  mutate(datetime = 
           as.POSIXct(strptime(paste(Date , paste(time, ":00", sep=""), sep=" "), "%Y-%m-%d %H:%M:%S")))


## CHANGE -9999 TO NA ##

meteo[meteo == -9999.000] = NA 

## PLOT THE VARIABLES FOR THE WEEK ##

# ATMSOSPHERIC TEMPERATURE #
meteo %>% filter(between(Date, as.Date("2023-08-01"), as.Date("2023-08-08"))) %>%
  ggplot(aes(y = Ta_1_1_1, x=datetime)) +
  geom_point(size = 1, colour='red')+
  geom_line(size=.4, colour='red')+
  #stat_smooth(method="lm")+
  scale_y_continuous(limits = c(15, 40))+
  labs(list(x = "Date", y = expression(paste("Valores"))),size=3)+
  scale_fill_manual(values=c("gray","gray","gray","gray","gray","gray","gray"))+
  labs(x="Date & Time",y="T(ºC)")+
  theme_bw(base_size = 10)

# RH #
meteo %>% filter(between(Date, as.Date("2023-08-01"), as.Date("2023-08-08"))) %>%
  ggplot (aes(y = RH_1_1_1, x=datetime)) +
  geom_point(size = 1, colour='blue')+
  geom_line(size=.4, colour='blue')+
  #stat_smooth(method="lm")+
  scale_y_continuous(limits = c(25, 55))+
  labs(list(x = "Data", y = expression(paste("Valores"))),size=3)+
  labs(x="Date",y="RH(%)")+
  theme_bw(base_size = 10)

# PRECIPITATION #
meteo %>% filter(between(Date, as.Date("2023-08-01"), as.Date("2023-08-08"))) %>%
  ggplot(aes(y = P_1_1_1, x=datetime)) +
  geom_point(size = 1, colour='darkblue')+
  geom_line(size=.4, colour='darkblue')+
  #stat_smooth(method="lm")+
  labs(list(x = "Data", y = expression(paste("Valores"))),size=3)+
  labs(x="Date",y="Precipitation (mm)")+
  theme_bw(base_size = 10)

# WIND DIRECTION #
meteo %>% filter(between(Date, as.Date("2023-08-01"), as.Date("2023-08-08"))) %>%
  ggplot(aes(y = WD_1_1_1, x=datetime)) +
  geom_point(size = 1, colour='orange')+
  geom_line(size=.4, colour='orange')+
  #stat_smooth(method="lm")+
  scale_y_continuous(limits = c(50, 110))+
  labs(list(x = "Data", y = expression(paste("Valores"))),size=3)+
  labs(x="Date",y="Wind Direction(º)")+
  theme_bw(base_size = 10)

# WIND SPEED #
meteo %>% filter(between(Date, as.Date("2023-08-01"), as.Date("2023-08-08"))) %>%
  ggplot(aes(y = MWS_1_1_1, x=datetime)) +
  geom_point(size = 1, colour='orange')+
  geom_line(size=.4, colour='orange')+
  #stat_smooth(method="lm")+
  scale_y_continuous(limits = c(0, 5))+
  labs(list(x = "Data", y = expression(paste("Valores"))),size=3)+
  labs(x="Date",y="Wind Speed (m/s)")+
  theme_bw(base_size = 10)

# PAR #
meteo %>% filter(between(Date, as.Date("2023-08-01"), as.Date("2023-08-08"))) %>%
  ggplot(aes(y = PPFD_1_1_1, x=datetime)) +
  geom_point(size = 1, colour='green')+
  geom_line(size=.4, colour='green')+
  #stat_smooth(method="lm")+
  scale_y_continuous(limits = c(0, 1000))+
  labs(list(x = "Data", y = expression(paste("Valores"))),size=3)+
  labs(x="Date",y="PAR ()")+
  theme_bw(base_size = 10)

# PRESSURE #
meteo %>% filter(between(Date, as.Date("2023-08-01"), as.Date("2023-08-08"))) %>%
  ggplot(aes(y = Pa_1_1_1, x=datetime)) +
  geom_point(size = 1, colour='blue')+
  geom_line(size=.4, colour='blue')+
  #stat_smooth(method="lm")+
  labs(list(x = "Data", y = expression(paste("Valores"))),size=3)+
  labs(x="Date",y="Atmospheric pressure()")+
  theme_bw(base_size = 10)

# SOIL WATER CONTENT #
meteo %>% filter(between(Date, as.Date("2023-08-01"), as.Date("2023-08-08"))) %>%
  ggplot(aes(datetime)) +
  geom_line(aes(y = SWC_1_1_1), color = "gray10", size = 0.8, alpha = 0.8) +
  geom_line(aes(y = SWC_2_1_1), color = "blue", size = 0.8, alpha = 0.5) +
  geom_line(aes(y = SWC_3_1_1), color = "darkred", size = 0.8, alpha = 0.5) +
  geom_line(aes(y = SWC_4_1_1), color = "goldenrod", size = 0.8, alpha = 0.5) +
  geom_line(aes(y = SWC_5_1_1), color = "purple", size = 0.8, alpha = 0.5) +
  theme_bw()

# SOIL HEAT FLUX #
meteo %>% filter(between(Date, as.Date("2023-08-01"), as.Date("2023-08-08"))) %>%
  ggplot(aes(datetime)) +
  geom_line(aes(y = SHF_1_1_1), color = "gray10", size = 0.8, alpha = 0.8) +
  geom_line(aes(y = SHF_2_1_1), color = "blue", size = 0.8, alpha = 0.5) +
  geom_line(aes(y = SHF_3_1_1), color = "darkred", size = 0.8, alpha = 0.5) +
  geom_line(aes(y = SHF_4_1_1), color = "goldenrod", size = 0.8, alpha = 0.5) +
  geom_line(aes(y = SHF_5_1_1), color = "purple", size = 0.8, alpha = 0.5) +
  theme_bw()
