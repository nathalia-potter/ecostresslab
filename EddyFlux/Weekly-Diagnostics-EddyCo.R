

## LOAD LIBRARIES ##
library(tidyverse)
library(chron)
library(lubridate)
library(RColorBrewer)
library(purrr)

## LOAD DATA ##
path <- "C:/Users/np658/Dropbox (YSE)/ECOTRESS/EddyFlux/PreProcess/control_tower/2023/"

eddyco_files <- paste0(path,list.files(path, pattern = "*.csv"))
select_eddyco_files <- eddyco_files[6782:7117]
eddyco_lists <- lapply(select_eddyco_files, read_csv)
eddyco <- do.call(rbind, eddyco_lists)

group_size <- 36002
eddyco_average <- eddyco %>%
  group_by(group_id = rep(1:(n() %/% group_size + 1), each = group_size, length.out = n())) %>%
  summarize_all(.funs = mean, na.rm = TRUE) %>%
  ungroup() %>%
  select(-group_id)

eddyco_average[eddyco_average == -9999] = NA

## GET DATE FROM FILE NAMES ##

allfiledates = data.frame(year = as.numeric(substr(select_eddyco_files, 79, 82)),
                          day = as.numeric(substr(select_eddyco_files, 83, 85)),
                          hour = as.numeric(substr(select_eddyco_files, 86, 87)),
                          minutes = substr(select_eddyco_files, 88, 89))

## CHANGE DATE ##

allfiledates$month <- month(as.Date(paste(allfiledates$year, allfiledates$day, sep = "-"), format = "%Y-%j"))
allfiledates$day <- day(as.Date(paste(allfiledates$year, allfiledates$day, sep = "-"), format = "%Y-%j"))
allfiledates$Date <- ymd(with(allfiledates,paste(year,month,day,sep="-")))

allfiledates %>% 
  unite(time, hour, minutes, sep = ":", format = "%H:%M")
  
allfiledates <- allfiledates %>% 
  mutate(time = paste0(hour,":",minutes)) 
 
allfiledates = allfiledates %>% 
  mutate(datetime = 
         as.POSIXct(strptime(paste(Date , paste(time, ":00", sep=""), sep=" "), "%Y-%m-%d %H:%M:%S")))

## ADD DATE AND TIME COMPONENTS TO EDDYCO AVERAGE ##

eddyco_average$datetime <- allfiledates$datetime

## GRAPH TEMPERATURE ##
eddyco_average %>% 
  ggplot(aes(y = T, x = datetime)) +
  geom_point(size = 1, colour = 'red') +
  geom_line(size = 0.4, colour = 'red') +
  labs(x = "Date & Time", y = expression(paste("Valores")), size = 3) +
  labs(x = "Date & Time", y = "T(ºC)") +
  theme_bw(base_size = 10)

## GRAPH CO2 ##
eddyco_average %>% 
  ggplot (aes(y = CO2, x=datetime)) +
  geom_point(size = 1, colour='purple')+
  geom_line(size=.4, colour='purple')+
  labs(list(x = "Data", y = expression(paste("Valores"))),size=3)+
  labs(x="Date",y="CO2 (mmol/m3)")+
  theme_bw(base_size = 10)

## GRAPH WATER VAPOR ##
eddyco_average %>% 
  ggplot (aes(y = H2O, x=datetime)) +
  geom_point(size = 1, colour='blue')+
  geom_line(size=.4, colour='blue')+
  labs(list(x = "Data", y = expression(paste("Valores"))),size=3)+
  labs(x="Date",y="H2O (mmol/m3)")+
  theme_bw(base_size = 10)

## GRAPH WIND DIRECTION ##
eddyco_average %>% 
  ggplot(aes(y = wd, x=datetime)) +
  geom_point(size = 1, colour='orange')+
  geom_line(size=.4, colour='orange')+
  labs(list(x = "Data", y = expression(paste("Valores"))),size=3)+
  labs(x="Date",y="Wind Direction(º)")+
  theme_bw(base_size = 10)

## GRAPH WIND SPEED ##
eddyco_average %>%
  ggplot(aes(y = vh, x=datetime)) +
  geom_point(size = 1, colour='orange')+
  geom_line(size=.4, colour='orange')+
  labs(list(x = "Data", y = expression(paste("Valores"))),size=3)+
  labs(x="Date",y="Wind Speed (m/s)")+
  theme_bw(base_size = 10)



