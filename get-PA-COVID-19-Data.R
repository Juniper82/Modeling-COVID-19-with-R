# Author: Scott Dolan
# Date: 12-06-2020
# Class: STAT 401
# Assignment: Final Project

# This scipt is to download COVID-19 Data, and Political Voter Data from PA Gov site and create data set for MLR model

library(tidyverse)
library(rio)

# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014") 
# Clean workspace
rm(list=ls())

path = '/home/scott/GIT/Modeling-COVID-19-with-R/'

if(getwd() != path){setwd(path)
  print('Path set succesfully')}

sources = read.table('Sources-list.txt',header=T)
sources[1]

start.date <- '2020-06-01'
#end.date <- '2020-12-12'# Comment out for most current data

# dat1 - Death Count Data
names.keep1 <- c('New Deaths','County','2018 Population')
dat1 <- read_csv(sources$source[2]) %>%  
  mutate_at(.vars = "Date of Death", ~as.Date(.,format = "%m/%d/%y")) %>% 
  dplyr::rename(., Date = `Date of Death`,daily.death.count = `New Deaths`,population.2018 = `2018 Population`) %>%
  dplyr::filter(Date >= as.Date(start.date)) %>% # & Date <= as.Date(end.date)) %>%
  dplyr::rename(., County = `County Name`) %>%
  dplyr::select(Date,County,daily.death.count,population.2018) %>%
  dplyr::arrange(.,County,Date)

counties <- unique(dat1$County)

# dat2 - Hospitalization Data
names.keep2 <- c('COVID-19 Patients Hospitalized','COVID-19 Patients on Ventilators','Airborn isolation beds, percent available','Adult ICU beds, percent available')
dat2 <- read_csv(sources$source[1]) %>% 
  mutate_at(.vars = "Date of data", ~as.Date(.,format = "%m/%d/%y")) %>%
  dplyr::rename(., Date = `Date of data`, patients.hospitalized = `COVID-19 Patients Hospitalized`, patients.on.ventilators = `COVID-19 Patients on Ventilators`, airborn.isolation.beds = `Airborn isolation beds, percent available`, adult.ICU.beds = `Adult ICU beds, percent available`) %>%
  dplyr::filter(Date >= as.Date(start.date)) %>% # & Date <= as.Date(end.date)) %>%
  dplyr::filter(.,County %in% counties) %>%
  dplyr::select(Date,County,patients.hospitalized,patients.on.ventilators, airborn.isolation.beds, adult.ICU.beds) %>%
  dplyr::arrange(.,County,Date)

# dat3 - New Case Data
names.keep3 <- c('new.cases','County')
dat3 <- read_csv(sources$source[3]) %>% 
  mutate_at(.vars = "Date", ~as.Date(.,format = "%m/%d/%y")) %>%
  dplyr::filter(Date >= as.Date(start.date)) %>% # & Date <= as.Date(end.date)) %>%
  dplyr::rename(., County = Jurisdiction) %>%
  dplyr::rename(.,new.cases = `New Cases`) %>%
  dplyr::filter(.,County %in% counties) %>%
  dplyr::select(Date,new.cases,County) %>%
  dplyr::arrange(.,County,Date)

county.keep <- unique(dat2$County)
start.date = min(dat2$Date,dat1$Date,dat3$Date)
end.date = max(dat2$Date,dat1$Date,dat3$Date)

dat1 <- dplyr::filter(dat1,County %in% county.keep) 
dat3 <- dplyr::filter(dat3,County %in% county.keep) 

# Merging Data into single data.frame 
Data <- dat1 %>% left_join(.,dat2, by=c("County","Date")) %>% 
  left_join(.,dat3, by = c("County","Date")) %>%
  dplyr::mutate(.,month=format(Date, "%m"))

# Define Regions
southeast <- c('Schuykill','Berks','Lancaster','Chester','Montgomery','Bucks','Delaware','Philadelphia')
southcentral <- c('Blair','Bedford','Huntingdon','Fulton','Franklin','Mifflin','Juniata','Perry','Cumberland','Adams','Dauphin','Lebanon','York')
southwest <- c('Greene','Washington','Beaver','Allegheny','Butler','Fayette','Westmoreland','Armstrong','Indiana','Cambria','Somerset')
northwest <- c('Lawrence','Mercer','Crawford','Erie','Venango','Warren','Forest','Clarion','McKean','Elk','Jefferson','Cameron','Clearfield')
northcentral <- c('Potter','Clinton','Centre','Tioga','Lycoming','Union','Snyder','Northumberland','Montour','Columbia','Sullivan','Bradford')
northeast <- c('Susquehanna','Wyoming','Luzerne','Carbon','Lehigh','Northampton','Monroe','Pike','Lackawanna','Wayne')

SEdat <- Data %>% dplyr::filter(.,County %in% southeast) %>%
  dplyr::mutate(.,region = 'southeast') 
SCdat <- Data %>% dplyr::filter(.,County %in% southcentral) %>%
  dplyr::mutate(.,region = 'southcentral') 
SWdat <- Data %>% dplyr::filter(.,County %in% southwest) %>%
  dplyr::mutate(.,region = 'southwest') 
NWdat <- Data %>% dplyr::filter(.,County %in% northwest) %>%
  dplyr::mutate(.,region = 'northwest') 
NCdat <- Data %>% dplyr::filter(.,County %in% northcentral) %>%
  dplyr::mutate(.,region = 'northcentral')
NEdat <- Data %>% dplyr::filter(.,County %in% northeast) %>%
  dplyr::mutate(.,region = 'northeast') 

Data <- SEdat %>% bind_rows(.,SCdat) %>% bind_rows(.,SWdat) %>% bind_rows(.,NWdat) %>% bind_rows(.,NCdat) %>% bind_rows(.,NEdat) %>% drop_na(.)

write_csv(Data,'PA-COVID-19-Data-Merged.csv')