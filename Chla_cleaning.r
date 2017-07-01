############################################################
#####  Terrie Klinger's Kasitsna Bay Data              #####
#####  Chlorophyll a data cleaning script              #####
#####  by Rachael E. Blake                             #####
#####  9 June 2017                                     #####
############################################################

## load packages (order matters)
library(httr) ; library(plyr) ; library(XML) ; library(curl)
library(rvest) ; library(tidyr) ; library(stringr) ; library(dplyr)

# NOTE: These data were downloaded from the NERR data repository 
# http://cdmo.baruch.sc.edu/aqs/output/746178.zip on June 30, 2017.  
# This data can't be scraped from the web, but rather has to be requested, and
# is then e-mailed to the requester within an hour or less.

# Requested citation format: NOAA National Estuarine Research Reserve System (NERRS) System-wide Monitoring Program. 
# Data accessed from the NOAA NERRS Centralized Data Management Office website: http://cdmo.baruch.sc.edu/


chla_file <- read.csv("./Chla_NERR/746178.csv", stringsAsFactors=FALSE,
                      header=TRUE, row.names=NULL, strip.white=TRUE)

#print(object.size(chla_file),units="Gb")


chla_clean <- chla_file %>%
              dplyr::select(StationCode, DateTimeStamp, Temp, F_Temp,
                            ChlFluor, F_ChlFluor) %>%
              dplyr::filter(!is.na(ChlFluor), 
                            F_ChlFluor %in% c("<0>", "<0> (CND)"))
              




