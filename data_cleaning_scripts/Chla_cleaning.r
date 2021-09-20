############################################################
#####  Terrie Klinger's Kasitsna Bay Data              #####
#####  Chlorophyll a data cleaning script              #####
#####  by Rachael E. Blake                             #####
#####  9 June 2017                                     #####
############################################################

## load packages 
library(httr) ; library(tidyverse) ; library(XML) ; library(curl)
library(rvest) 

# NOTE: These data were downloaded from the NERR data repository 
# http://cdmo.baruch.sc.edu/aqs/output/746178.zip on June 30, 2017.  
# This data can't be scraped from the web, but rather has to be requested, and
# is then e-mailed to the requester within an hour or less.

# Requested citation format: NOAA National Estuarine Research Reserve System (NERRS) System-wide Monitoring Program. 
# Data accessed from the NOAA NERRS Centralized Data Management Office website: http://cdmo.baruch.sc.edu/

#####
chla_file <- read.csv("./Chla_NERR/746178.csv", stringsAsFactors=FALSE,
                      header=TRUE, row.names=NULL, strip.white=TRUE)
# print(object.size(chla_file),units="Gb")

chla_file$StationCode <- trimws(chla_file$StationCode)

chla_clean <- chla_file %>%
              dplyr::select(DateTimeStamp, StationCode, ChlFluor, F_ChlFluor) %>%
              dplyr::rename(Station_Code = StationCode) %>%
              dplyr::mutate(Station_Code = toupper(Station_Code)) %>%  # make column all upper case
              dplyr::filter(!is.na(ChlFluor), 
                            F_ChlFluor %in% c("<0>", "<0> (CND)"))   %>%
              dplyr::mutate(Date = sapply(strsplit(as.character(DateTimeStamp), split=" ") , function(x) x[1]),
                            Year = sapply(strsplit(as.character(Date), split="/") , function(x) x[3]),
                            Month = sapply(strsplit(as.character(Date), split="/") , function(x) x[1])) %>%
              dplyr::arrange(Year, Month) 
              
#####
chla_file2 <- read.csv("./Chla_NERR/151505/151505.csv", stringsAsFactors=FALSE,
                       header=TRUE, row.names=NULL, strip.white=TRUE)
# print(object.size(chla_file2),units="Gb")

#
chla2_WQ <- chla_file2[,c(1:51)] # select just the columns coming from the water quality dataset

# neaten column names
colnames(chla2_WQ) <- stringr::str_replace_all(colnames(chla2_WQ), "KACBCWQ_", "")
colnames(chla2_WQ) <- stringr::str_replace_all(colnames(chla2_WQ), "KACDLWQ_", "")
colnames(chla2_WQ) <- stringr::str_replace_all(colnames(chla2_WQ), "KACH3WQ_", "")
colnames(chla2_WQ) <- stringr::str_replace_all(colnames(chla2_WQ), "KACHDWQ_", "")
colnames(chla2_WQ) <- stringr::str_replace_all(colnames(chla2_WQ), "KACHOWQ_", "")
colnames(chla2_WQ) <- stringr::str_replace_all(colnames(chla2_WQ), "KACHSWQ_", "")
colnames(chla2_WQ) <- stringr::str_replace_all(colnames(chla2_WQ), "KACPGWQ_", "")
colnames(chla2_WQ) <- stringr::str_replace_all(colnames(chla2_WQ), "KACSDWQ_", "")
colnames(chla2_WQ) <- stringr::str_replace_all(colnames(chla2_WQ), "KACSEWQ_", "")
colnames(chla2_WQ) <- stringr::str_replace_all(colnames(chla2_WQ), "KACSSWQ_", "")


# make a long dataframe by stacking all columns of the same name, maintaining relationships
chla2_WQ <- do.call(rbind,
                    lapply(seq(2, ncol(chla2_WQ), 5), function(i){
                                                      chla2_WQ[, c(1, i:(i + 4))]
                                                      }
                           )
                    )

# more data cleaning
chla2_WQ_clean <- chla2_WQ %>%
                  dplyr::filter(!is.na(ChlFluor),
                                F_ChlFluor %in% c("<0>", "<0> (CND)")) %>% # remove NA columns
                  dplyr::mutate(Date = sapply(strsplit(as.character(DateTimeStamp), split=" ") , function(x) x[1]),
                                Year = sapply(strsplit(as.character(Date), split="/") , function(x) x[3]),
                                Month = sapply(strsplit(as.character(Date), split="/") , function(x) x[1])) %>%
                  dplyr::group_by(Year, Month) %>%
                  dplyr::mutate(ChlFluor_Month_Mn = mean(ChlFluor)) %>%
                  dplyr::ungroup() %>% 
                  dplyr::select(-isSWMP, -F_Record, -DateTimeStamp, -Date, -ChlFluor, -F_ChlFluor, -Station_Code) %>%
                  dplyr::distinct() %>%
                  dplyr::arrange(Year, Month)

#
chla2_NT <- chla_file2[,c(52:91)] # select just the columns coming from the nutrient dataset

# neaten column names
colnames(chla2_NT) <- stringr::str_replace_all(colnames(chla2_NT), "KACHDNUT_", "")
colnames(chla2_NT) <- stringr::str_replace_all(colnames(chla2_NT), "KACHHNUT_", "")
colnames(chla2_NT) <- stringr::str_replace_all(colnames(chla2_NT), "KACHSNUT_", "")
colnames(chla2_NT) <- stringr::str_replace_all(colnames(chla2_NT), "KACSDNUT_", "")
colnames(chla2_NT) <- stringr::str_replace_all(colnames(chla2_NT), "KACSDNUT_", "")
colnames(chla2_NT) <- stringr::str_replace_all(colnames(chla2_NT), "KACSSNUT_", "")

# make a long dataframe by stacking all columns of the same name, maintaining relationships
chla2_NT2 <- do.call(rbind,
                     lapply(seq(2, ncol(chla2_NT), 8), function(i){
                                                       chla2_NT[, c(1, i:(i + 6))]
                                                       }
                            )
                     )

# more data cleaning
chla2_NT_clean <- chla2_NT2 %>%
                  dplyr::filter(!is.na(CHLA_N),
                                F_CHLA_N %in% c("<0>", "<0> (CSM)")) %>% # remove NA columns
                  dplyr::mutate(Date = sapply(strsplit(as.character(DateTimeStamp), split=" ") , function(x) x[1]),
                                Year = sapply(strsplit(as.character(Date), split="/") , function(x) x[3]),
                                Month = sapply(strsplit(as.character(Date), split="/") , function(x) x[1])) %>%
                  dplyr::arrange(Year, Month) %>%
                  dplyr::select(-isSWMP, -CollMethd, -REP, -F_Record, -Date, -DateTimeStamp, -F_CHLA_N, -Station_Code)



# need to column bind these
# all.equal(chla_clean,chla2_WQ_clean) # more lenient test of whether two dataframes are the same

chla_all <- chla_clean %>%
            dplyr::full_join(chla2_WQ_clean) %>%
            dplyr::full_join(chla2_NT_clean, by=c("Station_Code","DateTimeStamp","Date",
                                                  "Year","Month"))


#NOTE: Not sure about differences in values for ChlFluor and CHLA_N

# write.csv(chla_clean, file = "chla_clean.csv", row.names=FALSE)
# write.csv(chla2_WQ_clean, file = "chla2_WQ_clean.csv", row.names=FALSE)
# write.csv(chla2_NT_clean, file = "chla2_NT_clean.csv", row.names=FALSE)















