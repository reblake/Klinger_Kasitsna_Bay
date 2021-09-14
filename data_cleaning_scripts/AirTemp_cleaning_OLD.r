##################################################
### OLD DATASET, NOT USING THIS SCRIPT ANYMORE
### Saving for archival purposes only
### June 15, 2017
##################################################



############################################################
#####  Terrie Klinger's Kasitsna Bay Data              #####
#####  Air Temperature data cleaning script            #####
#####  by Rachael E. Blake                             #####
#####  9 June 2017                                     #####
############################################################

## load packages (order matters)
library(httr) ; library(plyr) ; library(XML) ; library(curl)
library(rvest) ; library(tidyr) ; library(stringr) ; library(dplyr)


first_file <- read.csv("./Homer_NERRS_Station/KACHOMET_June03_07.csv", skip=2, stringsAsFactors=FALSE,
                        header=TRUE, row.names=NULL, strip.white=TRUE)

second_file <- read.csv("./Homer_NERRS_Station/KACHOMET_08-12.csv", skip=2, stringsAsFactors=FALSE,
                        header=TRUE, row.names=NULL, strip.white=TRUE)

third_file <- read.csv("./Homer_NERRS_Station/KACHOMET_13_16.csv", skip=2, stringsAsFactors=FALSE,
                        header=TRUE, row.names=NULL, strip.white=TRUE)

a_temp_raw1 <- bind_rows(third_file, second_file)
a_temp_raw <- bind_rows(a_temp_raw1, first_file)

a_temp <- a_temp_raw %>%
          dplyr::select(-X, -X.1) %>%
          dplyr::filter(Station_Code == "kachomet") %>%
          dplyr::mutate(Year = substr(DateTimeStamp, 7, 10),
                        Month = sapply(strsplit(as.character(DateTimeStamp), split="/") , function(x) x[1]), 
                        Day = sapply(strsplit(as.character(DateTimeStamp), split="/") , function(x) x[2]), 
                        Time = substr(DateTimeStamp, 12, 16)) %>%
          dplyr::select(Station_Code, ATemp, Year, Month, Day, Time) %>%
          dplyr::arrange(Year, Month, Day)


# dummy dataframe to add in missing months of 2003 for nice plotting purpose
dummy_2003 <- data.frame(Year = c("2003","2003","2003","2003","2003"),
                         Month = c("01","02","03","04","05"),
                         Day = c("01","01","01","01","01"), 
                         stringsAsFactors=FALSE)

dummy_2003$YrMnDy <- paste(dummy_2003$Year, dummy_2003$Month, dummy_2003$Day, sep="-")
dummy_2003$YrMn <- paste(dummy_2003$Year, dummy_2003$Month, sep="-")
dummy_2003$Month_Sign <- "B"
dummy_2003$Month_Anom <- 0.0000000001
dummy_2003$Ann_mn_all <- 0.0000000001
dummy_2003$ATemp_MonthMn <- 0.0000000001


# making seasonal and annual dataframes
spring_a_temp <- a_temp %>%
                 dplyr::filter(Month %in% c("03", "04", "05"),
                               !is.na(ATemp)) %>%
                 dplyr::mutate(Spr_mn_all = mean(ATemp)) %>%
                 dplyr::group_by(Year, Month, Day) %>%
                 dplyr::mutate(ATemp_DayMn = mean(ATemp)) %>%
                 dplyr::ungroup() %>%
                 dplyr::group_by(Year, Month) %>%
                 dplyr::mutate(ATemp_MonthMn = mean(ATemp)) %>%
                 dplyr::ungroup() %>%
                 dplyr::select(-ATemp, -Time) %>%
                 dplyr::distinct() %>%
                 dplyr::mutate(YrMnDy = paste(Year, Month, Day, sep="-"),
                               Day_Anom = ATemp_DayMn - Spr_mn_all,
                               Month_Anom = ATemp_MonthMn - Spr_mn_all,
                               Day_Sign = ifelse(Day_Anom>0, "A", "B"),
                               Month_Sign = ifelse(Month_Anom>0, "A", "B")) %>%
                 dplyr::group_by(Year) %>%
                 dplyr::mutate(Num_Day_Less_0 = sum(ATemp_DayMn < 0, na.rm=TRUE)) %>%
                 dplyr::ungroup() 
  
summer_a_temp <- a_temp %>%
                 dplyr::filter(Month %in% c("06", "07", "08"),
                               !is.na(ATemp)) %>%
                 dplyr::mutate(Sum_mn_all = mean(ATemp)) %>%
                 dplyr::group_by(Year, Month, Day) %>%
                 dplyr::mutate(ATemp_DayMn = mean(ATemp)) %>%
                 dplyr::ungroup() %>%
                 dplyr::group_by(Year, Month) %>%
                 dplyr::mutate(ATemp_MonthMn = mean(ATemp)) %>%
                 dplyr::ungroup() %>%
                 dplyr::select(-ATemp, -Time) %>%
                 dplyr::distinct() %>%
                 dplyr::mutate(YrMnDy = paste(Year, Month, Day, sep="-"),
                               Day_Anom = ATemp_DayMn - Sum_mn_all,
                               Month_Anom = ATemp_MonthMn - Sum_mn_all,
                               Day_Sign = ifelse(Day_Anom>0, "A", "B"),
                               Month_Sign = ifelse(Month_Anom>0, "A", "B")) %>%
                 dplyr::group_by(Year) %>%
                 dplyr::mutate(Num_Day_More_15 = sum(ATemp_DayMn > 15, na.rm=TRUE)) %>%
                 dplyr::ungroup()  
  
fall_a_temp <- a_temp %>%
               dplyr::filter(Month %in% c("09", "10", "11"),
                             !is.na(ATemp)) %>%
               dplyr::mutate(Fal_mn_all = mean(ATemp)) %>%
               dplyr::group_by(Year, Month, Day) %>%
               dplyr::mutate(ATemp_DayMn = mean(ATemp)) %>%
               dplyr::ungroup() %>%
               dplyr::group_by(Year, Month) %>%
               dplyr::mutate(ATemp_MonthMn = mean(ATemp)) %>%
               dplyr::ungroup() %>%
               dplyr::select(-ATemp, -Time) %>%
               dplyr::distinct() %>%
               dplyr::mutate(YrMnDy = paste(Year, Month, Day, sep="-"),
                             Day_Anom = ATemp_DayMn - Fal_mn_all,
                             Month_Anom = ATemp_MonthMn - Fal_mn_all,
                             Day_Sign = ifelse(Day_Anom>0, "A", "B"),
                             Month_Sign = ifelse(Month_Anom>0, "A", "B")) %>%
               dplyr::group_by(Year) %>%
               dplyr::mutate(Num_Day_Less_0 = sum(ATemp_DayMn < 0, na.rm=TRUE)) %>%
               dplyr::ungroup()    

winter_a_temp <- a_temp %>%
                 dplyr::filter(Month %in% c("12", "01", "02"),
                               !is.na(ATemp)) %>%
                 dplyr::mutate(Win_mn_all = mean(ATemp)) %>%
                 dplyr::group_by(Year, Month, Day) %>%
                 dplyr::mutate(ATemp_DayMn = mean(ATemp)) %>%
                 dplyr::ungroup() %>%
                 dplyr::group_by(Year, Month) %>%
                 dplyr::mutate(ATemp_MonthMn = mean(ATemp)) %>%
                 dplyr::ungroup() %>%
                 dplyr::select(-ATemp, -Time) %>%
                 dplyr::distinct() %>%
                 dplyr::mutate(YrMnDy = paste(Year, Month, Day, sep="-"),
                               Day_Anom = ATemp_DayMn - Win_mn_all,
                               Month_Anom = ATemp_MonthMn - Win_mn_all,
                               Day_Sign = ifelse(Day_Anom>0, "A", "B"),
                               Month_Sign = ifelse(Month_Anom>0, "A", "B")) %>%
                 dplyr::group_by(Year) %>%
                 dplyr::mutate(Num_Day_Less_neg10 = sum(ATemp_DayMn < -10, na.rm=TRUE)) %>%
                 dplyr::ungroup()    

ann_a_temp <- a_temp %>%
              dplyr::filter(!is.na(ATemp)) %>%
              dplyr::mutate(Ann_mn_all = mean(ATemp)) %>%
              dplyr::group_by(Year, Month) %>%
              dplyr::mutate(ATemp_MonthMn = mean(ATemp)) %>%
              dplyr::ungroup() %>%
              dplyr::select(-ATemp, -Time, -Station_Code) %>%
              dplyr::distinct() %>%
              dplyr::mutate(YrMnDy = paste(Year, Month, Day, sep="-"),
                            YrMn = paste(Year, Month, sep="-"),
                            Month_Anom = ATemp_MonthMn - Ann_mn_all,
                            Month_Sign = ifelse(Month_Anom>0, "A", "B")) %>%
              dplyr::bind_rows(dummy_2003) %>%
              dplyr::arrange(Year, Month, Day)
  
year_a_temp <- a_temp %>%
               dplyr::filter(!is.na(ATemp)) %>%
               dplyr::mutate(Ann_mn_all = mean(ATemp)) %>%
               dplyr::group_by(Year) %>%
               dplyr::mutate(ATemp_YearMn = mean(ATemp)) %>%
               dplyr::ungroup() %>%
               dplyr::select(-Station_Code, -Time, -Day, -Month, -ATemp) %>%
               dplyr::distinct() %>%
               dplyr::mutate(Year_Anom = ATemp_YearMn - Ann_mn_all,
                             Year_Sign = ifelse(Year_Anom>0, "A", "B")) %>%
               dplyr::select(-Ann_mn_all)



  
  
