############################################################
#####  Terrie Klinger's Kasitsna Bay Data              #####
#####  Air Temperature data cleaning script            #####
#####  by Rachael E. Blake                             #####
#####  9 June 2017                                     #####
############################################################

## load packages 
library(httr) ; library(tidyverse) ; library(XML) ; library(curl)
library(rvest) ; library(tidyr) ; library(stringr) ; library(lubridate)
library(rnoaa)

# NOTE: This air temperature data was downloaded from https://www.ncdc.noaa.gov/cdo-web/datatools/lcd
# on 15 June 2017.  This data can't be scraped from the web, but rather has to be requested, and
# is then e-mailed to the requester within an hour or less.  

# NOTE: As of 13 Sept 2021, the manually downloaded LCD data are no longer available as per Mark Seiderman at NOAA.
# See this email message: 
#########################
# Subject: 	Re: ms*Fwd: CDO support request: 2708954
# Date: 	Mon, 13 Sep 2021 15:03:17 -0400
# From: 	Mark Seiderman - NOAA Federal <mark.seiderman@noaa.gov>
# To: 	rblake@sesync.org
# 
# 
# Rachael,
# 
# Unfortunately we do not offer this product anymore. 
# 
# Thanks,
# 
# Mark Seiderman
# Meteorologist
# NOAA's National Centers for Environmental Information (NCEI)
# Climatic Science and Services Division (CSSD)
# 151 Patton Ave
# Asheville, NC 28801
# Mark.Seiderman@noaa.gov
# Phone: 828-271-4800 ext. 3171
# To be kept aware of NCEI planned and unplanned Outages see: https://www.ncei.noaa.gov/alerts
# To be kept aware of changes to NCEI and NESDIS' products and services, see: https://www.nesdis.noaa.gov/content/notice-changes
# 
# The newly formed NCEI merges the National Climatic Data Center (NCDC), the National Geophysical Data Center (NGDC), and the National Oceanographic Data Center (NODC).
#########################


# New approach is to use the R package `rnoaa`, which uses the 
# NOAA API (docs here: https://www.ncdc.noaa.gov/cdo-web/webservices/v2)
# NOTE: The temperature values are in degrees F, so will have to be converted to C in the R code below.   

stations <- "70341025507"

years <- c(1999:2021)

get_lcd <- function(w_station, w_year){
           df <- rnoaa::lcd(station = w_station, year = w_year) # download data
           df <- df %>% 
                 rename(hourlydrybulbtemperatureF = hourlydrybulbtemperature) %>%  # rename column  
                 mutate(across(hourlydrybulbtemperatureF, as.numeric)) %>% 
                 mutate(across(!select(hourlydrybulbtemperatureF), as.character))
                 
                 
           return(df)
           }

lcd_list <- lapply(years, get_lcd, w_station = stations)

lcd_df <-  lcd_list %>% 
           purrr::reduce(full_join) 


a_temp <- lcd_df %>% 
          select(date, hourlydrybulbtemperatureF) %>%
          mutate(across(hourlydrybulbtemperatureF, as.numeric)) %>% 
          mutate(hourlydrybulbtemperatureC = (hourlydrybulbtemperatureF -32)*(5/9),
                 hourlydrybulbtemperatureC = round(hourlydrybulbtemperatureC, 1))  # convert from F to C




  

    
          dplyr::mutate(DATE = str_replace_all(DATE, "/", "-"),
                        Time = sapply(strsplit(as.character(DATE), split=" ") , function(x) x[2]),
                        Date = sapply(strsplit(as.character(DATE), split=" ") , function(x) x[1]), 
                        Date = parse_date_time(Date, c('ymd','mdy')), 
                        Year = sapply(strsplit(as.character(Date), split="-") , function(x) x[1]),
                        Month = sapply(strsplit(as.character(Date), split="-") , function(x) x[2]), 
                        Day = sapply(strsplit(as.character(Date), split="-") , function(x) x[3]) 
                        ) %>%
          dplyr::filter(!(HOURLYDRYBULBTEMPC %in% c(""," ", "3.0s","*","1.7s","15.0s"))) %>%  #Note: look in LDC documentation for codes
          dplyr::mutate_at(vars(HOURLYDRYBULBTEMPC),funs(as.numeric)) %>%
          dplyr::arrange(Year, Month, Day)


# making seasonal and annual dataframes
spring_a_temp <- a_temp %>%
                 dplyr::filter(Month %in% c("03", "04", "05")) %>%
                 dplyr::mutate(Spr_mn_all = mean(HOURLYDRYBULBTEMPC)) %>%
                 dplyr::group_by(Year, Month, Day) %>%
                 dplyr::mutate(ATemp_DayMn = mean(HOURLYDRYBULBTEMPC)) %>%
                 dplyr::ungroup() %>%
                 dplyr::group_by(Year, Month) %>%
                 dplyr::mutate(ATemp_MonthMn = mean(HOURLYDRYBULBTEMPC)) %>%
                 dplyr::ungroup() %>%
                 dplyr::group_by(Year) %>%
                 dplyr::mutate(ATemp_YearMn = mean(HOURLYDRYBULBTEMPC),
                               ATemp_Spr_min = min(HOURLYDRYBULBTEMPC)) %>%
                 dplyr::ungroup() %>%
                 dplyr::select(-HOURLYDRYBULBTEMPC, -Time, -DATE, -Date) %>%
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
                 dplyr::filter(Month %in% c("06", "07", "08")) %>%
                 dplyr::mutate(Sum_mn_all = mean(HOURLYDRYBULBTEMPC)) %>%
                 dplyr::group_by(Year, Month, Day) %>%
                 dplyr::mutate(ATemp_DayMn = mean(HOURLYDRYBULBTEMPC)) %>%
                 dplyr::ungroup() %>%
                 dplyr::group_by(Year, Month) %>%
                 dplyr::mutate(ATemp_MonthMn = mean(HOURLYDRYBULBTEMPC)) %>%
                 dplyr::ungroup() %>%
                 dplyr::group_by(Year) %>%
                 dplyr::mutate(ATemp_YearMn = mean(HOURLYDRYBULBTEMPC),
                               ATemp_Summ_max = max(HOURLYDRYBULBTEMPC)) %>%
                 dplyr::ungroup() %>%
                 dplyr::select(-HOURLYDRYBULBTEMPC, -Time, -DATE, -Date) %>%
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
               dplyr::filter(Month %in% c("09", "10", "11")) %>%
               dplyr::mutate(Fal_mn_all = mean(HOURLYDRYBULBTEMPC)) %>%
               dplyr::group_by(Year, Month, Day) %>%
               dplyr::mutate(ATemp_DayMn = mean(HOURLYDRYBULBTEMPC)) %>%
               dplyr::ungroup() %>%
               dplyr::group_by(Year, Month) %>%
               dplyr::mutate(ATemp_MonthMn = mean(HOURLYDRYBULBTEMPC)) %>%
               dplyr::ungroup() %>%
               dplyr::select(-HOURLYDRYBULBTEMPC, -Time, -DATE, -Date) %>%
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
                 dplyr::filter(Month %in% c("12", "01", "02")) %>%
                 dplyr::mutate(Win_mn_all = mean(HOURLYDRYBULBTEMPC)) %>%
                 dplyr::group_by(Year, Month, Day) %>%
                 dplyr::mutate(ATemp_DayMn = mean(HOURLYDRYBULBTEMPC)) %>%
                 dplyr::ungroup() %>%
                 dplyr::group_by(Year, Month) %>%
                 dplyr::mutate(ATemp_MonthMn = mean(HOURLYDRYBULBTEMPC)) %>%
                 dplyr::ungroup() %>%
                 dplyr::select(-HOURLYDRYBULBTEMPC, -Time, -DATE, -Date) %>%
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
              dplyr::mutate(Ann_mn_all = mean(HOURLYDRYBULBTEMPC)) %>%
              dplyr::group_by(Year, Month) %>%
              dplyr::mutate(ATemp_MonthMn = mean(HOURLYDRYBULBTEMPC)) %>%
              dplyr::ungroup() %>%
              dplyr::select(-HOURLYDRYBULBTEMPC, -Time, -DATE, -Date) %>%
              dplyr::distinct() %>%
              dplyr::mutate(YrMnDy = paste(Year, Month, Day, sep="-"),
                            YrMn = paste(Year, Month, sep="-"),
                            Month_Anom = ATemp_MonthMn - Ann_mn_all,
                            Month_Sign = ifelse(Month_Anom>0, "A", "B")) %>%
              dplyr::arrange(Year, Month, Day)
  
year_a_temp <- a_temp %>%
               dplyr::mutate(Ann_mn_all = mean(HOURLYDRYBULBTEMPC)) %>%
               dplyr::group_by(Year) %>%
               dplyr::mutate(ATemp_YearMn = mean(HOURLYDRYBULBTEMPC),
                             ATemp_YearSD = sd(HOURLYDRYBULBTEMPC),
                             ATemp_YearSE = ATemp_YearSD/sqrt(n())) %>%
               dplyr::ungroup() %>%
               dplyr::select(-Time, -Day, -Month, -HOURLYDRYBULBTEMPC, -DATE, -Date) %>%
               dplyr::distinct() %>%
               dplyr::mutate(Year_Anom = ATemp_YearMn - Ann_mn_all,
                             Year_Sign = ifelse(Year_Anom>0, "A", "B")) %>%
               dplyr::select(-Ann_mn_all)



  
  
