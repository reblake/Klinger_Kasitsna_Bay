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
           
           df1 <- df %>% 
                  rename_with(tolower) %>% # make column names all lowercase
                  dplyr::rename(hourlydrybulbtemperatureF = hourlydrybulbtemperature) %>%  # rename column
                  dplyr::select(c(station:report_type, hourlydrybulbtemperatureF)) %>%  # select columns that will be useful
                  mutate(hourlydrybulbtemperatureF = gsub("s", "", hourlydrybulbtemperatureF),
                         hourlydrybulbtemperatureF = gsub("*", "", hourlydrybulbtemperatureF))

           return(df1)
           }

lcd_list <- lapply(years, get_lcd, w_station = stations)

lcd_df <-  lcd_list %>% 
           purrr::reduce(full_join) 

a_temp <- lcd_df %>% 
          mutate(hourlydrybulbtemperatureF = ifelse(hourlydrybulbtemperatureF == "*",
                                                    NA_character_, hourlydrybulbtemperatureF),
                 hourlydrybulbtemperatureF = ifelse(hourlydrybulbtemperatureF == "",
                                                    NA_character_, hourlydrybulbtemperatureF)) %>% 
          filter(!(report_type %in% c("SOD  ", "SOM  ", "FM-12", "FM-16"))) %>%  # remove these report types
          mutate(hourlydrybulbtemperatureF = as.integer(hourlydrybulbtemperatureF),
                 hourlydrybulbtemperatureC = (hourlydrybulbtemperatureF-32)*(5/9), # convert from F to C
                 hourlydrybulbtemperatureC = round(hourlydrybulbtemperatureC, 1)) %>%  # round values
          mutate(cal_date = sapply(strsplit(as.character(date), split=" ") , function(x) x[1]),
                 # cal_date = parse_date_time(cal_date, c('ymd','mdy')), 
                 time = sapply(strsplit(as.character(date), split=" ") , function(x) x[2]),
                 year = sapply(strsplit(as.character(cal_date), split="-") , function(x) x[1]),
                 month = sapply(strsplit(as.character(cal_date), split="-") , function(x) x[2]), 
                 day = sapply(strsplit(as.character(cal_date), split="-") , function(x) x[3])) %>%
          arrange(year, month, day)


# making seasonal and annual dataframes
spring_a_temp <- a_temp %>%
                 dplyr::filter(month %in% c("03", "04", "05")) %>%
                 dplyr::select(-hourlydrybulbtemperatureF) %>% 
                 dplyr::mutate(Spr_mn_all = mean(hourlydrybulbtemperatureC, na.rm = TRUE)) %>%
                 dplyr::group_by(year, month, day) %>%
                 dplyr::mutate(ATemp_dayMn = mean(hourlydrybulbtemperatureC, na.rm = TRUE)) %>%
                 dplyr::ungroup() %>%
                 dplyr::group_by(year, month) %>%
                 dplyr::mutate(ATemp_monthMn = mean(hourlydrybulbtemperatureC, na.rm = TRUE)) %>%
                 dplyr::ungroup() %>%
                 dplyr::group_by(year) %>%
                 dplyr::mutate(ATemp_yearMn = mean(hourlydrybulbtemperatureC, na.rm = TRUE),
                               ATemp_Spr_min = min(hourlydrybulbtemperatureC, na.rm = TRUE)) %>%
                 dplyr::ungroup() %>%
                 dplyr::select(-hourlydrybulbtemperatureC, -time, -date, -cal_date, -report_type) %>%
                 dplyr::distinct() %>%
                 dplyr::mutate(YrMnDy = paste(year, month, day, sep="-"),
                               day_Anom = ATemp_dayMn - Spr_mn_all,
                               month_Anom = ATemp_monthMn - Spr_mn_all,
                               day_Sign = ifelse(day_Anom>0, "A", "B"),
                               month_Sign = ifelse(month_Anom>0, "A", "B")) %>%
                 dplyr::group_by(year) %>%
                 dplyr::mutate(Num_day_Less_0 = sum(ATemp_dayMn < 0, na.rm=TRUE)) %>%
                 dplyr::ungroup() 
  
summer_a_temp <- a_temp %>%
                 dplyr::filter(month %in% c("06", "07", "08")) %>%
                 dplyr::select(-hourlydrybulbtemperatureF) %>% 
                 dplyr::mutate(Sum_mn_all = mean(hourlydrybulbtemperatureC, na.rm = TRUE)) %>%
                 dplyr::group_by(year, month, day) %>%
                 dplyr::mutate(ATemp_dayMn = mean(hourlydrybulbtemperatureC, na.rm = TRUE)) %>%
                 dplyr::ungroup() %>%
                 dplyr::group_by(year, month) %>%
                 dplyr::mutate(ATemp_monthMn = mean(hourlydrybulbtemperatureC, na.rm = TRUE)) %>%
                 dplyr::ungroup() %>%
                 dplyr::group_by(year) %>%
                 dplyr::mutate(ATemp_yearMn = mean(hourlydrybulbtemperatureC, na.rm = TRUE),
                               ATemp_Summ_max = max(hourlydrybulbtemperatureC, na.rm = TRUE)) %>%
                 dplyr::ungroup() %>%
                 dplyr::select(-hourlydrybulbtemperatureC, -time, -date, -cal_date, -report_type) %>%
                 dplyr::distinct() %>%
                 dplyr::mutate(YrMnDy = paste(year, month, day, sep="-"),
                               day_Anom = ATemp_dayMn - Sum_mn_all,
                               month_Anom = ATemp_monthMn - Sum_mn_all,
                               day_Sign = ifelse(day_Anom>0, "A", "B"),
                               month_Sign = ifelse(month_Anom>0, "A", "B")) %>%
                 dplyr::group_by(year) %>%
                 dplyr::mutate(Num_day_More_15 = sum(ATemp_dayMn > 15, na.rm=TRUE)) %>%
                 dplyr::ungroup()  
  
fall_a_temp <- a_temp %>%
               dplyr::filter(month %in% c("09", "10", "11")) %>%
               dplyr::select(-hourlydrybulbtemperatureF) %>% 
               dplyr::mutate(Fal_mn_all = mean(hourlydrybulbtemperatureC, na.rm = TRUE)) %>%
               dplyr::group_by(year, month, day) %>%
               dplyr::mutate(ATemp_dayMn = mean(hourlydrybulbtemperatureC, na.rm = TRUE)) %>%
               dplyr::ungroup() %>%
               dplyr::group_by(year, month) %>%
               dplyr::mutate(ATemp_monthMn = mean(hourlydrybulbtemperatureC, na.rm = TRUE)) %>%
               dplyr::ungroup() %>%
               dplyr::select(-hourlydrybulbtemperatureC, -time, -date, -cal_date, -report_type) %>%
               dplyr::distinct() %>%
               dplyr::mutate(YrMnDy = paste(year, month, day, sep="-"),
                             day_Anom = ATemp_dayMn - Fal_mn_all,
                             month_Anom = ATemp_monthMn - Fal_mn_all,
                             day_Sign = ifelse(day_Anom>0, "A", "B"),
                             month_Sign = ifelse(month_Anom>0, "A", "B")) %>%
               dplyr::group_by(year) %>%
               dplyr::mutate(Num_day_Less_0 = sum(ATemp_dayMn < 0, na.rm=TRUE)) %>%
               dplyr::ungroup()    

winter_a_temp <- a_temp %>%
                 dplyr::filter(month %in% c("12", "01", "02")) %>%
                 dplyr::select(-hourlydrybulbtemperatureF) %>% 
                 dplyr::mutate(Win_mn_all = mean(hourlydrybulbtemperatureC, na.rm = TRUE)) %>%
                 dplyr::group_by(year, month, day) %>%
                 dplyr::mutate(ATemp_dayMn = mean(hourlydrybulbtemperatureC, na.rm = TRUE)) %>%
                 dplyr::ungroup() %>%
                 dplyr::group_by(year, month) %>%
                 dplyr::mutate(ATemp_monthMn = mean(hourlydrybulbtemperatureC, na.rm = TRUE)) %>%
                 dplyr::ungroup() %>%
                 dplyr::select(-hourlydrybulbtemperatureC, -time, -date, -cal_date, -report_type) %>%
                 dplyr::distinct() %>%
                 dplyr::mutate(YrMnDy = paste(year, month, day, sep="-"),
                               day_Anom = ATemp_dayMn - Win_mn_all,
                               month_Anom = ATemp_monthMn - Win_mn_all,
                               day_Sign = ifelse(day_Anom>0, "A", "B"),
                               month_Sign = ifelse(month_Anom>0, "A", "B")) %>%
                 dplyr::group_by(year) %>%
                 dplyr::mutate(Num_day_Less_neg10 = sum(ATemp_dayMn < -10, na.rm=TRUE)) %>%
                 dplyr::ungroup()    

ann_a_temp <- a_temp %>%
              dplyr::select(-hourlydrybulbtemperatureF) %>% 
              dplyr::mutate(Ann_mn_all = mean(hourlydrybulbtemperatureC, na.rm = TRUE)) %>%
              dplyr::group_by(year, month) %>%
              dplyr::mutate(ATemp_monthMn = mean(hourlydrybulbtemperatureC, na.rm = TRUE)) %>%
              dplyr::ungroup() %>%
              dplyr::select(-hourlydrybulbtemperatureC, -time, -date, -cal_date, -report_type) %>%
              dplyr::distinct() %>%
              dplyr::mutate(YrMnDy = paste(year, month, day, sep="-"),
                            YrMn = paste(year, month, sep="-"),
                            month_Anom = ATemp_monthMn - Ann_mn_all,
                            month_Sign = ifelse(month_Anom>0, "A", "B")) %>%
              dplyr::arrange(year, month, day)
  
year_a_temp <- a_temp %>%
               dplyr::select(-hourlydrybulbtemperatureF) %>% 
               dplyr::mutate(Ann_mn_all = mean(hourlydrybulbtemperatureC, na.rm = TRUE)) %>%
               dplyr::group_by(year) %>%
               dplyr::mutate(ATemp_yearMn = mean(hourlydrybulbtemperatureC, na.rm = TRUE),
                             ATemp_yearSD = sd(hourlydrybulbtemperatureC, na.rm = TRUE),
                             ATemp_yearSE = ATemp_yearSD/sqrt(n())) %>%
               dplyr::ungroup() %>%
               dplyr::select(-time, -day, -month, -hourlydrybulbtemperatureC, -date, 
                             -cal_date, -report_type) %>%
               dplyr::distinct() %>%
               dplyr::mutate(year_Anom = ATemp_yearMn - Ann_mn_all,
                             year_Sign = ifelse(year_Anom>0, "A", "B")) %>%
               dplyr::select(-Ann_mn_all)


# write_csv(ann_a_temp, "./data_clean/air_temp_annual_clean.csv")
# write_csv(spring_a_temp, "./data_clean/air_temp_spring_clean.csv")
# write_csv(summer_a_temp, "./data_clean/air_temp_summer_clean.csv")
# write_csv(fall_a_temp, "./data_clean/air_temp_fall_clean.csv")
# write_csv(winter_a_temp, "./data_clean/air_temp_winter_clean.csv")
# write_csv(year_a_temp, "./data_clean/air_temp_year_clean.csv")
  
