#################################################################
#####  Terrie Klinger's Kasitsna Bay Data                   #####
#####  Structural Equation Modeling script                  #####
#####  by Rachael E. Blake                                  #####
#####  22 June 2017                                         #####
#################################################################

# load packages
library(lavaan) ; library(dplyr)  


# source the data cleaning script to get the % cover data
source("Data_Cleaning_K_Bay.r")     # dataframe is called PerCov_clean

# Freshwater discharge data
source("Fresh_Discharge_cleaning.r")

# Air temperature data
source("AirTemp_cleaning.r")


# add in the environmental data

PerCov_all <- PerCov_clean %>%
              dplyr::left_join(pdo_ann, by="Year") %>%
              dplyr::filter(Treatment == "01") %>%
              dplyr::select(-standard_code, -plot, -abbr_code, -Treatment, -Block) %>%
              dplyr::group_by(Year) %>%
              dplyr::summarize_each(funs(mean)) %>%
              dplyr::ungroup() %>%
              dplyr::mutate(PDO_Sign = ifelse(PDO_anul_mn>0, "A", "B")) %>%
              dplyr::full_join(FWD_ann_mn, by="Year") %>%
              dplyr::full_join(year_a_temp, by="Year") %>%
              dplyr::rename(ATmp_Sign = Year_Sign,
                            ATemp_Year_Anom = Year_Anom, 
                            mn_yr_discharge = mean_yearly_discharge_m3s1,
                            FWD_Sign = Sign) %>%
              dplyr::filter(Year != "2015")


