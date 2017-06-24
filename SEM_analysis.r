#################################################################
#####  Terrie Klinger's Kasitsna Bay Data                   #####
#####  Structural Equation Modeling script                  #####
#####  by Rachael E. Blake                                  #####
#####  22 June 2017                                         #####
#################################################################

# load packages
library(lavaan) ; library(dplyr) ; library(car) ; library(ggm) ; library(semPlot)


# source the data cleaning script to get the % cover data
source("Data_Cleaning_K_Bay.r")     # dataframe is called PerCov_clean

# Freshwater discharge data
source("Fresh_Discharge_cleaning.r")

# Air temperature data
source("AirTemp_cleaning.r")


# add in the environmental data

spr <- spring_a_temp %>%
       dplyr::select(Year, ATemp_YearMn, Num_Day_Less_0) %>% 
       dplyr::distinct()

sum <- summer_a_temp %>%
       dplyr::select(Year, ATemp_YearMn, Num_Day_More_15) %>% 
       dplyr::distinct()


PerCov_FWT <- PerCov_clean %>%
              dplyr::select(-standard_code, -abbr_code, -Myelophycus, -Halosaccion, 
                            -Colpomenia, -Crustose_coralline, -Callithamnion, 
                            -Porphyra, -erect_coralline, -Acrosiphonia, 
                            -Neorhodomela, -Palmaria_callophylloides) %>%
              dplyr::filter(Treatment == "01") %>%
              dplyr::full_join(FWD_ann_mn, by="Year") %>%
              dplyr::full_join(year_a_temp, by="Year") %>%
              dplyr::rename(ATmp_Sign = Year_Sign,
                            ATemp_Year_Anom = Year_Anom, 
                            ATemp_YearlyMn = ATemp_YearMn,
                            mn_yr_discharge = mean_yearly_discharge_m3s1,
                            FWD_Sign = Sign) %>%
              dplyr::full_join(spr, by="Year") %>%
              dplyr::rename(ATemp_SpringMn = ATemp_YearMn, 
                            Spr_Days_Less_0 = Num_Day_Less_0) %>%
              dplyr::full_join(sum, by="Year") %>%
              dplyr::rename(ATemp_SummerMn = ATemp_YearMn, 
                            Summ_Days_More_15 = Num_Day_More_15) %>%
              dplyr::filter(Year != "2015") %>%
              dplyr::arrange(Year)


#########
## SEM ##
#########

sem1_model <- 'FUCUS_TOTAL ~ ATemp_YearlyMn + Barnacles + Mytilus
               Mytilus ~ ATemp_YearlyMn + mn_yr_discharge
               Barnacles ~ mn_yr_discharge'
              # ATemp_YearlyMn ~~ mn_yr_discharge'

sem1 <- sem(sem1_model, data=PerCov_FWT)

summary(sem1, rsquare=T, standardized=T)

semPaths(sem1, "std")                    
 
#

sem2_model <- 'FUCUS_TOTAL ~ ATemp_YearlyMn + Barnacles + Mytilus
               Mytilus ~ ATemp_YearlyMn + mn_yr_discharge
               Barnacles ~ mn_yr_discharge
               '

sem2 <- sem(sem2_model, data=PerCov_FWT)

summary(sem2, rsquare=T, standardized=T)

semPaths(sem2, "std")  


                     
                     
                     



