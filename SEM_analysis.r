#################################################################
#####  Terrie Klinger's Kasitsna Bay Data                   #####
#####  Structural Equation Modeling script                  #####
#####  by Rachael E. Blake                                  #####
#####  22 June 2017                                         #####
#################################################################

# load packages
library(lavaan) ; library(dplyr) ; library(car) ; library(ggm) ; library(semPlot)  
library(semTools)


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

sem1a_model <- 'FUCUS_TOTAL ~ ATemp_YearlyMn + Barnacles 
                Mytilus ~ ATemp_YearlyMn + mn_yr_discharge + FUCUS_TOTAL
                Barnacles ~ mn_yr_discharge'

sem1a <- sem(sem1a_model, data=PerCov_FWT)

summary(sem1a, rsquare=T, standardized=T)

semPaths(sem1a, "std")

#

sem1b_model <- 'FUCUS_TOTAL ~ ATemp_YearlyMn + Barnacles 
                Mytilus ~ ATemp_YearlyMn + mn_yr_discharge + Barnacles
                Barnacles ~ mn_yr_discharge'

sem1b <- sem(sem1b_model, data=PerCov_FWT)

summary(sem1b, rsquare=T, standardized=T)

semPaths(sem1b, "std")

#

sem1c_model <- 'FUCUS_TOTAL ~ ATemp_YearlyMn + Barnacles 
                Mytilus ~ ATemp_YearlyMn + mn_yr_discharge + Barnacles
                Barnacles ~ mn_yr_discharge + ATemp_YearlyMn'

sem1c <- sem(sem1c_model, data=PerCov_FWT)

summary(sem1c, rsquare=T, standardized=T)

semPaths(sem1c, "std")

#

sem1d_model <- 'FUCUS_TOTAL ~ ATemp_YearlyMn + Barnacles + mn_yr_discharge
                Mytilus ~ ATemp_YearlyMn + mn_yr_discharge + Barnacles
                Barnacles ~ mn_yr_discharge'

sem1d <- sem(sem1d_model, data=PerCov_FWT)

summary(sem1d, rsquare=T, standardized=T)

semPaths(sem1d, "std")

#

sem1e_model <- 'FUCUS_TOTAL ~ ATemp_YearlyMn 
                Mytilus ~ ATemp_YearlyMn + mn_yr_discharge + Barnacles
                Barnacles ~ mn_yr_discharge + FUCUS_TOTAL
                '

sem1e <- sem(sem1e_model, data=PerCov_FWT)

summary(sem1e, rsquare=T, standardized=T)
modificationIndices(sem1e, standardized=F)
parameterEstimates(sem1e)

semPaths(sem1e, "std")

###

sem2_model <- 'FUCUS_TOTAL ~ ATemp_YearlyMn + Spr_Days_Less_0 + Summ_Days_More_15
               Mytilus ~ ATemp_YearlyMn + mn_yr_discharge + Spr_Days_Less_0 + Summ_Days_More_15 + Barnacles
               Barnacles ~ mn_yr_discharge + Spr_Days_Less_0 + Summ_Days_More_15 + FUCUS_TOTAL
               '

sem2 <- sem(sem2_model, data=PerCov_FWT)

summary(sem2, rsquare=T, standardized=T, fit.measures=T)
modificationIndices(sem2, standardized=F)

semPaths(sem2, "std")  

#

sem2a_model <- 'FUCUS_TOTAL ~ ATemp_YearlyMn + Summ_Days_More_15 + mn_yr_discharge
                Mytilus ~ ATemp_YearlyMn + mn_yr_discharge + Spr_Days_Less_0 + Summ_Days_More_15 + Barnacles
                Barnacles ~ mn_yr_discharge + FUCUS_TOTAL
                '

sem2a <- sem(sem2a_model, data=PerCov_FWT)

summary(sem2a, rsquare=T, standardized=T, fit.measures=T)
modificationIndices(sem2a, standardized=F)
parameterEstimates(sem2a)

semPaths(sem2a, "std")  

#

sem2b_model <- 'FUCUS_TOTAL ~ ATemp_YearlyMn + Summ_Days_More_15 + mn_yr_discharge
                Mytilus ~ ATemp_YearlyMn + mn_yr_discharge + Spr_Days_Less_0 + Summ_Days_More_15 + Barnacles
                Barnacles ~ mn_yr_discharge + ATemp_YearlyMn + FUCUS_TOTAL
                '

sem2b <- sem(sem2b_model, data=PerCov_FWT)

summary(sem2b, rsquare=T, standardized=T, fit.measures=T)
modificationIndices(sem2b, standardized=F)

semPaths(sem2b, "std")  

#                     

sem2c_model <- 'FUCUS_TOTAL ~ ATemp_YearlyMn  
                Mytilus ~ mn_yr_discharge + Spr_Days_Less_0 + Summ_Days_More_15 + Barnacles
                Barnacles ~ mn_yr_discharge + FUCUS_TOTAL
                '

sem2c <- sem(sem2c_model, data=PerCov_FWT)

summary(sem2c, rsquare=T, standardized=T, fit.measures=T)
modificationIndices(sem2c, standardized=F)

semPaths(sem2c, "std")  

#                          
 
sem2d_model <- 'FUCUS_TOTAL ~ ATemp_YearlyMn 
                Mytilus ~ mn_yr_discharge + Summ_Days_More_15 + Barnacles
                Barnacles ~ mn_yr_discharge + FUCUS_TOTAL 
                '

sem2d <- sem(sem2d_model, data=PerCov_FWT)

summary(sem2d, rsquare=T, standardized=T, fit.measures=T)
modificationIndices(sem2d, standardized=F)

semPaths(sem2d, "std")  

#                     
 
sem2e_model <- 'FUCUS_TOTAL ~ ATemp_YearlyMn + mn_yr_discharge
                Mytilus ~ mn_yr_discharge + Summ_Days_More_15 + Barnacles
                Barnacles ~ mn_yr_discharge + FUCUS_TOTAL
                '

sem2e <- sem(sem2e_model, data=PerCov_FWT)

summary(sem2e, rsquare=T, standardized=T, fit.measures=T)
modificationIndices(sem2e, standardized=F)

semPaths(sem2e, "std")  

#  
 
sem2f_model <- 'FUCUS_TOTAL ~ ATemp_YearlyMn + Summ_Days_More_15 
                Mytilus ~ ATemp_YearlyMn + mn_yr_discharge + Spr_Days_Less_0 + Summ_Days_More_15 + Barnacles
                Barnacles ~ mn_yr_discharge + FUCUS_TOTAL 
                '

sem2f <- sem(sem2f_model, data=PerCov_FWT)

summary(sem2f, rsquare=T, standardized=T, fit.measures=T)
modificationIndices(sem2f, standardized=F)
parameterEstimates(sem2f)
inspect(sem2f, "sample") ; fitted(sem2f) 
residuals(sem2f) ; residuals(sem2f, type="cor")

semPaths(sem2f, "std")  

#  
 
sem2g_model <- 'FUCUS_TOTAL ~ Summ_Days_More_15 
                Mytilus ~ mn_yr_discharge + Spr_Days_Less_0 + Summ_Days_More_15 + Barnacles
                Barnacles ~ mn_yr_discharge + FUCUS_TOTAL
                '

sem2g <- sem(sem2g_model, data=PerCov_FWT)

summary(sem2g, rsquare=T, standardized=T, fit.measures=T)
modificationIndices(sem2g, standardized=F)
parameterEstimates(sem2g)

semPaths(sem2g, "std")  

#  

sem2h_model <- 'FUCUS_TOTAL ~ Summ_Days_More_15 + mn_yr_discharge
                Mytilus ~ mn_yr_discharge + Spr_Days_Less_0 + Summ_Days_More_15 + Barnacles
                Barnacles ~ mn_yr_discharge + FUCUS_TOTAL
                '

sem2h <- sem(sem2h_model, data=PerCov_FWT)

summary(sem2h, rsquare=T, standardized=T, fit.measures=T)
modificationIndices(sem2h, standardized=F)
parameterEstimates(sem2h)

semPaths(sem2h, "std")  

#

sem2i_model <- 'FUCUS_TOTAL ~ ATemp_YearlyMn + Summ_Days_More_15 + mn_yr_discharge + Mytilus
                Mytilus ~ ATemp_YearlyMn + mn_yr_discharge + Spr_Days_Less_0 + Summ_Days_More_15 + Barnacles 
                Barnacles ~ mn_yr_discharge + FUCUS_TOTAL + Spr_Days_Less_0 + Summ_Days_More_15 +  ATemp_YearlyMn 
                '

sem2i <- sem(sem2i_model, data=PerCov_FWT)

summary(sem2i, rsquare=T, standardized=T, fit.measures=T)
modificationIndices(sem2i, standardized=F)
parameterEstimates(sem2i)
inspect(sem2i, "sample") ; fitted(sem2i) 
residuals(sem2i) ; residuals(sem2i, type="cor")

semPaths(sem2i, "std")  

#

sem3_model <- 'FUCUS_TOTAL ~ ATemp_YearlyMn + Summ_Days_More_15 + mn_yr_discharge + Mytilus + Elachista + Pterosiphonia_poly
               Mytilus ~ ATemp_YearlyMn + mn_yr_discharge + Spr_Days_Less_0 + Summ_Days_More_15 + Barnacles 
               Barnacles ~ mn_yr_discharge + FUCUS_TOTAL + Spr_Days_Less_0 + Summ_Days_More_15 + ATemp_YearlyMn + Elachista 
               Pterosiphonia_poly ~ Mytilus 
               '
#Elachista ~ 
sem3 <- sem(sem3_model, data=PerCov_FWT)

summary(sem3, rsquare=T, standardized=T, fit.measures=T)
modificationIndices(sem3, standardized=F)
parameterEstimates(sem3)
inspect(sem3, "sample") ; fitted(sem3) 
residuals(sem3) ; residuals(sem3, type="cor")

semPaths(sem3, "std")  








