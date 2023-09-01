#################################################################
#####  Terrie Klinger's Kasitsna Bay Data                   #####
#####  Structural Equation Modeling script                  #####
#####  by Rachael E. Blake                                  #####
#####  22 June 2017                                         #####
#####  Updated 25 Aug 2023                                  #####
#################################################################

# load packages
library(lavaan) ; library(tidyverse) ; library(car) ; library(ggm) ; library(semPlot)  
library(semTools) ; library(psych) ; library(stats) ; library(here)


###################################################################################
###### Start here if you need to re-build the input dataframe                ######
###################################################################################

# load the % cover species data
AllData_clean <- read_csv(here("data_clean", "K_Bay_All_Sp_Yrs_Clean.csv"))

# Freshwater discharge data
fwd_ann <- read_csv(here("data_clean", "FWD_annual_mn_clean.csv"))
fwd_mon <- read_csv(here("data_clean", "FWD_monthly_mn_clean.csv"))

# Air temperature data
year_a_temp <- read.csv(here("data_clean", "air_temp_year_clean.csv"))
ann_a_temp <- read.csv(here("data_clean", "air_temp_annual_clean.csv"))
spring_a_temp <- read.csv(here("data_clean", "air_temp_spring_clean.csv"))
summer_a_temp <- read.csv(here("data_clean", "air_temp_summer_clean.csv"))
fall_a_temp <- read.csv(here("data_clean", "air_temp_fall_clean.csv"))
winter_a_temp <- read.csv(here("data_clean", "air_temp_winter_clean.csv"))

# Water temperature data
# WTemp_Yr, WTemp_June, WTemp_Dec
wtemp_all <- read_csv(here("data_clean", "WTemp_all_clean.csv"))   
wtemp_year <- read_csv(here("data_clean", "WTemp_year_clean.csv")) 
wtemp_june <- read_csv(here("data_clean", "WTemp_June_clean.csv")) 
wtemp_dec <- read_csv(here("data_clean", "WTemp_Dec_clean.csv"))   


# add in the environmental data

spr <- spring_a_temp %>%
       dplyr::select(year, ATemp_yearMn, Num_day_Less_0, ATemp_Spr_min) %>% 
       dplyr::rename(ATemp_Spr_Mn = ATemp_yearMn,
                     Spr_Days_Less_0 = Num_day_Less_0) %>% 
       dplyr::distinct()

sum <- summer_a_temp %>%
       dplyr::select(year, ATemp_yearMn, Num_day_More_15, ATemp_Summ_max) %>% 
       dplyr::rename(ATemp_Sum_Mn = ATemp_yearMn,
                     Summ_Days_More_15 = Num_day_More_15) %>% 
       dplyr::distinct()

yratemp <- year_a_temp %>%
           dplyr::select(year, ATemp_yearMn, ATemp_yearSD) 

fwdann <- fwd_ann %>% 
          filter(!(Year %in% c(1999, 2000, 2001)))

wtj <- wtemp_june %>% 
       dplyr::rename(Water_Temp_June_SD = Water_Temp_Monthly_SD,
                     Water_Temp_June_SE = Water_Temp_Monthly_SE)

wtd <- wtemp_dec %>% 
       dplyr::rename(Water_Temp_Dec_SD = Water_Temp_Monthly_SD,
                     Water_Temp_Dec_SE = Water_Temp_Monthly_SE)


PerCov_FWT <- AllData_clean %>%
              dplyr::select(-QUAD, -HALOSACCION, -COLPOMENIA, 
                            -CRUSTOSE_CORALLINE, -CALLITHAMNION, -ERECT_CORALLINE, 
                            -ACROSIPHONIA, -NEORHODOMELA, -PALMARIA) %>%
              dplyr::rename(Year = YEAR) %>%
              dplyr::arrange(Year) %>%
              dplyr::full_join(fwdann, by = "Year") %>%  # join in the freshwater data
              dplyr::rename(year = Year) %>%
              dplyr::full_join(yratemp, by = "year") %>%  # join in the air temp data
              dplyr::rename(mn_yr_discharge = mean_yearly_discharge_m3d1,
                            FWD_Sign = Sign) %>%
              dplyr::full_join(spr, by = "year") %>%  # join in the spring air temp data
              dplyr::full_join(sum, by = "year") %>%  # join in the summer air temp data
              dplyr::rename(Year = year) %>%
              dplyr::full_join(wtemp_year, by = "Year") %>%  # join in the annual water temp data
              dplyr::full_join(wtj, by = "Year") %>%  # join in the June water temp data
              dplyr::full_join(wtd, by = "Year") %>%  # join in the December water temp data
              dplyr::arrange(Year)

# write.csv(PerCov_FWT, file = "K_Bay_All_Data_SEM.csv", row.names=FALSE)
 
###################################################################################
###### Start here if running the SEM but not re-building the input dataframe ######
###################################################################################

PerCov_FWT <- read_csv("K_Bay_All_Data_SEM.csv")


# without any NAs
PerCov_FWT_NA <- PerCov_FWT %>%
                 dplyr::select(-WTemp_Dec_Lag) %>%
                 # dplyr::mutate_at(c(5:20,22,24:50), funs(as.numeric)) %>% # converts select columns to numeric
                 dplyr::group_by() %>%
                 dplyr::mutate(L.SITKANA_scaled = L.SITKANA/10,
                               LOTTIIDAE_scaled = LOTTIIDAE/10,
                               FUCUS_PERCOV_TOTAL_scaled = FUCUS_PERCOV_TOTAL/10) %>%
                 dplyr::ungroup()
                 
                 


#########
# Looking at correlations
pairs.panels(PerCov_FWT_NA[,c(2:22)], smooth = F, density = T, 
             ellipses = F, lm = T, digits = 3, scale = T)

pairs.panels(PerCov_FWT_NA[,c(44, 49, 51, 57, 58, 65:67)], smooth = F, density = T, ellipses = F, lm = T, 
             digits = 3, scale = T)

pairs.panels(PerCov_FWT_NA[,c(44, 49, 51:58, 62)], smooth = F, density = T, ellipses = F, 
             lm = T, digits = 3, scale = T)


#########
## SEM ##
#########

sem1_model <- 'FUCUS_PERCOV_TOTAL ~ ATemp_yearMn + BARNACLES + MYTILUS + Water_Temp_Yearly
               MYTILUS ~ ATemp_yearMn + mn_yr_discharge + Water_Temp_Yearly
               BARNACLES ~ mn_yr_discharge 
               '

sem1 <- sem(sem1_model, data = PerCov_FWT_NA, estimator = "MLM")  # using a robust ML estimator due to some non-normality

AIC(sem1)
fitMeasures(sem1, "pvalue")
summary(sem1, rsquare = T, standardized = T)
parameterEstimates(sem1)
residuals(sem1) ; residuals(sem1, type = "cor")

semPaths(sem1, "std")                    
 
#

sem1a_model <- 'FUCUS_PERCOV_TOTAL ~ ATemp_yearMn + BARNACLES  + Water_Temp_Yearly
                MYTILUS ~ ATemp_yearMn + mn_yr_discharge + FUCUS_PERCOV_TOTAL + Water_Temp_Yearly
                BARNACLES ~ mn_yr_discharge
                '

sem1a <- sem(sem1a_model, data = PerCov_FWT_NA, estimator = "MLM")

AIC(sem1a)
fitMeasures(sem1a, "pvalue")
summary(sem1a, rsquare = T, standardized = T)
residuals(sem1a) ; residuals(sem1a, type = "cor")

semPaths(sem1a, "std")

#

sem1b_model <- 'FUCUS_PERCOV_TOTAL ~ ATemp_yearMn + BARNACLES  + Water_Temp_Yearly
                MYTILUS ~ ATemp_yearMn + mn_yr_discharge + BARNACLES + Water_Temp_Yearly
                BARNACLES ~ mn_yr_discharge'

sem1b <- sem(sem1b_model, data = PerCov_FWT_NA, estimator = "MLM")

AIC(sem1b)
fitMeasures(sem1b, "pvalue")
summary(sem1b, rsquare = T, standardized = T)
residuals(sem1b) ; residuals(sem1b, type = "cor")

semPaths(sem1b, "std")

#

sem1c_model <- 'FUCUS_PERCOV_TOTAL ~ ATemp_yearMn + BARNACLES + Water_Temp_Yearly
                MYTILUS ~ ATemp_yearMn + mn_yr_discharge + BARNACLES + Water_Temp_Yearly
                BARNACLES ~ mn_yr_discharge + ATemp_yearMn'

sem1c <- sem(sem1c_model, data = PerCov_FWT_NA, estimator = "MLM")

AIC(sem1c)
fitMeasures(sem1c, "pvalue")
summary(sem1c, rsquare = T, standardized = T)
residuals(sem1c) ; residuals(sem1c, type = "cor")

semPaths(sem1c, "std")

#

sem1d_model <- 'FUCUS_PERCOV_TOTAL ~ ATemp_yearMn + BARNACLES + mn_yr_discharge + Water_Temp_Yearly
                MYTILUS ~ ATemp_yearMn + mn_yr_discharge + BARNACLES + Water_Temp_Yearly
                BARNACLES ~ mn_yr_discharge'

sem1d <- sem(sem1d_model, data = PerCov_FWT_NA, estimator = "MLM")

AIC(sem1d)
fitMeasures(sem1d, "pvalue")
summary(sem1d, rsquare = T, standardized = T)
residuals(sem1d) ; residuals(sem1d, type = "cor")


semPaths(sem1d, "std")

#

sem1e_model <- 'FUCUS_PERCOV_TOTAL ~ ATemp_yearMn 
                MYTILUS ~ ATemp_yearMn + mn_yr_discharge + BARNACLES
                BARNACLES ~ mn_yr_discharge + FUCUS_PERCOV_TOTAL 
                '#+ Water_Temp_Yearly   + Water_Temp_Yearly
           
sem1e <- sem(sem1e_model, data = PerCov_FWT_NA, estimator = "MLM")

AIC(sem1e)
fitMeasures(sem1e, "pvalue")
summary(sem1e, rsquare = T, standardized = T)
residuals(sem1e) ; residuals(sem1e, type = "cor")
modificationIndices(sem1e, standardized = F)
parameterEstimates(sem1e)

semPaths(sem1e, "std")

#

AIC(sem1, sem1a, sem1b, sem1c, sem1d, sem1e)

#####

sem2_model <- 'FUCUS_PERCOV_TOTAL ~ ATemp_yearMn + Spr_Days_Less_0 + Summ_Days_More_15
               MYTILUS ~ ATemp_yearMn + mn_yr_discharge + Spr_Days_Less_0 + Summ_Days_More_15 + BARNACLES
               BARNACLES ~ mn_yr_discharge + Spr_Days_Less_0 + Summ_Days_More_15 + FUCUS_PERCOV_TOTAL
               '

sem2 <- sem(sem2_model, data = PerCov_FWT_NA, estimator = "MLM")

AIC(sem2)
fitMeasures(sem2, "pvalue")
summary(sem2, rsquare = T, standardized = T, fit.measures = T)
residuals(sem2) ; residuals(sem2, type = "cor")
modificationIndices(sem2, standardized = F)

semPaths(sem2, "std")  

#

sem2a_model <- 'FUCUS_PERCOV_TOTAL ~ ATemp_yearMn + Summ_Days_More_15 + mn_yr_discharge
                MYTILUS ~ ATemp_yearMn + mn_yr_discharge + Summ_Days_More_15 + BARNACLES
                BARNACLES ~ mn_yr_discharge + FUCUS_PERCOV_TOTAL + ATemp_yearMn
                '

sem2a <- sem(sem2a_model, data = PerCov_FWT_NA, estimator = "MLM")

AIC(sem2a)
fitMeasures(sem2a, "pvalue")
summary(sem2a, rsquare = T, standardized = T, fit.measures = T)
residuals(sem2a) ; residuals(sem2a, type = "cor")
modificationIndices(sem2a, standardized = F)
parameterEstimates(sem2a)

semPaths(sem2a, "std")  

#

sem2b_model <- 'FUCUS_PERCOV_TOTAL ~ ATemp_yearMn + Summ_Days_More_15 + mn_yr_discharge + Water_Temp_Dec
                MYTILUS ~ ATemp_yearMn + mn_yr_discharge + Summ_Days_More_15 + BARNACLES + Water_Temp_June
                BARNACLES ~ mn_yr_discharge + ATemp_yearMn + FUCUS_PERCOV_TOTAL + Water_Temp_Dec
                '

sem2b <- sem(sem2b_model, data = PerCov_FWT_NA, estimator = "MLM")

AIC(sem2b)
fitMeasures(sem2b, "pvalue")
summary(sem2b, rsquare = T, standardized = T, fit.measures = T)
residuals(sem2b) ; residuals(sem2b, type = "cor")
modificationIndices(sem2b, standardized = F)

semPaths(sem2b, "std")  

#                     

sem2c_model <- 'FUCUS_PERCOV_TOTAL ~ ATemp_yearMn + Summ_Days_More_15 + Water_Temp_June
                MYTILUS ~ mn_yr_discharge + Summ_Days_More_15 + BARNACLES + Water_Temp_June + Water_Temp_Dec
                BARNACLES ~ mn_yr_discharge + FUCUS_PERCOV_TOTAL + Water_Temp_June + Summ_Days_More_15
                '

sem2c <- sem(sem2c_model, data = PerCov_FWT_NA, estimator = "MLM")

AIC(sem2c)
fitMeasures(sem2c, "pvalue")
summary(sem2c, rsquare = T, standardized = T, fit.measures = T)
residuals(sem2c) ; residuals(sem2c, type = "cor")
modificationIndices(sem2c, standardized = F)

semPaths(sem2c, "std")  

#                         
 
sem2d_model <- 'FUCUS_PERCOV_TOTAL ~ ATemp_yearMn + Summ_Days_More_15 
                MYTILUS ~ mn_yr_discharge + BARNACLES + Water_Temp_June + Summ_Days_More_15 
                BARNACLES ~ mn_yr_discharge + FUCUS_PERCOV_TOTAL 
                '

sem2d <- sem(sem2d_model, data = PerCov_FWT_NA, estimator = "MLM")

AIC(sem2d)
fitMeasures(sem2d, "pvalue")
summary(sem2d, rsquare = T, standardized = T, fit.measures = T)
residuals(sem2d) ; residuals(sem2d, type = "cor")
modificationIndices(sem2d, standardized = F)

semPaths(sem2d, "std")  

#  this is model 2d but with maximum summer temps instead of number of days above 15C for summer
 
sem2e_model <- 'FUCUS_PERCOV_TOTAL ~ ATemp_yearMn + ATemp_Summ_max 
                MYTILUS ~ mn_yr_discharge + BARNACLES + Water_Temp_June + ATemp_Summ_max 
                BARNACLES ~ mn_yr_discharge + FUCUS_PERCOV_TOTAL 
                '

sem2e <- sem(sem2e_model, data=PerCov_FWT_NA, estimator="MLM")

AIC(sem2e)
fitMeasures(sem2e, "pvalue")
summary(sem2e, rsquare=T, standardized=T, fit.measures=T)
residuals(sem2e) ; residuals(sem2e, type="cor")
modificationIndices(sem2e, standardized=F)

semPaths(sem2e, "std")  

###### BARNACLES & Fucus only model

semb_model <- 'FUCUS_SPORELINGS_PERCOV ~ BARNACLES
               BARNACLE_SPAT ~ FUCUS_PERCOV_TOTAL
               BARNACLES ~ FUCUS_PERCOV_TOTAL + BARNACLE_SPAT
              '
semb <- sem(semb_model, data=PerCov_FWT_NA, estimator="MLM")

AIC(semb)
fitMeasures(semb, "pvalue")
summary(semb, rsquare=T, standardized=T, fit.measures=T)
residuals(semb) ; residuals(semb, type="cor")
modificationIndices(semb, standardized=F)
parameterEstimates(semb)
inspect(semb, "sample") ; fitted(semb) 


semPaths(semb, "std")  

###### Incorporating BARNACLES & Fucus into the second model

semb2_model <- 'FUCUS_PERCOV_TOTAL ~ ATemp_yearMn + ATemp_Summ_max 
                MYTILUS ~ mn_yr_discharge + BARNACLES + Water_Temp_June + ATemp_Summ_max 
                BARNACLES ~ FUCUS_PERCOV_TOTAL + BARNACLE_SPAT + mn_yr_discharge 
                FUCUS_SPORELINGS_PERCOV ~ BARNACLES + ATemp_yearMn + ATemp_Summ_max 
                BARNACLE_SPAT ~ FUCUS_PERCOV_TOTAL + mn_yr_discharge + ATemp_yearMn + ATemp_Summ_max 
                '

semb2 <- sem(semb2_model, data=PerCov_FWT_NA, estimator="MLM")

AIC(semb2)
fitMeasures(semb2, "pvalue")
summary(semb2, rsquare=T, standardized=T, fit.measures=T)
residuals(semb2) ; residuals(semb2, type="cor")
modificationIndices(semb2, standardized=F)
parameterEstimates(semb2)
inspect(semb2, "sample") ; fitted(semb2) 


semPaths(semb2, "std")  

######              #################################################################

# vif = variance inflaction factor 
# tolerance = 1/vif

# As a rule of thumb, a variable whose VIF values is greater than 10 may merit
# further investigation. Tolerance, defined as 1/VIF, is used by many researchers 
# to check on the degree of collinearity. A tolerance value lower than 0.1 is 
# comparable to a VIF of 10. It means that the variable could be considered as 
# a linear combination of other independent variables. 

# Barnacle Spat
BSVIF <- vif(lm(BARNACLE_SPAT ~ FUCUS_PERCOV_TOTAL + MYTILUS + mn_yr_discharge + 
                 ATemp_YearlyMn + Summ_Days_More_15 + Spr_Days_Less_0, data=PerCov_FWT)) # VIF
BSTol <- 1/BSVIF  # this is the tolerance

# MYTILUS
MYVIF <- vif(lm(MYTILUS ~ ATemp_YearlyMn + mn_yr_discharge + Spr_Days_Less_0 + 
                Summ_Days_More_15 + BARNACLES, data=PerCov_FWT)) # VIF
MYTol <- 1/MYVIF  # this is the tolerance

# FUCUS_PERCOV_TOTAL
FTVIF <- vif(lm(FUCUS_PERCOV_TOTAL ~ ATemp_YearlyMn + Summ_Days_More_15 + mn_yr_discharge + 
                MYTILUS + FUCUS_SPORELINGS_PERCOV, data=PerCov_FWT)) # VIF
FTTol <- 1/FTVIF  # this is the tolerance

# BARNACLES
BAVIF <- vif(lm(BARNACLES ~ mn_yr_discharge + FUCUS_PERCOV_TOTAL + Spr_Days_Less_0 + Summ_Days_More_15 +
                ATemp_YearlyMn + BARNACLE_SPAT, data=PerCov_FWT)) # VIF
BATol <- 1/BAVIF  # this is the tolerance

# FUCUS_SPORELINGS_PERCOV
FSVIF <- vif(lm(FUCUS_SPORELINGS_PERCOV ~ BARNACLES + MYTILUS + Spr_Days_Less_0 + Summ_Days_More_15 + 
                ATemp_YearlyMn, data=PerCov_FWT)) # VIF
FSTol <- 1/FSVIF  # this is the tolerance

# air temps
ATVIF <- vif(lm(ATemp_YearlyMn ~ Spr_Days_Less_0 + Summ_Days_More_15, data=PerCov_FWT)) # VIF
ATTol <- 1/ATVIF  # this is the tolerance

STVIF <- vif(lm(ATemp_YearlyMn ~ ATemp_Spr_min + ATemp_Summ_max, data=PerCov_FWT)) # VIF
STTol <- 1/STVIF  # this is the tolerance



#####             #################################################################

sem3_model <- 'FUCUS_PERCOV_TOTAL_scaled ~ ATemp_YearlyMn + ATemp_Summ_max + L.SITKANA_scaled + LOTTIIDAE_scaled + ELACHISTA + PTEROSIPHONIA
               MYTILUS ~ mn_yr_discharge + BARNACLES + Water_Temp_June + ATemp_Summ_max 
               BARNACLES ~ FUCUS_PERCOV_TOTAL_scaled + BARNACLE_SPAT + mn_yr_discharge 
               FUCUS_SPORELINGS_PERCOV ~ BARNACLES + ATemp_YearlyMn + ATemp_Summ_max  
               BARNACLE_SPAT ~ FUCUS_PERCOV_TOTAL_scaled + mn_yr_discharge + ATemp_YearlyMn + ATemp_Summ_max 
               ELACHISTA ~ L.SITKANA_scaled
               PTEROSIPHONIA ~ MYTILUS 
               '

sem3 <- sem(sem3_model, data=PerCov_FWT_NA, estimator="MLM")

AIC(sem3)
fitMeasures(sem3, "pvalue")
summary(sem3, rsquare=T, standardized=T, fit.measures=T)
residuals(sem3) ; residuals(sem3, type="cor")
modificationIndices(sem3, standardized=F)
parameterEstimates(sem3)
inspect(sem3, "sample") ; fitted(sem3) 

semPaths(sem3, "std")  

#

sem3a_model <- 'FUCUS_PERCOV_TOTAL_scaled ~ ATemp_YearlyMn + ATemp_Summ_max + L.SITKANA_scaled + LOTTIIDAE_scaled + ELACHISTA
                MYTILUS ~ mn_yr_discharge + BARNACLES + Water_Temp_June + ATemp_Summ_max
                BARNACLES ~ FUCUS_PERCOV_TOTAL_scaled + mn_yr_discharge + PTEROSIPHONIA + L.SITKANA_scaled
                FUCUS_SPORELINGS_PERCOV ~ BARNACLES + L.SITKANA_scaled + PTEROSIPHONIA + LOTTIIDAE_scaled 
                BARNACLE_SPAT ~ FUCUS_PERCOV_TOTAL_scaled + mn_yr_discharge + ATemp_YearlyMn 
                ELACHISTA ~ L.SITKANA_scaled + PTEROSIPHONIA 
                PTEROSIPHONIA ~ MYTILUS + L.SITKANA_scaled + mn_yr_discharge
               '

sem3a <- sem(sem3a_model, data=PerCov_FWT_NA, estimator="MLM")

AIC(sem3a)
fitMeasures(sem3a, "pvalue")
summary(sem3a, rsquare=T, standardized=T, fit.measures=T)
residuals(sem3a) ; residuals(sem3a, type="cor")
modificationIndices(sem3a, standardized=F)
parameterEstimates(sem3a)
inspect(sem3a, "sample") ; fitted(sem3a) 

semPaths(sem3a, "std") 

# 

sem3b_model <- 'FUCUS_PERCOV_TOTAL_scaled ~ ATemp_YearlyMn + LOTTIIDAE_scaled + ELACHISTA
                MYTILUS ~ mn_yr_discharge + BARNACLES + Water_Temp_June + ATemp_Summ_max + PTEROSIPHONIA
                BARNACLES ~ FUCUS_PERCOV_TOTAL_scaled + mn_yr_discharge + PTEROSIPHONIA + L.SITKANA_scaled
                FUCUS_SPORELINGS_PERCOV ~ BARNACLES + PTEROSIPHONIA
                BARNACLE_SPAT ~ FUCUS_PERCOV_TOTAL_scaled + mn_yr_discharge + ATemp_YearlyMn 
                PTEROSIPHONIA ~ L.SITKANA_scaled 
               '

sem3b <- sem(sem3b_model, data=PerCov_FWT_NA, estimator="MLM")

AIC(sem3b)
fitMeasures(sem3b, "pvalue")
summary(sem3b, rsquare=T, standardized=T, fit.measures=T)
residuals(sem3b) ; residuals(sem3b, type="cor")
modificationIndices(sem3b, standardized=F)
parameterEstimates(sem3b)
inspect(sem3b, "sample") ; fitted(sem3b) 

semPaths(sem3b, "std") 

#

sem3c_model <- 'FUCUS_PERCOV_TOTAL_scaled ~ ATemp_YearlyMn + LOTTIIDAE_scaled
                MYTILUS ~ mn_yr_discharge + Water_Temp_June + ATemp_Summ_max + PTEROSIPHONIA + L.SITKANA_scaled
                BARNACLES ~ FUCUS_PERCOV_TOTAL_scaled + mn_yr_discharge + PTEROSIPHONIA + L.SITKANA_scaled + LOTTIIDAE_scaled
                FUCUS_SPORELINGS_PERCOV ~ BARNACLES + MYTILUS + ATemp_YearlyMn + LOTTIIDAE_scaled
                BARNACLE_SPAT ~ FUCUS_PERCOV_TOTAL_scaled + mn_yr_discharge + ATemp_YearlyMn + ATemp_Summ_max + Water_Temp_June
                PTEROSIPHONIA ~ L.SITKANA_scaled + ATemp_YearlyMn 
                LOTTIIDAE_scaled ~ MYTILUS
                '
#    
sem3c <- sem(sem3c_model, data=PerCov_FWT_NA, estimator="MLM")

AIC(sem3c)
fitMeasures(sem3c, "pvalue")
summary(sem3c, rsquare=T, standardized=T, fit.measures=T)
residuals(sem3c) ; residuals(sem3c, type="cor")
modificationIndices(sem3c, standardized=F)
parameterEstimates(sem3c)
inspect(sem3c, "sample") ; fitted(sem3c) 

semPaths(sem3c, "std") 


#  MYTILUS  ~          L.SITKANA_scaled 3.095     3.124
#   BARNACLE_SPAT  ~           Water_Temp_June 3.061     3.090

#

sem3d_model <- 'FUCUS_PERCOV_TOTAL_scaled ~ ATemp_YearlyMn + LOTTIIDAE_scaled
                MYTILUS ~ mn_yr_discharge + Water_Temp_June  + PTEROSIPHONIA + L.SITKANA_scaled 
                BARNACLES ~ FUCUS_PERCOV_TOTAL_scaled + mn_yr_discharge + PTEROSIPHONIA + L.SITKANA_scaled + LOTTIIDAE_scaled
                FUCUS_SPORELINGS_PERCOV ~ BARNACLES + MYTILUS + ATemp_YearlyMn + LOTTIIDAE_scaled
                BARNACLE_SPAT ~ FUCUS_PERCOV_TOTAL_scaled + mn_yr_discharge + ATemp_YearlyMn + Water_Temp_June
                PTEROSIPHONIA ~ L.SITKANA_scaled + ATemp_YearlyMn 
                LOTTIIDAE_scaled ~ MYTILUS
                '
#     
sem3d <- sem(sem3d_model, data=PerCov_FWT_NA, estimator="MLM")

AIC(sem3d)
fitMeasures(sem3d, "pvalue")
summary(sem3d, rsquare=T, standardized=T, fit.measures=T)
residuals(sem3d) ; residuals(sem3d, type="cor")
modificationIndices(sem3d, standardized=F)
parameterEstimates(sem3d)
inspect(sem3d, "sample") ; fitted(sem3d) 

semPaths(sem3d, "std") 

# 
 
sem3e_model <- 'FUCUS_PERCOV_TOTAL_scaled ~ ATemp_YearlyMn + LOTTIIDAE_scaled
                MYTILUS ~ mn_yr_discharge + Water_Temp_June  + PTEROSIPHONIA + L.SITKANA_scaled + LOTTIIDAE_scaled
                BARNACLES ~ FUCUS_PERCOV_TOTAL_scaled + mn_yr_discharge + PTEROSIPHONIA + L.SITKANA_scaled + LOTTIIDAE_scaled
                FUCUS_SPORELINGS_PERCOV ~ BARNACLES + MYTILUS + ATemp_YearlyMn + LOTTIIDAE_scaled
                BARNACLE_SPAT ~ FUCUS_PERCOV_TOTAL_scaled + mn_yr_discharge + ATemp_YearlyMn + Water_Temp_June
                PTEROSIPHONIA ~ L.SITKANA_scaled + ATemp_YearlyMn 
               
                '
#     LOTTIIDAE_scaled ~ MYTILUS
sem3e <- sem(sem3e_model, data=PerCov_FWT_NA, estimator="MLM")

AIC(sem3e)
fitMeasures(sem3e, "pvalue")
summary(sem3e, rsquare=T, standardized=T, fit.measures=T)
residuals(sem3e) ; residuals(sem3e, type="cor")
modificationIndices(sem3e, standardized=F)
parameterEstimates(sem3e)
inspect(sem3e, "sample") ; fitted(sem3e) 

semPaths(sem3e, "std") 




 

# from Yves Rosseel on lavaan forum

# Try this:
# 
# fit <- growth(sem3a_model,data=PerCov_FWT_NA,estimator="MLM", do.fit = FALSE)
# 
# This will NOT fit the data. But still process the model, data, etc...
# and give you back a lavaan object that can be inspected.
# 
# Next, try:
# 
# fitted(fit)$cov
# 
# and you will *see* the initial model-implied covariance matrix (Sigma).
# Look for covariances that are larger than the corresponding variances.
# 
# Finally, add better starting values to avoid this (make sure the implied
# variances are large enough).
# 
# Yves. 


























