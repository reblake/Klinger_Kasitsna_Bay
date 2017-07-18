#################################################################
#####  Terrie Klinger's Kasitsna Bay Data                   #####
#####  Structural Equation Modeling script                  #####
#####  by Rachael E. Blake                                  #####
#####  22 June 2017                                         #####
#################################################################

# load packages
library(lavaan) ; library(dplyr) ; library(car) ; library(ggm) ; library(semPlot)  
library(semTools) ; library(psych)


# source the data cleaning script to get the % cover data
source("Data_Cleaning_K_Bay_ALL.r")     # dataframe is called AllData_clean

# Freshwater discharge data
source("Fresh_Discharge_cleaning.r")

# Air temperature data
source("AirTemp_cleaning.r")

# Water temperature data
source("WaterTemp_cleaning.r")   # WTemp_Yr, WTemp_June, WTemp_Dec



# add in the environmental data

spr <- spring_a_temp %>%
       dplyr::select(Year, ATemp_YearMn, Num_Day_Less_0, ATemp_Spr_min) %>% 
       dplyr::distinct()

sum <- summer_a_temp %>%
       dplyr::select(Year, ATemp_YearMn, Num_Day_More_15, ATemp_Summ_max) %>% 
       dplyr::distinct()


PerCov_FWT <- AllData_clean %>%
              dplyr::select(-MYELOPHYCUS, -HALOSACCION, -COLPOMENIA, 
                            -CRUSTOSE_CORALLINE, -CALLITHAMNION, -ERECT_CORALLINE, 
                            -ACROSIPHONIA, -NEORHODOMELA, -PALMARIA) %>%
              dplyr::rename(Year = YEAR) %>%
              dplyr::filter(TREATMENT == "CONTROL") %>%
              dplyr::mutate_at(vars(TRIPLET), funs(as.numeric)) %>%
              dplyr::arrange(Year, TRIPLET) %>%
              dplyr::full_join(FWD_ann_mn, by="Year") %>%  # join in the freshwater data
              dplyr::full_join(year_a_temp, by="Year") %>%  # join in the air temp data
              dplyr::rename(ATmp_Sign = Year_Sign,
                            ATemp_Year_Anom = Year_Anom, 
                            ATemp_YearlyMn = ATemp_YearMn,
                            mn_yr_discharge = mean_yearly_discharge_m3s1,
                            FWD_Sign = Sign) %>%
              dplyr::full_join(spr, by="Year") %>%  # join in the spring air temp data
              dplyr::rename(ATemp_SpringMn = ATemp_YearMn, 
                            Spr_Days_Less_0 = Num_Day_Less_0) %>%
              dplyr::full_join(sum, by="Year") %>%  # join in the summer air temp data
              dplyr::rename(ATemp_SummerMn = ATemp_YearMn, 
                            Summ_Days_More_15 = Num_Day_More_15) %>%
              dplyr::full_join(WTemp_Yr, by="Year") %>%  # join in the annual water temp data
              dplyr::full_join(WTemp_June, by="Year") %>%  # join in the June water temp data
              dplyr::full_join(WTemp_Dec, by="Year") %>%  # join in the December water temp data
              dplyr::arrange(Year)

# write.csv(PerCov_FWT, file = "K_Bay_All_Data_SEM.csv", row.names=FALSE)

#########
# Looking at correlations
pairs.panels(PerCov_FWT[,c(7,8,22,24,28,29,33,37,40,51,54,57:65)], smooth=F, density=T, ellipses=F, lm=T, 
             digits=3, scale=T)

pairs.panels(PerCov_FWT[,c(51,54,57:65)], smooth=F, density=T, ellipses=F, lm=T, 
             digits=3, scale=T)

pairs.panels(PerCov_FWT[,c(7,8,22,24,28,29,33,37,40)], smooth=F, density=T, ellipses=F, lm=T, 
             digits=3, scale=T)


#########
## SEM ##
#########

sem1_model <- 'FUCUS_PERCOV_TOTAL ~ ATemp_YearlyMn + BARNACLES + MYTILUS
               MYTILUS ~ ATemp_YearlyMn + mn_yr_discharge
               BARNACLES ~ mn_yr_discharge'

sem1 <- sem(sem1_model, data=PerCov_FWT, estimator="MLM")  

AIC(sem1)
fitMeasures(sem1, "pvalue")
summary(sem1, rsquare=T, standardized=T)
parameterEstimates(sem1)
residuals(sem1) ; residuals(sem1, type="cor")

semPaths(sem1, "std")                    
 
#

sem1a_model <- 'FUCUS_PERCOV_TOTAL ~ ATemp_YearlyMn + BARNACLES 
                MYTILUS ~ ATemp_YearlyMn + mn_yr_discharge + FUCUS_PERCOV_TOTAL
                BARNACLES ~ mn_yr_discharge'

sem1a <- sem(sem1a_model, data=PerCov_FWT)

AIC(sem1a)
fitMeasures(sem1a, "pvalue")
summary(sem1a, rsquare=T, standardized=T)
residuals(sem1a) ; residuals(sem1a, type="cor")

semPaths(sem1a, "std")

#

sem1b_model <- 'FUCUS_PERCOV_TOTAL ~ ATemp_YearlyMn + BARNACLES 
                MYTILUS ~ ATemp_YearlyMn + mn_yr_discharge + BARNACLES
                BARNACLES ~ mn_yr_discharge'

sem1b <- sem(sem1b_model, data=PerCov_FWT)

summary(sem1b, rsquare=T, standardized=T)

semPaths(sem1b, "std")

#

sem1c_model <- 'FUCUS_PERCOV_TOTAL ~ ATemp_YearlyMn + BARNACLES 
                MYTILUS ~ ATemp_YearlyMn + mn_yr_discharge + BARNACLES
                BARNACLES ~ mn_yr_discharge + ATemp_YearlyMn'

sem1c <- sem(sem1c_model, data=PerCov_FWT)

summary(sem1c, rsquare=T, standardized=T)

semPaths(sem1c, "std")

#

sem1d_model <- 'FUCUS_PERCOV_TOTAL ~ ATemp_YearlyMn + BARNACLES + mn_yr_discharge
                MYTILUS ~ ATemp_YearlyMn + mn_yr_discharge + BARNACLES
                BARNACLES ~ mn_yr_discharge'

sem1d <- sem(sem1d_model, data=PerCov_FWT)

summary(sem1d, rsquare=T, standardized=T)

semPaths(sem1d, "std")

#

sem1e_model <- 'FUCUS_PERCOV_TOTAL ~ ATemp_YearlyMn 
                MYTILUS ~ ATemp_YearlyMn + mn_yr_discharge + BARNACLES
                BARNACLES ~ mn_yr_discharge + FUCUS_PERCOV_TOTAL
                '

sem1e <- sem(sem1e_model, data=PerCov_FWT)

summary(sem1e, rsquare=T, standardized=T)
modificationIndices(sem1e, standardized=F)
parameterEstimates(sem1e)

semPaths(sem1e, "std")

#####

sem2_model <- 'FUCUS_PERCOV_TOTAL ~ ATemp_YearlyMn + Spr_Days_Less_0 + Summ_Days_More_15
               MYTILUS ~ ATemp_YearlyMn + mn_yr_discharge + Spr_Days_Less_0 + Summ_Days_More_15 + BARNACLES
               BARNACLES ~ mn_yr_discharge + Spr_Days_Less_0 + Summ_Days_More_15 + FUCUS_PERCOV_TOTAL
               '

sem2 <- sem(sem2_model, data=PerCov_FWT)

summary(sem2, rsquare=T, standardized=T, fit.measures=T)
modificationIndices(sem2, standardized=F)

semPaths(sem2, "std")  

#

sem2a_model <- 'FUCUS_PERCOV_TOTAL ~ ATemp_YearlyMn + Summ_Days_More_15 + mn_yr_discharge
                MYTILUS ~ ATemp_YearlyMn + mn_yr_discharge + Spr_Days_Less_0 + Summ_Days_More_15 + BARNACLES
                BARNACLES ~ mn_yr_discharge + FUCUS_PERCOV_TOTAL
                '

sem2a <- sem(sem2a_model, data=PerCov_FWT)

summary(sem2a, rsquare=T, standardized=T, fit.measures=T)
modificationIndices(sem2a, standardized=F)
parameterEstimates(sem2a)

semPaths(sem2a, "std")  

#

sem2b_model <- 'FUCUS_PERCOV_TOTAL ~ ATemp_YearlyMn + Summ_Days_More_15 + mn_yr_discharge
                MYTILUS ~ ATemp_YearlyMn + mn_yr_discharge + Spr_Days_Less_0 + Summ_Days_More_15 + BARNACLES
                BARNACLES ~ mn_yr_discharge + ATemp_YearlyMn + FUCUS_PERCOV_TOTAL
                '

sem2b <- sem(sem2b_model, data=PerCov_FWT)

summary(sem2b, rsquare=T, standardized=T, fit.measures=T)
modificationIndices(sem2b, standardized=F)

semPaths(sem2b, "std")  

#                     

sem2c_model <- 'FUCUS_PERCOV_TOTAL ~ ATemp_YearlyMn  
                MYTILUS ~ mn_yr_discharge + Spr_Days_Less_0 + Summ_Days_More_15 + BARNACLES
                BARNACLES ~ mn_yr_discharge + FUCUS_PERCOV_TOTAL
                '

sem2c <- sem(sem2c_model, data=PerCov_FWT)

summary(sem2c, rsquare=T, standardized=T, fit.measures=T)
modificationIndices(sem2c, standardized=F)

semPaths(sem2c, "std")  

#                          
 
sem2d_model <- 'FUCUS_PERCOV_TOTAL ~ ATemp_YearlyMn 
                MYTILUS ~ mn_yr_discharge + Summ_Days_More_15 + BARNACLES
                BARNACLES ~ mn_yr_discharge + FUCUS_PERCOV_TOTAL 
                '

sem2d <- sem(sem2d_model, data=PerCov_FWT)

summary(sem2d, rsquare=T, standardized=T, fit.measures=T)
modificationIndices(sem2d, standardized=F)

semPaths(sem2d, "std")  

#                     
 
sem2e_model <- 'FUCUS_PERCOV_TOTAL ~ ATemp_YearlyMn + mn_yr_discharge
                MYTILUS ~ mn_yr_discharge + Summ_Days_More_15 + BARNACLES
                BARNACLES ~ mn_yr_discharge + FUCUS_PERCOV_TOTAL
                '

sem2e <- sem(sem2e_model, data=PerCov_FWT)

summary(sem2e, rsquare=T, standardized=T, fit.measures=T)
modificationIndices(sem2e, standardized=F)

semPaths(sem2e, "std")  

#  
 
sem2f_model <- 'FUCUS_PERCOV_TOTAL ~ ATemp_YearlyMn + Summ_Days_More_15 
                MYTILUS ~ ATemp_YearlyMn + mn_yr_discharge + Spr_Days_Less_0 + Summ_Days_More_15 + BARNACLES
                BARNACLES ~ mn_yr_discharge + FUCUS_PERCOV_TOTAL 
                '

sem2f <- sem(sem2f_model, data=PerCov_FWT)

summary(sem2f, rsquare=T, standardized=T, fit.measures=T)
modificationIndices(sem2f, standardized=F)
parameterEstimates(sem2f)
inspect(sem2f, "sample") ; fitted(sem2f) 
residuals(sem2f) ; residuals(sem2f, type="cor")

semPaths(sem2f, "std")  

#  
 
sem2g_model <- 'FUCUS_PERCOV_TOTAL ~ Summ_Days_More_15 
                MYTILUS ~ mn_yr_discharge + Spr_Days_Less_0 + Summ_Days_More_15 + BARNACLES
                BARNACLES ~ mn_yr_discharge + FUCUS_PERCOV_TOTAL
                '

sem2g <- sem(sem2g_model, data=PerCov_FWT)

summary(sem2g, rsquare=T, standardized=T, fit.measures=T)
modificationIndices(sem2g, standardized=F)
parameterEstimates(sem2g)

semPaths(sem2g, "std")  

#  

sem2h_model <- 'FUCUS_PERCOV_TOTAL ~ Summ_Days_More_15 + mn_yr_discharge
                MYTILUS ~ mn_yr_discharge + Spr_Days_Less_0 + Summ_Days_More_15 + BARNACLES
                BARNACLES ~ mn_yr_discharge + FUCUS_PERCOV_TOTAL
                '

sem2h <- sem(sem2h_model, data=PerCov_FWT)

summary(sem2h, rsquare=T, standardized=T, fit.measures=T)
modificationIndices(sem2h, standardized=F)
parameterEstimates(sem2h)

semPaths(sem2h, "std")  

#

sem2i_model <- 'FUCUS_PERCOV_TOTAL ~ ATemp_YearlyMn + Summ_Days_More_15 + mn_yr_discharge + MYTILUS
                MYTILUS ~ ATemp_YearlyMn + mn_yr_discharge + Spr_Days_Less_0 + Summ_Days_More_15 + BARNACLES 
                BARNACLES ~ mn_yr_discharge + FUCUS_PERCOV_TOTAL + Spr_Days_Less_0 + Summ_Days_More_15 +  ATemp_YearlyMn 
                '

sem2i <- sem(sem2i_model, data=PerCov_FWT)  #, estimator="MLM"

AIC(sem2i)
summary(sem2i, rsquare=T, standardized=T, fit.measures=T)
modificationIndices(sem2i, standardized=F)
parameterEstimates(sem2i)
inspect(sem2i, "sample") ; fitted(sem2i) 
residuals(sem2i) ; residuals(sem2i, type="cor")

semPaths(sem2i, "std")  

# note: this model below uses different seasonal temp variables

try2i_model <- 'FUCUS_PERCOV_TOTAL ~ ATemp_YearlyMn + mn_yr_discharge + ATemp_Spr_min + ATemp_Summ_max 
                MYTILUS ~ ATemp_YearlyMn + mn_yr_discharge + ATemp_Summ_max + BARNACLES 
                BARNACLES ~ mn_yr_discharge + FUCUS_PERCOV_TOTAL + ATemp_YearlyMn + ATemp_Summ_max
                '

try2i <- sem(try2i_model, data=PerCov_FWT)

AIC(try2i)
fitMeasures(try2i, "pvalue")
summary(try2i, rsquare=T, standardized=T, fit.measures=T)
modificationIndices(try2i, standardized=F)
parameterEstimates(try2i)
inspect(try2i, "sample") ; fitted(try2i) 
residuals(try2i) ; residuals(try2i, type="cor")

semPaths(try2i, "std") 


##### BARNACLES & Fucus only model

semb_model <- 'FUCUS_SPORELINGS_PERCOV ~ BARNACLES 
               BARNACLE_SPAT ~ FUCUS_PERCOV_TOTAL 
               BARNACLES ~ FUCUS_PERCOV_TOTAL + BARNACLE_SPAT
              '
semb <- sem(semb_model, data=PerCov_FWT)

summary(semb, rsquare=T, standardized=T, fit.measures=T)
modificationIndices(semb, standardized=F)
parameterEstimates(semb)
inspect(semb, "sample") ; fitted(semb) 
residuals(semb) ; residuals(semb, type="cor")

semPaths(semb, "std")  

# Incorporating BARNACLES & Fucus into the second model

semb2_model <- 'FUCUS_PERCOV_TOTAL ~ ATemp_YearlyMn + Summ_Days_More_15 + mn_yr_discharge + MYTILUS + FUCUS_SPORELINGS_PERCOV
                MYTILUS ~ ATemp_YearlyMn + mn_yr_discharge + Spr_Days_Less_0 + Summ_Days_More_15 + BARNACLES
                BARNACLES ~ mn_yr_discharge + FUCUS_PERCOV_TOTAL + Spr_Days_Less_0 + Summ_Days_More_15 +  ATemp_YearlyMn + BARNACLE_SPAT
                FUCUS_SPORELINGS_PERCOV ~ BARNACLES + MYTILUS + Spr_Days_Less_0 + Summ_Days_More_15 + ATemp_YearlyMn
                BARNACLE_SPAT ~ FUCUS_PERCOV_TOTAL + MYTILUS + mn_yr_discharge + ATemp_YearlyMn + Summ_Days_More_15 + Spr_Days_Less_0
                '

semb2 <- sem(semb2_model, data=PerCov_FWT)

summary(semb2, rsquare=T, standardized=T, fit.measures=T)
modificationIndices(semb2, standardized=F)
parameterEstimates(semb2)
inspect(semb2, "sample") ; fitted(semb2) 
residuals(semb2) ; residuals(semb2, type="cor")

semPaths(semb2, "std")  

#

tryb2_model <- 'FUCUS_PERCOV_TOTAL ~ ATemp_YearlyMn + ATemp_Spr_min + ATemp_Summ_max + FUCUS_SPORELINGS_PERCOV
                MYTILUS ~ ATemp_YearlyMn + mn_yr_discharge + ATemp_Summ_max + BARNACLES
                BARNACLES ~ mn_yr_discharge + FUCUS_PERCOV_TOTAL + BARNACLE_SPAT
                FUCUS_SPORELINGS_PERCOV ~ BARNACLES + MYTILUS + ATemp_YearlyMn + ATemp_Summ_max 
                BARNACLE_SPAT ~ FUCUS_PERCOV_TOTAL + mn_yr_discharge + ATemp_YearlyMn + ATemp_Summ_max 
                '

tryb2 <- sem(tryb2_model, data=PerCov_FWT)

AIC(tryb2)
fitMeasures(tryb2, "pvalue")
summary(tryb2, rsquare=T, standardized=T, fit.measures=T)
modificationIndices(tryb2, standardized=F)
parameterEstimates(tryb2)
inspect(tryb2, "sample") ; fitted(tryb2) 
residuals(tryb2) ; residuals(tryb2, type="cor")

semPaths(tryb2, "std")  


#####

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



#####

sem3_model <- 'FUCUS_PERCOV_TOTAL ~ ATemp_YearlyMn + Summ_Days_More_15 + mn_yr_discharge + MYTILUS + ELACHISTA + PTEROSIPHONIA
               MYTILUS ~ ATemp_YearlyMn + mn_yr_discharge + Spr_Days_Less_0 + Summ_Days_More_15 + BARNACLES 
               BARNACLES ~ mn_yr_discharge + FUCUS_PERCOV_TOTAL + Spr_Days_Less_0 + Summ_Days_More_15 + ATemp_YearlyMn + ELACHISTA 
               PTEROSIPHONIA ~ MYTILUS 
               '
#ELACHISTA ~ 
sem3 <- sem(sem3_model, data=PerCov_FWT)

summary(sem3, rsquare=T, standardized=T, fit.measures=T)
modificationIndices(sem3, standardized=F)
parameterEstimates(sem3)
inspect(sem3, "sample") ; fitted(sem3) 
residuals(sem3) ; residuals(sem3, type="cor")

semPaths(sem3, "std")  


 






























