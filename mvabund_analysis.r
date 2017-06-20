############################################################
#####  Terrie Klinger's Kasitsna Bay Data              #####
#####  mvabund Community Analysis script               #####
#####  by Rachael E. Blake                             #####
#####  10 May 2017                                     #####
############################################################

# load necessary libraries  
library(mvabund) 
library(dplyr) 

# source the data cleaning script to get the % cover data
source("Data_Cleaning_K_Bay.r")     # dataframe is called PerCov_clean

# PDO data
source("PDO_cleaning.r")      # dataframes are pdo_mon and pdo_ann

# Freshwater discharge data
source("Fresh_Discharge_cleaning.r")

# Air temperature data
source("AirTemp_cleaning.r")


# add in the PDO data
PerCov_PDO1 <- PerCov_clean %>%
               dplyr::left_join(pdo_ann, by="Year") %>%
               dplyr::left_join(FWD_ann_mn, by="Year") %>%
               dplyr::left_join(year_a_temp, by="Year") %>%
               dplyr::filter(Treatment == "01") %>%  # and subset to just the control treatment
               dplyr::mutate(PDO_Sign = ifelse(PDO_anul_mn>0, "A", "B")) %>%
               dplyr::rename(ATmp_Sign = Year_Sign,
                             ATemp_Year_Anom = Year_Anom, 
                             mn_yr_discharge = mean_yearly_discharge_m3s1,
                             FWD_Sign = Sign) %>%
               dplyr::filter(Year != "2015")
  
#####################################################
# Trying out the new-to-me R package called mvabund #
#####################################################

# make seperate dataframes to accomodate how this package works
sp_percov1 <- PerCov_PDO1 %>% dplyr::select(FUCUS_TOTAL, Barnacles, Mytilus, Pterosiphonia_poly,
                                            Odonthalia, Barnacle_spat, Endocladia, FUCUS_SPORELINGS,
                                            Clad_sericia, Masto_pap, Gloiopeltis, Elachista)

tr_percov1 <- PerCov_PDO1 %>% dplyr::select(standard_code, plot, abbr_code, Year, Block, Treatment, 
                                            PDO_anul_mn, PDO_Sign, mn_yr_discharge, mean_yearly_anomaly,
                                            FWD_Sign, ATemp_YearMn, ATemp_Year_Anom, ATmp_Sign)

# make mvabund object 
vis_PerCov <- mvabund(sp_percov1)

# visualizations
boxplot(vis_PerCov, horizontal=TRUE, las=2, main="Abundance")
plot(vis_PerCov~as.factor(tr_percov1$Year))   # this just plots abundances of the 12 most abundant species
plot(vis_PerCov~as.factor(tr_percov1$PDO_Sign))

# creates a mean-variance plot
meanvar.plot(vis_PerCov~as.factor(tr_percov1$PDO_Sign),legend=TRUE, col=c(1,10,16,5), pch=20) 

# fit a multivariate linear model:
lm_percov <- manylm(vis_PerCov~tr_percov1$mn_yr_discharge*tr_percov1$ATemp_YearMn, family="gaussian")
plot(lm_percov)

glm_percov <- manyglm(vis_PerCov~tr_percov1$mn_yr_discharge*tr_percov1$ATemp_YearMn, family="negative_binomial")
plot(glm_percov)

# look at model results:
anova(glm_percov)  #Time elapsed: 0 hr 6 min 31 sec
anova(glm_percov, p.uni="adjusted")  #Time elapsed:  0 hr 2 min 32 sec
#summary(glm_percov)

# NOTE: significant effects of 
# freshwater and air temperature overall
# Univariate tests reveal...
# freshwater on barnacles and barnacle spat (p = 0.02), mytilus (p = 0.001)
# air temperature on mytilus (p = 0.001), and fucus (p = 0.024)






