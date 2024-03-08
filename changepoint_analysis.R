#################################################################
#####  Terrie Klinger's Kasitsna Bay Data                   #####
#####  Changepoint analysis script                          #####
#####  by Rachael E. Blake                                  #####
#####  15 December 2023                                     #####
#####                                                       #####
#################################################################

# load packages
library(tidyverse) ; library(bcp)

# load the dataset
PerCov_FWT <- read_csv("K_Bay_All_Data_SEM.csv")

# scale some variables
cp_dat <- PerCov_FWT %>% 
          dplyr::select(-WTemp_Dec_Lag, -LAG_WTemp_June, -FWD_Sign) %>%
          dplyr::group_by() %>%
          dplyr::mutate(L.SITKANA_scaled = L.SITKANA/10,  # scaling variables
                        L.SCUTULATA_scaled = L.SCUTULATA/10,
                        LOTTIIDAE_scaled = LOTTIIDAE/10,
                        FUCUS_PERCOV_TOTAL_scaled = FUCUS_PERCOV_TOTAL/10,
                        mn_yr_discharge_scaled = mn_yr_discharge/1000) %>%
          dplyr::ungroup() %>% 
          dplyr::group_by(Year) %>% 
          summarize(across(2:66, mean)) %>% 
          dplyr::ungroup()


##### Biological variables change point analysis

# univariate changepoint analysis
set.seed(5)
cp_mod_1 <- bcp(cp_dat$L.SCUTULATA_scaled)
# plot the univariate model
plot(cp_mod_1, main = "L. scutulata")
year_prob_1 <- cbind(cp_dat$Year, cp_mod_1$posterior.prob)
plot(year_prob_1, type = "l", xlab = "Year", ylab = "Posterior Probability", main = "L. scutulata")
# look at probabilities
year_prob_1a <- as.data.frame(year_prob_1) %>% rename(year = V1, prob = V2) %>% 
                arrange(desc(prob))
head(year_prob_1a)

# univariate changepoint analysis
set.seed(5)
cp_mod_2 <- bcp(cp_dat$L.SITKANA_scaled)
# plot the univariate model
plot(cp_mod_2, main = "L. sitkana")
year_prob_2 <- cbind(cp_dat$Year, cp_mod_2$posterior.prob)
plot(year_prob_2, type = "l", xlab = "Year", ylab = "Posterior Probability", main = "L. sitkana")
# look at probabilities
year_prob_2a <- as.data.frame(year_prob_2) %>% rename(year = V1, prob = V2) %>% 
                arrange(desc(prob))
head(year_prob_2a)




# select vars for multivariate model
cp_mv_1 <- as.matrix(cp_dat %>% 
                     dplyr::select(BARNACLES, MYTILUS, FUCUS_PERCOV_TOTAL_scaled, PTERO_POLY_SUM))
# do the changepoint analysis
set.seed(5)
cp_mod_mv <- bcp(cp_mv_1)
# plot the multivariate model
plot(cp_mod_mv, separated = TRUE, main = "Barnacles Mytilus Fucus Ptero")
year_prob_mv <- cbind(cp_dat$Year, cp_mod_mv$posterior.prob)
plot(year_prob_mv, type = "l", xlab = "Year", ylab = "Posterior Probability", 
     main = "Barnacles Mytilus Fucus Ptero")
# look at probabilities
year_prob_mva <- as.data.frame(year_prob_mv) %>% rename(year = V1, prob = V2) %>% 
                 arrange(desc(prob))
head(year_prob_mva)


# select vars for multivariate model
cp_mv_2 <- as.matrix(cp_dat %>% 
                     dplyr::select(BARNACLES, MYTILUS, FUCUS_PERCOV_TOTAL_scaled, PTERO_POLY_SUM, 
                                   L.SITKANA_scaled, L.SCUTULATA_scaled))
# do the changepoint analysis
set.seed(5)
cp_mod_mv2 <- bcp(cp_mv_2)
# plot the multivariate model
plot(cp_mod_mv2, separated = TRUE, main = "Barnacles Mytilus Fucus Ptero Snails")
year_prob_mv2 <- cbind(cp_dat$Year, cp_mod_mv2$posterior.prob)
plot(year_prob_mv2, type = "l", xlab = "Year", ylab = "Posterior Probability", 
     main = "Barnacles Mytilus Fucus Ptero Snails")
# look at probabilities
year_prob_mva2 <- as.data.frame(year_prob_mv2) %>% rename(year = V1, prob = V2) %>% 
                  arrange(desc(prob))
head(year_prob_mva2)


##### Physical variables change point analysis

# select vars for multivariate model
cp_mv_3 <- as.matrix(cp_dat %>% 
                     dplyr::select(ATemp_yearMn, Water_Temp_June, mn_yr_discharge_scaled))
# do the changepoint analysis
set.seed(5)
cp_mod_mv3 <- bcp(cp_mv_3)
# plot the multivariate model
plot(cp_mod_mv3, separated = TRUE, main = "Air & water temp, freshwater discharge")
year_prob_mv3 <- cbind(cp_dat$Year, cp_mod_mv3$posterior.prob)
plot(year_prob_mv3, type = "l", xlab = "Year", ylab = "Posterior Probability", 
     main = "Air & water temp, freshwater discharge")
# look at probabilities
year_prob_mva3 <- as.data.frame(year_prob_mv3) %>% rename(year = V1, prob = V2) %>% 
                  arrange(desc(prob))
head(year_prob_mva3)


# univariate changepoint analysis
set.seed(5)
cp_mod_u3 <- bcp(cp_dat$ATemp_yearMn)
# plot the univariate model
plot(cp_mod_u3, main = "Mean Annual Air Temp")
year_prob_u3 <- cbind(cp_dat$Year, cp_mod_u3$posterior.prob)
plot(year_prob_u3, type = "l", xlab = "Year", ylab = "Posterior Probability", main = "Mean Annual Air Temp")
# look at probabilities
year_prob_u3a <- as.data.frame(year_prob_u3) %>% rename(year = V1, prob = V2) %>% 
                arrange(desc(prob))
head(year_prob_u3a)


# univariate changepoint analysis
# load winter air temp dataset
Winter_ATemp <- read_csv("./data_clean/air_temp_winter_clean.csv")
                
#
watmp <- Winter_ATemp %>% select(year, ATemp_yearMn) %>% unique()

set.seed(5)
cp_mod_u4 <- bcp(watmp$ATemp_yearMn)
# plot the univariate model
plot(cp_mod_u4, main = "Winter Mean Annual Air Temp")
year_prob_u4 <- cbind(cp_dat$Year, cp_mod_u4$posterior.prob)
plot(year_prob_u4, type = "l", xlab = "Year", ylab = "Posterior Probability", main = "Winter Mean Annual Air Temp")
# look at probabilities
year_prob_u4a <- as.data.frame(year_prob_u4) %>% rename(year = V1, prob = V2) %>% 
                arrange(desc(prob))
head(year_prob_u4a)

#
JanFeb_Atemp <- read_csv("./data_clean/air_temp_JanFeb_clean.csv") %>% 
                select(year, ATemp_yearMn) %>% unique()

set.seed(5)
cp_mod_u5 <- bcp(JanFeb_Atemp$ATemp_yearMn)
# plot the univariate model
plot(cp_mod_u5, main = "JanFeb Mean Annual Air Temp")
year_prob_u5 <- cbind(cp_dat$Year, cp_mod_u5$posterior.prob)
plot(year_prob_u5, type = "l", xlab = "Year", ylab = "Posterior Probability", main = "JanFeb Mean Annual Air Temp")
# look at probabilities
year_prob_u5a <- as.data.frame(year_prob_u5) %>% rename(year = V1, prob = V2) %>% 
                arrange(desc(prob))
head(year_prob_u5a)

#
water_temp_Dec <- read_csv("./data_clean/WTemp_Dec_clean.csv")

set.seed(5)
cp_mod_u6 <- bcp(water_temp_Dec$Water_Temp_Dec)
# plot the univariate model
plot(cp_mod_u6, main = "Dec Annual Water Temp")
year_prob_u6 <- cbind(cp_dat$Year, cp_mod_u6$posterior.prob)
plot(year_prob_u6, type = "l", xlab = "Year", ylab = "Posterior Probability", main = "Dec Annual Water Temp")
# look at probabilities
year_prob_u6a <- as.data.frame(year_prob_u6) %>% rename(year = V1, prob = V2) %>% 
                arrange(desc(prob))
head(year_prob_u6a)


#
wtemp_j <- cp_dat %>% dplyr::select(Water_Temp_June)

set.seed(5)
cp_mod_u8 <- bcp(wtemp_j$Water_Temp_June)
# plot the univariate model
plot(cp_mod_u8, main = "June Water Temp")
year_prob_u8 <- cbind(cp_dat$Year, cp_mod_u8$posterior.prob)
plot(year_prob_u8, type = "l", xlab = "Year", ylab = "Posterior Probability", main = "June Water Temp")
# look at probabilities
year_prob_u8a <- as.data.frame(year_prob_u8) %>% rename(year = V1, prob = V2) %>% 
                arrange(desc(prob))
head(year_prob_u8a)


#####################################################################
###########Freshwater Discharge##############
cp_mv_4 <- cp_dat %>% dplyr::select(Year, mn_yr_discharge_scaled)
                     
set.seed(5)
cp_mod_u9 <- bcp(cp_mv_4$mn_yr_discharge_scaled)
# plot the univariate model
plot(cp_mod_u9, main = "Freshwater Discharge")
year_prob_u9 <- cbind(cp_dat$Year, cp_mod_u9$posterior.prob)
plot(year_prob_u9, type = "l", xlab = "Year", ylab = "Posterior Probability", main = "Freshwater Discharge")
# look at probabilities
year_prob_u9a <- as.data.frame(year_prob_u9) %>% rename(year = V1, prob = V2) %>% 
                arrange(desc(prob))
head(year_prob_u9a)




