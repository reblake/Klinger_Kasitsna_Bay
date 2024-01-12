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




