############################################################
#####  Terrie Klinger's Kasitsna Bay Data              #####
#####  NMDS Community Analysis script                  #####
#####  by Rachael E. Blake                             #####
#####  10 May 2017                                     #####
############################################################

# load necessary libraries  
library(vegan) ; library(MASS) ; library(tidyverse)
library(dplyr) ; library(here)
 

# source the data cleaning script to get the % cover data
# source(here("data_cleaning_scripts", "Data_Cleaning_K_Bay_ALL.r"))     # dataframe is called AllData_clean
AllData_clean <- read.csv(here("data_clean", "K_Bay_All_Sp_Yrs_Clean.csv"))

# PDO data
# source(here("data_cleaning_scripts", "PDO_cleaning.r"))      # dataframes are pdo_mon and pdo_ann
pdo_mon <- read.csv(here("data_clean", "PDO_monthly_clean.csv"))
pdo_ann <- read.csv(here("data_clean", "PDO_annual_clean.csv"))

# Freshwater discharge data
# source(here("data_cleaning_scripts", "Fresh_Discharge_cleaning.r"))
fwd_ann <- read_csv(here("data_clean", "FWD_annual_mn_clean.csv"))
fwd_mon <- read_csv(here("data_clean", "FWD_monthly_mn_clean.csv"))

# Air temperature data
ann_a_temp <- read.csv(here("data_clean", "air_temp_annual_clean.csv"))
year_a_temp <- read.csv(here("data_clean", "air_temp_year_clean.csv"))

# Water temperature data
# source(here("data_clean", "WTemp_June_clean.csv"))
WTemp_June <- read_csv(here("data_clean", "WTemp_June_clean.csv"))

################################
# Trying regular NMDS analysis #
################################
########
###PDO
########
# add in the PDO data
PerCov_PDO <- AllData_clean %>%
              dplyr::rename(Year = YEAR) %>%
              dplyr::mutate(across(Year, as.numeric)) %>% 
              dplyr::left_join(pdo_ann, by = "Year") %>%
              #dplyr::filter(TREATMENT == "CONTROL") %>%
              dplyr::select(-QUAD#, -FUCUS_SPORELINGS_NUM, -FUCUS_NUM_ADULTS, -TREATMENT, -TRIPLET
                            ) %>%
              dplyr::mutate_if(is.character, as.numeric) %>% # converts columns to numeric
              dplyr::group_by(Year) %>%
              dplyr::summarize(across(.cols = everything(), .fns = mean)) %>%
              dplyr::ungroup() %>%
              dplyr::mutate(PDO_Sign = ifelse(PDO_anul_mn > 0, "A", "B")) #%>%
              #dplyr::filter(Year != "2017")


# subset data into seperate dataframes
sp_percov <- PerCov_PDO %>% 
             dplyr::select(FUCUS_PERCOV_TOTAL, BARNACLES, MYTILUS, #PTEROSIPHONIA,
                           ODONTHALIA, BARNACLE_SPAT, ENDOCLADIA, FUCUS_SPORELINGS_PERCOV,
                           CLAD_SERICEA, MASTO_PAP, GLOIOPELTIS, ELACHISTA)

yr_percov <- PerCov_PDO$Year
pdo_percov <- PerCov_PDO$PDO_anul_mn
pdo_sign <- PerCov_PDO$PDO_Sign
pdo_treats <- PerCov_PDO[,c("Year", "PDO_anul_mn", "PDO_Sign")]
pdo_treats$Year <- as.factor(pdo_treats$Year)
pdo_treats$PDO_Sign <- as.factor(pdo_treats$PDO_Sign)

# do the analysis
# percov_mds <- vegan::metaMDS(sp_percov, distance = "bray", k = 2, trymax = 1000, autotransform = TRUE)
# saveRDS(percov_mds, file = "./data_clean/percov_mds.rds")

# plot it
#stressplot(percov_mds)
#plot(percov_mds$points, col=as.factor(pdo_sign), pch=16, cex=0.9) 
#f5 <- envfit(percov_mds, PerCov_PDO$PDO_anul_mn)
#plot(f5, col="black", cex=0.95)

# calculate PERMANOVA 
# percov_perm <- vegan::adonis2(sp_percov~PDO_anul_mn, pdo_treats, perm = 1000, method = "bray")
# saveRDS(percov_perm, file = "./data_clean/percov_perm.rds")

###############
###Freshwater
###############
# add in the freshwater data
PerCov_Fresh <- PerCov_PDO %>%
                dplyr::full_join(fwd_ann, by = "Year") %>%
                dplyr::rename(FWD_Sign = Sign) %>%
                dplyr::rename(year = Year) %>% 
                dplyr::filter(!(year %in% c("1999", "2000", "2001", "2022")))  # when we get complete FWD data, remove 2022

# subset to different dataframes
sp_percov2 <- PerCov_Fresh %>% 
              dplyr::select(FUCUS_PERCOV_TOTAL, BARNACLES, MYTILUS, #PTEROSIPHONIA,
                            ODONTHALIA, BARNACLE_SPAT, ENDOCLADIA, FUCUS_SPORELINGS_PERCOV,
                            CLAD_SERICEA, MASTO_PAP, GLOIOPELTIS, ELACHISTA) 

fresh_treats <- PerCov_Fresh[,c("year", "mean_yearly_discharge_m3d1", "mean_yearly_anomaly", "FWD_Sign")]
fresh_treats <- dplyr::filter(fresh_treats, !is.na(mean_yearly_discharge_m3d1))

# do the analysis
# percov_mds2 <- vegan::metaMDS(sp_percov2, distance = "bray", k = 2, trymax = 1000, autotransform = TRUE)
# saveRDS(percov_mds2, file = "./data_clean/percov_mds2.rds")

# calculate PERMANOVA 
# percov_perm2 <- vegan::adonis2(sp_percov2~mean_yearly_discharge_m3d1, fresh_treats, perm = 1000, method = "bray")
# saveRDS(percov_perm2, file = "./data_clean/percov_perm2.rds")

##############
### Air Temp
##############
# add in the air temp data

PerCov_ATmp <- PerCov_PDO %>%
               dplyr::rename(year = Year) %>% 
               dplyr::mutate(across(year, as.integer)) %>% 
               dplyr::full_join(year_a_temp, by = "year") %>%
               dplyr::rename(ATmp_Sign = year_Sign,
                             ATemp_Year_Anom = year_Anom) #%>%
              # dplyr::filter(#year != "2016",
              #               year != "2017"#,
              #               #year > "2002"
              #               )

# subset to different dataframes
sp_percov3 <- PerCov_ATmp %>% 
              dplyr::select(FUCUS_PERCOV_TOTAL, BARNACLES, MYTILUS, #PTEROSIPHONIA,
                            ODONTHALIA, BARNACLE_SPAT, ENDOCLADIA, FUCUS_SPORELINGS_PERCOV,
                            CLAD_SERICEA, MASTO_PAP, GLOIOPELTIS, ELACHISTA) %>% 
              filter(!is.na(BARNACLES))

atemp_treats <- PerCov_ATmp[,c("year", "ATemp_yearMn", "ATemp_Year_Anom", "ATmp_Sign")]

# do the analysis
# percov_mds3 <- vegan::metaMDS(sp_percov3, distance = "bray", k = 2, trymax = 1000, autotransform = TRUE)
# saveRDS(percov_mds3, file = "./data_clean/percov_mds3.rds")

# calculate PERMANOVA 
# percov_perm3 <- vegan::adonis2(sp_percov3~ATemp_yearMn, atemp_treats, perm = 1000, method = "bray")
# saveRDS(percov_perm3, file = "./data_clean/percov_perm3.rds")

#################
### all env. predictors
#################

PerCov_all <- PerCov_PDO %>%
              dplyr::full_join(fwd_ann, by = "Year") %>%
              dplyr::rename(year = Year) %>% 
              dplyr::mutate(year = as.integer(year)) %>% 
              dplyr::full_join(year_a_temp, by = "year") %>%
              dplyr::rename(Year = year) %>% 
              dplyr::mutate(Year = as.numeric(Year)) %>% 
              dplyr::full_join(WTemp_June, by = "Year") %>%
              dplyr::rename(ATmp_Sign = year_Sign,
                            ATemp_Year_Anom = year_Anom, 
                            mn_yr_discharge = mean_yearly_discharge_m3d1,
                            FWD_Sign = Sign) %>%
              filter(!(Year %in% c("1999", "2000", "2001")))  # filter out years we're not using and incomplete FWD years

# subset to different dataframes
sp_percov4 <- PerCov_all %>% 
              dplyr::select(FUCUS_PERCOV_TOTAL, BARNACLES, MYTILUS, #PTEROSIPHONIA,
                            ODONTHALIA, BARNACLE_SPAT, ENDOCLADIA, FUCUS_SPORELINGS_PERCOV,
                            CLAD_SERICEA, MASTO_PAP, GLOIOPELTIS, ELACHISTA) %>% 
              dplyr::filter(!is.na(BARNACLES))

all_treats <- PerCov_all[,c("Year", "PDO_anul_mn", "PDO_Sign", "mn_yr_discharge", 
                            "mean_yearly_anomaly", "FWD_Sign", "ATemp_yearMn", "ATemp_Year_Anom",
                            "ATmp_Sign")]

# do the analysis
# percov_mds4 <- vegan::metaMDS(sp_percov4, distance = "bray", k = 2, trymax = 1000, autotransform = TRUE)
# saveRDS(percov_mds4, file = "./data_clean/percov_mds4.rds")

# calculate PERMANOVA 
# percov_perm4 <- vegan::adonis2(sp_percov4 ~ ATemp_yearMn + mn_yr_discharge, all_treats, perm = 1000, method = "bray")
# saveRDS(percov_perm4, file = "./data_clean/percov_perm4.rds")

##################
### biological NMDS 
##################

# subset data into seperate dataframes
sp_percov5 <- AllData_clean %>% 
              dplyr::rename(Year = YEAR) %>%
              # dplyr::filter(!Year %in% c("2022")) %>%
              # dplyr::select(-TRIPLET, -TREATMENT, -FUCUS_NUM_ADULTS, -FUCUS_SPORELINGS_NUM) %>%
              dplyr::mutate(across(.cols = c(3:52), .fns = as.numeric)) %>% # converts select columns to numeric
              dplyr::group_by(Year, QUAD) %>%
              dplyr::summarize(across(.cols = everything(), .fns = mean)) %>%
              dplyr::ungroup() %>%
              dplyr::select(FUCUS_PERCOV_TOTAL, BARNACLES, MYTILUS, #PTEROSIPHONIA,
                            BARNACLE_SPAT, FUCUS_SPORELINGS_PERCOV, 
                            LOTTIIDAE, L.SITKANA)

all_treats2 <- AllData_clean %>%
               dplyr::mutate(YEAR = as.numeric(YEAR)) %>% 
               dplyr::rename(Year = YEAR) %>%
               dplyr::left_join(pdo_ann, by = "Year") %>%
               dplyr::full_join(fwd_ann, by = "Year") %>%
               dplyr::full_join(WTemp_June, by = "Year") %>%
               dplyr::rename(year = Year) %>% 
               dplyr::mutate(year = as.integer(year)) %>% 
               dplyr::full_join(year_a_temp, by = "year") %>%
               dplyr::filter(#TREATMENT == "CONTROL",
                             !year %in% c("1999","2000","2001")) %>%
               dplyr::rename(FWD_Sign = Sign,
                             ATmp_Sign = year_Sign,
                             ATemp_year_Anom = year_Anom, 
                             mn_yr_discharge = mean_yearly_discharge_m3d1) %>%
               dplyr::mutate(PDO_Sign = ifelse(PDO_anul_mn > 0, "A", "B")) %>%
               dplyr::select(year, QUAD, PDO_anul_mn, PDO_Sign, mn_yr_discharge, mean_yearly_anomaly,
                             FWD_Sign, ATemp_yearMn, ATemp_year_Anom, ATmp_Sign, Water_Temp_June) %>%
               dplyr::arrange(year)


# do the analysis
# percov_mds5 <- vegan::metaMDS(sp_percov5, distance = "bray", k = 2, trymax = 1000, autotransform = TRUE)
# saveRDS(percov_mds5, file = "./data_clean/percov_mds5.rds")

# calculate PERMANOVA 
# percov_perm5 <- vegan::adonis2(sp_percov5 ~ ATemp_yearMn * mn_yr_discharge * Water_Temp_June, all_treats2, perm = 1000, method = "bray")
# saveRDS(percov_perm5, file = "./data_clean/percov_perm5.rds")




