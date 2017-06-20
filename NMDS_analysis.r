############################################################
#####  Terrie Klinger's Kasitsna Bay Data              #####
#####  NMDS Community Analysis script                  #####
#####  by Rachael E. Blake                             #####
#####  10 May 2017                                     #####
############################################################

# load necessary libraries  
library(vegan) ; library(MASS)
library(dplyr) 
 

# source the data cleaning script to get the % cover data
source("Data_Cleaning_K_Bay.r")     # dataframe is called PerCov_clean

# PDO data
source("PDO_cleaning.r")      # dataframes are pdo_mon and pdo_ann

# Freshwater discharge data
source("Fresh_Discharge_cleaning.r")

# Air temperature data
source("AirTemp_cleaning.r")

################################
# Trying regular NMDS analysis #
################################
########
###PDO
########
# add in the PDO data
PerCov_PDO <- PerCov_clean %>%
              dplyr::left_join(pdo_ann, by="Year") %>%
              dplyr::filter(Treatment == "01") %>%
              dplyr::select(-standard_code, -plot, -abbr_code, -Treatment, -Block) %>%
              dplyr::group_by(Year) %>%
              dplyr::summarize_each(funs(mean)) %>%
              dplyr::ungroup() %>%
              dplyr::mutate(PDO_Sign = ifelse(PDO_anul_mn>0, "A", "B"))


# subset data into seperate dataframes
sp_percov <- PerCov_PDO %>% 
             dplyr::select(FUCUS_TOTAL, Barnacles, Mytilus, Pterosiphonia_poly,
                           Odonthalia, Barnacle_spat, Endocladia, FUCUS_SPORELINGS,
                           Clad_sericia, Masto_pap, Gloiopeltis, Elachista)

yr_percov <- PerCov_PDO$Year
pdo_percov <- PerCov_PDO$PDO_anul_mn
pdo_sign <- PerCov_PDO$PDO_Sign
pdo_treats <- PerCov_PDO[,c("Year", "PDO_anul_mn", "PDO_Sign")]
pdo_treats$Year <- as.factor(pdo_treats$Year)
pdo_treats$PDO_Sign <- as.factor(pdo_treats$PDO_Sign)

# do the analysis
percov_mds <- vegan::metaMDS(sp_percov, distance="bray", k=2, trymax=1000, autotransform=TRUE)

# plot it
#stressplot(percov_mds)
#plot(percov_mds$points, col=as.factor(pdo_sign), pch=16, cex=0.9) 
#f5 <- envfit(percov_mds, PerCov_PDO$PDO_anul_mn)
#plot(f5, col="black", cex=0.95)

# calculate PERMANOVA 
percov_perm <- vegan::adonis(sp_percov~PDO_anul_mn, pdo_treats, perm=1000, method="bray")

###############
###Freshwater
###############
# add in the freshwater data
PerCov_Fresh <- PerCov_PDO %>%
                dplyr::full_join(FWD_ann_mn, by="Year") %>%
                dplyr::rename(FWD_Sign = Sign) %>%
                dplyr::filter(Year != "2015") 

# subset to different dataframes
sp_percov2 <- PerCov_Fresh %>% 
              dplyr::select(FUCUS_TOTAL, Barnacles, Mytilus, Pterosiphonia_poly,
                            Odonthalia, Barnacle_spat, Endocladia, FUCUS_SPORELINGS,
                            Clad_sericia, Masto_pap, Gloiopeltis, Elachista)

fresh_treats <- PerCov_Fresh[,c("Year", "mean_yearly_discharge_m3s1", "mean_yearly_anomaly", "FWD_Sign")]
fresh_treats <- dplyr::filter(fresh_treats, !is.na(mean_yearly_discharge_m3s1))

# do the analysis
percov_mds2 <- vegan::metaMDS(sp_percov2, distance="bray", k=2, trymax=1000, autotransform=TRUE)

# calculate PERMANOVA 
percov_perm2 <- vegan::adonis(sp_percov2~mean_yearly_discharge_m3s1, fresh_treats, perm=1000, method="bray")

##############
### Air Temp
##############
# add in the air temp data

PerCov_ATmp <- PerCov_PDO %>%
               dplyr::full_join(year_a_temp, by="Year") %>%
               dplyr::rename(ATmp_Sign = Year_Sign,
                             ATemp_Year_Anom = Year_Anom) %>%
               dplyr::filter(Year != "2016",
                             Year > "2002")

# subset to different dataframes
sp_percov3 <- PerCov_ATmp %>% 
              dplyr::select(FUCUS_TOTAL, Barnacles, Mytilus, Pterosiphonia_poly,
                            Odonthalia, Barnacle_spat, Endocladia, FUCUS_SPORELINGS,
                            Clad_sericia, Masto_pap, Gloiopeltis, Elachista)

atemp_treats <- PerCov_ATmp[,c("Year", "ATemp_YearMn", "ATemp_Year_Anom", "ATmp_Sign")]

# do the analysis
percov_mds3 <- vegan::metaMDS(sp_percov3, distance="bray", k=2, trymax=1000, autotransform=TRUE)

# calculate PERMANOVA 
percov_perm3 <- vegan::adonis(sp_percov3~ATemp_YearMn, atemp_treats, perm=1000, method="bray")

#################
### all env. predictors
#################

PerCov_all <- PerCov_PDO %>%
              dplyr::full_join(FWD_ann_mn, by="Year") %>%
              dplyr::full_join(year_a_temp, by="Year") %>%
              dplyr::rename(ATmp_Sign = Year_Sign,
                            ATemp_Year_Anom = Year_Anom, 
                            mn_yr_discharge = mean_yearly_discharge_m3s1,
                            FWD_Sign = Sign) %>%
              dplyr::filter(Year != "2015")

# subset to different dataframes
sp_percov4 <- PerCov_all %>% 
              dplyr::select(FUCUS_TOTAL, Barnacles, Mytilus, Pterosiphonia_poly,
                            Odonthalia, Barnacle_spat, Endocladia, FUCUS_SPORELINGS,
                            Clad_sericia, Masto_pap, Gloiopeltis, Elachista)

all_treats <- PerCov_all[,c("Year", "PDO_anul_mn", "PDO_Sign", "mn_yr_discharge", 
                            "mean_yearly_anomaly", "FWD_Sign", "ATemp_YearMn", "ATemp_Year_Anom",
                            "ATmp_Sign")]

# do the analysis
percov_mds4 <- vegan::metaMDS(sp_percov4, distance="bray", k=2, trymax=1000, autotransform=TRUE)

# calculate PERMANOVA 
percov_perm4 <- vegan::adonis(sp_percov4~ATemp_YearMn+mn_yr_discharge,
                              all_treats, perm=1000, method="bray")

##################
### biological NMDS 
##################

# subset data into seperate dataframes
sp_percov5 <- PerCov_clean %>% 
              dplyr::filter(Treatment == "01",
                            !Year == "2015") %>%
              dplyr::select(-standard_code, -abbr_code, -Treatment, -Block) %>%
              dplyr::group_by(Year, plot) %>%
              dplyr::summarize_each(funs(mean)) %>%
              dplyr::ungroup() %>%
              dplyr::select(FUCUS_TOTAL, Barnacles, Mytilus, Pterosiphonia_poly,
                            Endocladia, 
                            Clad_sericia, Masto_pap, Gloiopeltis, Elachista)

all_treats2 <- PerCov_clean %>%
               dplyr::left_join(pdo_ann, by="Year") %>%
               dplyr::full_join(FWD_ann_mn, by="Year") %>%
               dplyr::full_join(year_a_temp, by="Year") %>%
               dplyr::filter(Treatment == "01",
                             !Year == "2015") %>%
               dplyr::rename(FWD_Sign = Sign,
                             ATmp_Sign = Year_Sign,
                             ATemp_Year_Anom = Year_Anom, 
                             mn_yr_discharge = mean_yearly_discharge_m3s1) %>%
               dplyr::mutate(PDO_Sign = ifelse(PDO_anul_mn>0, "A", "B")) %>%
               dplyr::select(Year, plot, PDO_anul_mn, PDO_Sign, mn_yr_discharge, mean_yearly_anomaly,
                             FWD_Sign, ATemp_YearMn, ATemp_Year_Anom, ATmp_Sign) %>%
               dplyr::arrange(Year)


# do the analysis
percov_mds5 <- vegan::metaMDS(sp_percov5, distance="bray", k=2, trymax=1000, autotransform=TRUE)

# calculate PERMANOVA 
percov_perm5 <- vegan::adonis(sp_percov5~ATemp_YearMn+mn_yr_discharge+Year, all_treats2, perm=1000, method="bray")





