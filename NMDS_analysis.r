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
source("AIrTemp_cleaning.r")

################################
# Trying regular NMDS analysis #
################################
###PDO
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
sp_percov <- PerCov_PDO %>% dplyr::select(FUCUS_TOTAL, Barnacles, Mytilus, Pterosiphonia_poly,
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

###Freshwater
# addin the freshwater data


       
