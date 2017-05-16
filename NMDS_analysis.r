############################################################
#####  Terrie Klinger's Kasitsna Bay Data              #####
#####  NMDS Community Analysis script                  #####
#####  by Rachael E. Blake                             #####
#####  10 May 2017                                     #####
############################################################

# load necessary libraries  
library(vegan) ; library(tidyverse) 
 

# source the data cleaning script to get the % cover data
source("Data_Cleaning_K_Bay.r")     # dataframe is called PerCov_clean

# PDO data
source("PDO_cleaning.r")      # dataframes are pdo_mon and pdo_ann

################################
# Trying regular NMDS analysis #
################################

# add in the PDO data
PerCov_PDO <- PerCov_clean %>%
              dplyr::left_join(pdo_ann, by="Year") %>%
# and subset to just the control treatment
              dplyr::filter(Treatment == "01") %>%
              dplyr::select(-standard_code, -plot, -abbr_code, -Treatment, -Block) %>%
              dplyr::group_by(Year) %>%
              dplyr::summarize_each(funs(mean)) %>%
              dplyr::ungroup()


# subset data into seperate dataframes
sp_percov <- PerCov_PDO %>% select(FUCUS_TOTAL, Barnacles, Mytilus, Pterosiphonia_poly,
                                   Odonthalia, Barnacle_spat, Endocladia, FUCUS_SPORELINGS,
                                   Clad_sericia, Masto_pap, Gloiopeltis, Elachista)
yr_percov <- PerCov_PDO[,"Year"]
pdo_percov <- PerCov_PDO[,"PDO_anul_mn"]

# do the analysis
percov_mds <- metaMDS(sp_percov, distance="bray", k=2, trymax=1000, autotransform=FALSE)





