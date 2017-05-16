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
sp_percov <- PerCov_PDO %>% dplyr::select(FUCUS_TOTAL, Barnacles, Mytilus, Pterosiphonia_poly,
                                          Odonthalia, Barnacle_spat, Endocladia, FUCUS_SPORELINGS,
                                          Clad_sericia, Masto_pap, Gloiopeltis, Elachista)
yr_percov <- PerCov_PDO[,"Year"]
pdo_percov <- PerCov_PDO[,"PDO_anul_mn"]

# do the analysis
percov_mds <- vegan::metaMDS(sp_percov, distance="bray", k=2, trymax=1000, autotransform=TRUE)







# making example dataset for posting question to StackOverflow
df_species <- sp_percov %>%
              dplyr::rename(Species_1 = FUCUS_TOTAL,
                            Species_2 = Barnacles,
                            Species_3 = Mytilus,
                            Species_4 = Pterosiphonia_poly,
                            Species_5 = Odonthalia, 
                            Species_6 = Barnacle_spat,
                            Species_7 = Endocladia,
                            Species_8 = FUCUS_SPORELINGS,
                            Species_9 = Clad_sericia,
                            Species_10 = Masto_pap,
                            Species_11 = Gloiopeltis,
                            Species_12 = Elachista)
     
my_mds <- vegan::metaMDS(df_species, distance="bray", k=2, trymax=1000, autotransform=TRUE)
          
