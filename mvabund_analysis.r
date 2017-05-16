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

# add in the PDO data
PerCov_PDO1 <- PerCov_clean %>%
               dplyr::left_join(pdo_ann, by="Year")   %>%
               dplyr::filter(Treatment == "01") %>%  # and subset to just the control treatment
               dplyr::mutate(PDO_Sign = ifelse(PDO_anul_mn>0, "A", "B"))
  
#####################################################
# Trying out the new-to-me R package called mvabund #
#####################################################

# make seperate dataframes to accomodate how this package works
sp_percov1 <- PerCov_PDO1 %>% dplyr::select(FUCUS_TOTAL, Barnacles, Mytilus, Pterosiphonia_poly,
                                            Odonthalia, Barnacle_spat, Endocladia, FUCUS_SPORELINGS,
                                            Clad_sericia, Masto_pap, Gloiopeltis, Elachista)

tr_percov1 <- PerCov_PDO1[,c(1:3,29:33)]

# make mvabund object 
vis_PerCov <- mvabund(sp_percov1)

# visualizations
boxplot(vis_PerCov, horizontal=TRUE, las=2, main="Abundance")
plot(vis_PerCov~as.factor(tr_percov1$Treatment)) # this just plots abundances of the 12 most abundant species
plot(vis_PerCov~as.factor(tr_percov1$Year))
plot(vis_PerCov~as.factor(tr_percov1$PDO_anul_mn))

# creates a mean-variance plot
meanvar.plot(vis_PerCov~as.factor(tr_percov1$Treatment),legend=TRUE, col=c(1,10,16,5), pch=20) 

# fit a multivariate linear model:
lm_percov <- manylm(vis_PerCov~tr_percov1$PDO_anul_mn, family="gaussian")
plot(lm_percov)

glm_percov <- manyglm(vis_PerCov~tr_percov1$Year*tr_percov1$PDO_Sign, family="negative_binomial")
plot(glm_percov)

# look at model results:
anova(glm_percov)  #Time elapsed: 0 hr 6 min 31 sec
anova(glm_percov, p.uni="adjusted")  #Time elapsed: 0 hr 3 min 20 sec
#summary(glm_percov)

# NOTE: significant effects of 
# Year on FOCUS_SPORELINGS, Ulva_Ent, Barnacle_spat, Mytilus
# PDO on FUCUS_TOTAL, Mytilus






