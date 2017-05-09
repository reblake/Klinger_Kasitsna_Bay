############################################################
#####  Terrie Klinger's Kasitsna Bay Data              #####
#####  Invertebrate Data Cleaning Script               #####
#####  by Rachael E. Blake                             #####
#####  1 May 2017                                      #####
############################################################

library(plyr) ; library(readxl) ; library(tidyverse) ; library(reshape2) ; library(stringr)


# read in excel file 
# this function creates a list of data frames, one for each excel sheet 
read_excel_allsheets <- function(filename) {
                        sheets <- readxl::excel_sheets(filename)
                        x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet=X, skip=2))
                        names(x) <- sheets
                        x
}

X_sheets_n <- read_excel_allsheets("std_percent_cvr_by_year_to_2015_for_RB.xlsx")

# remove the "notes" sheet
X_sheets <- X_sheets_n[c(1,2,4:18)]  

# make each data frame long instead of wide
X_long <- lapply(X_sheets, function(x) as.data.frame(t(x)))


fix_data <- function(df) {
            # make column names
            names(df) <- as.character(unlist(df[1,]))
            df <- df[-1,]
            
            df1 <- df %>%
                   # row names to column 
                   tibble::rownames_to_column(var="standard_code") %>%  
                   # remove spaces from column names
                   dplyr::rename(abbr_code=`abbr code`, FUCUS_TOTAL=`FUCUS%TOTAL`,
                                 FUCUS_SPORELINGS=`FUCUS SPORELINGS%`, Ulva_Ent=`Ulva/Ent`,
                                 Pterosiphonia_poly=`Pterosiphonia/poly`, Clad_sericia=`Clad sericia`,
                                 Masto_pap=`Masto pap`, Barnacle_spat=`Barnacle spat`, 
                                 Palmaria_callophylloides=`Palmaria callophylloides`, 
                                 Crustose_coralline=`Crustose coralline`, 
                                 erect_coralline=`erect coralline`
                                 ) %>%
                   # make everything character
                   dplyr::mutate_if(is.factor, as.character)
            
            # replace NAs with 0, because Terrie says missing values represent 0s, NOT missing data
            df1[is.na(df1)] <- 0

            # return
            return(df1)
}


# apply fix_data function to list of data frames
X_clean <- lapply(X_long, function(x) fix_data(x))

# put all data frames into one giant one
PerCov_clean <- do.call("rbind", X_clean)
         
# make column for Year using data frame name
PerCov_clean$Year <- rep(names(X_clean), sapply(X_clean, nrow))

# make columns for block and treatment
PerCov_clean$Block <- str_sub(PerCov_clean$standard_code, -9,-8)
PerCov_clean$Treatment <- str_sub(PerCov_clean$standard_code, -4,-3)

# make columns numeric
PerCov_clean[,c(4:28)] <- lapply(PerCov_clean[,c(4:28)], function(x) as.numeric(x)) 

head(PerCov_clean)












