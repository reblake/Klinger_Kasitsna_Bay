################################################################
#####  Terrie Klinger's Kasitsna Bay Data                  #####
#####  All Invertebrate Data Cleaning Script               #####
#####  by Rachael E. Blake                                 #####
#####  1 May 2017                                          #####
################################################################

library(plyr) ; library(readxl) ; library(tidyverse) ; library(reshape2) ; library(stringr)


# read in excel file 
# this function creates a list of data frames, one for each excel sheet 
read_excel_allsheets <- function(filename) {
                        sheets <- readxl::excel_sheets(filename)
                        x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet=X, skip=1))
                        names(x) <- sheets
                        x
}

X_sheets_m <- read_excel_allsheets("ClearedPlots_1999-2017.xlsx")

# remove the "notes" sheet
X_sheets_yrs <- X_sheets_m[c(2:20)]  

# make each data frame long instead of wide
X_long_all <- lapply(X_sheets_yrs, function(x) as.data.frame(t(x)))

# split years 1999 and 2000 require more reformatting
X_long_1999 <- X_long_all$`1999`
X_long_2000 <- X_long_all$`2000`

# format 1999 dataframe
names(X_long_1999) <- as.character(unlist(X_long_1999[1,])) # make column names 
X_long_1999 <- X_long_1999[-c(1:2),]
X_long_1999 <- X_long_1999[ , !duplicated(colnames(X_long_1999))]  # removing duplicate columns called `NA`            

clean_1999 <- X_long_1999 %>%
              tibble::rownames_to_column() %>%  # making column of rownames
              dplyr::select(-rowname) %>%   # removing the rownames column
              dplyr::rename(TRIPLET = `NA`,
                            QUAD = `Species`,
                            FUCUS_PERCOV_TOTAL = `Fucus (%)`,
                            FUCUS_NUM_ADULTS = `Fucus (#)`,
                            FUCUS_SPORELINGS_PERCOV = `Fucus germ (%)`,
                            FUCUS_SPORELINGS_NUM = `Fucus germ (#)`,
                            FUCUS_kg_WW_scrape_99 = `Fucus wt wt (kg) scrape '99`,
                            ACROSIPHONIA = `Acrosiphonia (%)`,
                            CHAETOMORPHA = `Chaetomorpha (%)`,
                            CRYPTOSIPHONIA = `Cryptosiphonia (%)`,
                            ENDOCLADIA = `Endocladia (%)`,
                            GLOIOPELTIS = `Gloiopeltis (%)`,
                            HALOSACCION = `Halosaccion (%)`,
                            HILDENBRANDIA = `Hildenbrandia (%)`,
                            MASTOCARPUS = `Mastocarpus (%)`,
                            MELANOSIPHON = `Melanosiphon (%)`,
                            ODONTHALIA = `Odonthalia (%)`,
                            PALMARIA_C = `Palmaria c (%)`,
                            PETROCELIS = `Petrocelis (%)`,
                            PILAYELLA = `Pilayella (%)`,
                            PORPHYRA = `Porphyra (%)`,
                            PTEROSIPHONIA = `Pterosiphonia (%)`,
                            RALFSIA = `Ralfsia (%)`,
                            TOTAL_ALGAL_PER_COV = `Total Algal Coverage (%)`,
                            L.SITKANA = `Litt sitkana (#)`,
                            L.SCUTULATA = `Litt scutulata (#)`,
                 #           BARNACLES = `Barnacle adult (%)`,
                            BARNACLES_SPAT = `Barnacle spat (%)`,
                            MYTILUS = `Mytilus (%)`,
                            TOTAL_ANIMAL_PER_COV = `Total Animal Cover (%)`,
                  #          TOTAL_BARNACLE_PER_COV = `Totl Barnacle Cvr (%)`,
                            NUCELLA = `Nucella (#)`,
                            PAGURUS = `Pagurus (#)`,
                            SIPHONARIA = `Siphonaria (#)`,
                            ONCHIDELLA = `Onchidella (#)`,
                            LOTTIIDAE_JUV = `Lottiidae juv (#)`,
                            LOTTIA_P_BOREALIS = `Lottia pelta/borealis (#)`,
                            L_DIGIT_STRIG = `L digit/strig (#)`,
                            NEREIDAE = `Nereidae (#)`,
                            EMPLECTONEMA = `Emplectonema (#)`,
                            PARANEMERTES = `Paranemertes (#)`,
                            AMPHIPORUS = `Amphiphorus (#)`,
                            MARGARITES = `Margarites (#)`,
                            CUCUMARIA = `Cucumaria (#)`,
                            LEPTASTERIAS = `Leptasterias (#)`,
                            BARE_ROCK = `Bare Rock (%)`,
                            TOTAL_COVERAGE = `Total Coverage (%)`,
                            TOTAL_ALG_ANIM_PER_COV = `Total Alg+Anim Cvr (%)`
                            ) %>%
  
  
  

# format 2000 dataframe
names(X_long_2000) <- as.character(unlist(X_long_2000[1,])) # make column names 
X_long_2000 <- X_long_2000[-1,]
X_long_2000 <- X_long_2000[ , !duplicated(colnames(X_long_2000))]  # removing one column called `NA`
            
clean_2000 <- X_long_2000 %>%
              tibble::rownames_to_column() %>%  # making column of rownames
              dplyr::select(-`NA`,-rowname) %>%   # removing the other `NA` and rownames columns
              dplyr::rename(FUCUS_PERCOV_TOTAL = `FUCUS%TOTAL`,
                            FUCUS_NUM_ADULTS = `FUCUS#ADULTS`,
                            FUCUS_SPORELINGS_PERCOV = `FUCUS SPORELINGS%`,
                            FUCUS_SPORELINGS_NUM = `FUCUS SPORELINGS#`,
                            RALFSIA_HILD = `RALFSIA/HILD`,
                            CLAD_SERICEA = `CLAD SERICEA`,
                            MASTO_PAP = `MASTO PAP`,
                            L.SITKANA = `L SITKANA`,
                            L.SCUTULATA = `L SCUTULATA`,
                            BARNACLES_SPAT = `BARNACLES SPAT`,
                            MARGARITS_JUV = `MARGARITS JUV`,
                            BARE_ROCK = `BARE ROCK`,
                            BOULDER_COBBLE = `BOULDER-COBBLE`,
                            SAND_GRAVEL = `SAND-GRAVEL`) %>%  # rename columns that had special characters
              dplyr::filter(!(TREATMENT %in% c("avg, sd","avg,sd","AVG,STD"))) %>%
              dplyr::mutate_if(is.factor, as.character) %>%  # converts all columns to character
              dplyr::mutate_at(c(4:6,8:39), funs(as.numeric))  # converts select columns to numeric
              
 

# split years 2001 - 2017, which are formatted the same in the excel file






