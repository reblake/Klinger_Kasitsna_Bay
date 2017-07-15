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
              dplyr::select(-rowname, -`Fucus wt wt (kg) scrape '99`, -`Ralfsia (%)`, -`Hildenbrandia (%)`,
                            -`Petrocelis (%)`, -`Total Algal Coverage (%)`, -`Barnacle adult (%)`,
                            -`Total Animal Cover (%)`, -`Bare Rock (%)`, -`Total Coverage (%)`,
                            -`Total Alg+Anim Cvr (%)`) %>%   # removing the columns
              dplyr::rename(QUAD = `Species`,
                            FUCUS_PERCOV_TOTAL = `Fucus (%)`,
                            FUCUS_NUM_ADULTS = `Fucus (#)`,
                            FUCUS_SPORELINGS_PERCOV = `Fucus germ (%)`,
                            FUCUS_SPORELINGS_NUM = `Fucus germ (#)`,
                            ACROSIPHONIA = `Acrosiphonia (%)`,
                            CHAETOMORPHA = `Chaetomorpha (%)`,
                            CRYPTOSIPHONIA = `Cryptosiphonia (%)`,
                            ENDOCLADIA = `Endocladia (%)`,
                            GLOIOPELTIS = `Gloiopeltis (%)`,
                            HALOSACCION = `Halosaccion (%)`,
                            MASTO_PAP = `Mastocarpus (%)`,
                            MELANOSIPHON = `Melanosiphon (%)`, 
                            ODONTHALIA = `Odonthalia (%)`,
                            PALMARIA = `Palmaria c (%)`,  
                            PILAYELLA = `Pilayella (%)`,
                            PORPHYRA = `Porphyra (%)`,
                            PTEROSIPHONIA = `Pterosiphonia (%)`,
                            L.SITKANA = `Litt sitkana (#)`,
                            L.SCUTULATA = `Litt scutulata (#)`,
                            BARNACLES =  `Totl Barnacle Cvr (%)`,
                            BARNACLE_SPAT = `Barnacle spat (%)`,
                            MYTILUS = `Mytilus (%)`,
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
                            LEPTASTERIAS = `Leptasterias (#)`
                            )

names(clean_1999) <- make.names(names(clean_1999))  # will turn names into accepted formats
  
clean_1999n <- clean_1999 %>%
               dplyr::rename(TRIPLET = `NA.`) %>%
               dplyr::mutate(YEAR = "1999",
                             TREATMENT = ifelse(QUAD %in% c(102,105,108,116,120,122,170,505,532,
                                                            537,542,546), "CONTROL",
                                         ifelse(QUAD %in% c(101,107,113,117,118,123,504,523,531,
                                                            538,541,545), "SCRAPE00",
                                         ifelse(QUAD %in% c(103,106,114,115,119,121,503,522,533,
                                                            536,540,547), "SCRAPE99", "")))) %>%
               setNames(toupper(names(.))) %>% # make column names all upper case
               dplyr::mutate_if(is.factor, as.character) %>%  # converts all columns to character
               dplyr::mutate_at(c(3:5,7:38), funs(as.numeric)) %>% # converts select columns to numeric
               dplyr::mutate(LOTTIIDAE = LOTTIIDAE_JUV + LOTTIA_P_BOREALIS) %>%
               dplyr::select(-LOTTIIDAE_JUV, -LOTTIA_P_BOREALIS) %>%
               tidyr::gather(-TRIPLET, -QUAD, -TREATMENT, -YEAR, key=TAXA, value=PER_COV_OR_COUNT)
# replace NAs with 0, because Terrie says missing values represent 0s for some variables
clean_1999n[is.na(clean_1999n)] <- 0
  

# format 2000 dataframe
names(X_long_2000) <- as.character(unlist(X_long_2000[1,])) # make column names 
X_long_2000 <- X_long_2000[-1,]
X_long_2000 <- X_long_2000[ , !duplicated(colnames(X_long_2000))]  # removing one column called `NA`
X_long_2000 <- X_long_2000[,-22]  # removing the other `NA`` column
            
clean_2000 <- X_long_2000 %>%
              tibble::rownames_to_column() %>%  # making column of rownames
              dplyr::select(-rowname, -`RALFSIA/HILD`, -PETROCELIS, -SPIRORBIDAE,
                            -KATHRINA, -`BARE ROCK`, -ROCK, -`BOULDER-COBBLE`,
                            -`SAND-GRAVEL`) %>%   # removing columns
              dplyr::rename(FUCUS_PERCOV_TOTAL = `FUCUS%TOTAL`,
                            FUCUS_NUM_ADULTS = `FUCUS#ADULTS`,
                            FUCUS_SPORELINGS_PERCOV = `FUCUS SPORELINGS%`,
                            FUCUS_SPORELINGS_NUM = `FUCUS SPORELINGS#`,
                            CLAD_SERICEA = `CLAD SERICEA`,
                            MASTO_PAP = `MASTO PAP`,
                            L.SITKANA = `L SITKANA`,
                            L.SCUTULATA = `L SCUTULATA`,
                            BARNACLE_SPAT = `BARNACLES SPAT`,
                            MARGARITES = `MARGARITS JUV`        
                            ) %>%  # rename columns that had special characters
              setNames(toupper(names(.))) %>% # make column names all upper case
              dplyr::filter(!(TREATMENT %in% c("avg, sd","avg,sd","AVG,STD"))) %>%
              dplyr::mutate_if(is.factor, as.character) %>%  # converts all columns to character
              dplyr::mutate_at(c(4:6,8:35), funs(as.numeric)) %>% # converts select columns to numeric
              dplyr::mutate(YEAR = "2000") %>% 
              tidyr::gather(-TRIPLET, -QUAD, -TREATMENT, -YEAR, key=TAXA, value=PER_COV_OR_COUNT)
              
# replace NAs with 0, because Terrie says missing values represent 0s for many columns
clean_2000[is.na(clean_2000)] <- 0



## Bind 1999 and 2000 together into one dataframe
clean_99_00 <- full_join(clean_1999n, clean_2000, by=c("TRIPLET","QUAD","TREATMENT","YEAR",
                                                       "TAXA","PER_COV_OR_COUNT"))
 

# split years 2001 - 2017, which are formatted the same in the excel file

# fix_data2 <- function(df) {
#              # make column names
#              names(df) <- as.character(unlist(df[1,]))
#              df <- df[-1,]
#              # remove entirely blank column
#              df <- df[!is.na(names(df))]
#              
#              df1 <- df %>%
#                     # row names to column 
#                     tibble::rownames_to_column(var="TRIPLET") %>%  
#                     dplyr::select(-`Ralfsia/Hild`, -`RALFSIA/HILD`,
#                                   -`Antho artemesia`, -`ANTHOPL ARTEMESIA`,
#                                   -`Petrocelis`, -`Petrocelia`, -`PETROCELIS`, 
#                                   -`Spirorbidae`, -`SPIRORBIDAE`, -`Katharina`, -`KATHRINA`,
#                                   -`BARE ROCK`, -`ROCK`, -`BOULDER-COBBLE`, -`SAND-GRAVEL`) %>%
#                     dplyr::rename(FUCUS_PERCOV_TOTAL = `FUCUS%TOTAL`, # remove spaces/characters from column names
#                                   FUCUS_NUM_ADULTS = `FUCUS#ADULTS`,
#                                   FUCUS_SPORELINGS_PERCOV=`FUCUS SPORELINGS%`,
#                                   FUCUS_SPORELINGS_NUM = `FUCUS SPORELINGS#`,
#                                   CLAD_SERICEA = `Clad sericia`,
#                                   L.SITKANA = `L sitkana`,
#                                   L.SCUTULATA = `L scutulata`,
#                                   MASTO_PAP = `Masto pap`,
#                                   ERECT_CORALLINE = `erect coralline`,
#                                   BARNACLES_SPAT = `Barnacle spat` #,
#                                   #CRYPTONATICA = `Cryptonatica[Acrosiphonia]`,
#                                   #IRRIDESCENT_SNAIL_HOMALOPOMA = `irridescent snail (Homalopoma?)`,
#                                   #ENCRUSTING_CORRALINE = `encrusting coralline`
#                                   ) %>%
#                     setNames(toupper(names(.))) %>%  # make column names all upper case
#                     dplyr::mutate_if(is.factor, as.character) %>%  # make everything character
#                      # split off extra numerals from "TRIPLET" column
#                     dplyr::mutate(TRIPLET = ifelse(!(TRIPLET %in% c(1:12)), 
#                                                    sapply(strsplit(as.character(TRIPLET), split="__") , function(x) x[1]),
#                                                    TRIPLET)
#                                   ) %>%
#                     dplyr::filter(TRIPLET %in% c(1:12))
#              
#              # replace NAs with 0, because Terrie says missing values represent 0s for certain categories
#              df1[is.na(df1)] <- 0 
#              
#              # return
#              return(df1)
# }


fix_data3 <- function(df) {
             # make column names
             names(df) <- as.character(unlist(df[1,]))
             df <- df[-1,]
             # remove entirely blank column
             df <- df[!is.na(names(df))]
             
             df1 <- df %>%
                    tibble::rownames_to_column(var="TRIPLET") %>%   # row names to column 
                    setNames(toupper(names(.))) %>%  # make column names all upper case
                    dplyr::mutate_if(is.factor, as.character) %>%  # converts all columns to character
                    tidyr::gather(-TRIPLET, -QUAD, -TREATMENT, key=TAXA, value=PER_COV_OR_COUNT) %>%
                    # split off extra numerals from "TRIPLET" column
                    dplyr::mutate(TRIPLET = ifelse(!(TRIPLET %in% c(1:12)), 
                                                   sapply(strsplit(as.character(TRIPLET), split="__") , function(x) x[1]),
                                                   TRIPLET),
                                  TAXA = str_replace_all(TAXA, "#ADULTS", " NUM_ADULTS"),
                                  TAXA = str_replace_all(TAXA, "%TOTAL", " PERCOV_TOTAL"),
                                  TAXA = str_replace_all(TAXA, "SPORELINGS#", "SPORELINGS_NUM"),
                                  TAXA = str_replace_all(TAXA, "SPORELINGS%", "SPORELINGS_PERCOV"),
                                  TAXA = str_replace_all(TAXA, "BARNACLES SPAT", "BARNACLE SPAT"),
                                  TAXA = str_replace_all(TAXA, "SERICIA", "SERICEA"),
                                  TAXA = case_when(TAXA == "IRRIDESCENT SNAIL (HOMALOPOMA?)" ~ "IRRIDESCENT SNAIL HOMALOPOMA",
                                                   TAXA == "LOTTIDS" ~ "LOTTIIDAE",
                                                   TAXA == "AMPHIPOROUS" ~ "AMPHIPORUS",
                                                   TAXA == "CORALLINE CRUST" ~ "CRUSTOSE CORALLINE",
                                                   TAXA == "CALLITHAM PIKEANUM" ~ "CALLITHAMNION",
                                                   TAXA == "CRYPTONATICA[ACROSIPHONIA]" ~ "ACROSIPHONIA",
                                                   TAXA == "MARGARITES MARG" ~ "MARGARITES",
                                                   TAXA == "PALMARIA CALLOPHYLLOIDES" ~ "PALMARIA",
                                                   TRUE ~ TAXA),
                                  TAXA = str_replace_all(TAXA,"[^A-Z]+","_"),
                                  TAXA = case_when(TAXA == "L_SCUTULATA" ~ "L.SCUTULATA",
                                                   TAXA == "L_SITKANA" ~ "L.SITKANA",
                                                   TRUE ~ TAXA)# replace spaces with underscores
                                  ) %>% 
                    dplyr::filter(!TAXA %in% c("RALFSIA_HILD","ANTHOPL_ARTEMESIA","ANTHO_ARTEMESIA",
                                               "PETROCELIA","PETROCELIS","SPIRORBIDAE","KATHARINA","KATHRINA",
                                               "BARE_ROCK","ROCK","BOULDER_COBBLE","SAND_GRAVEL",
                                               "GENERAL_NOTES_","MODERATE_SET_OF_MUSSELS",
                                               "FUCUS_VERY_DENSE_VIRTUALLY_ALL_PLANTS_NEW_SINCE_WINTER_MOST_REPRODUCTIVE_FEW_SECOND_OR_MULTI_YEAR_PLANTS",
                                               "RE_SAMPLING_DID_NOT_ESTIMATE_OR_RECORD_NCC_PETROCELIS_RALFSIA_HILDENBRANDIA_",
                                               "NOTES_AF_TK_JJ_","_NO_LEATHESIA_RELOAD_TIDBITS_DOWNLOAD_CB_",
                                               "TREATMENT_","NO_TAXA_FUCUS_","SUM_NO_SPECIES_FUCUS_",
                                               "AVG_NO_SPECIES_FUCUS_",
                                               "NOTE_WHEN_THE_NUMBER_OF_SPORELINGS_GERMLINGS_IS_IT_IS_LIKELY_AN_UNDERESTIMATE_SEE_THE_GERMLING_ESTIMATES_IN_THE_BARNACLE_COUNT_DATA",
                                               "NOTE_THE_NUMBER_OF_ADULTS_IS_AN_ESTIMATE_ESP_ROUGH_ESTIMATE_AS_THE_NUMBER_OF_INDIVIDUALS_INCREASES_ESP_WHEN_OVER_APPROACHES_"
                                               ),
                                  TRIPLET %in% c(1:12)) %>%
                   
               # converts select columns to numeric
             
             # replace NAs with 0, because Terrie says missing values represent 0s for certain categories
             df1[is.na(df1)] <- 0 
             
             # return
             return(df1)
}

# make list of just those more uniform dataframes
X_recent <- X_long_all[c("2017","2016","2015","2014","2013","2012","2011","2010","2009",
                         "2008","2007","2006","2005","2004","2003","2002","2001")] 
#12, 09, 08, 03, 01  # these need help in other dimension

# apply fix_data function to list of data frames
clean_17_01 <- lapply(X_recent, function(x) fix_data3(x))

# fix the 2006 duplicate species column issue 
clean_17_01$`2006` <- clean_17_01$`2006` %>%
                      dplyr::filter(!TAXA %in% c("ERECT_CORALLINE","ACROSIPHONIA","CALLITHAMNION")) %>%
                      dplyr::mutate(TAXA = case_when(TAXA == "ERECT_CORALLINE_" ~ "ERECT_CORALLINE",
                                                     TAXA == "ACROSIPHONIA_" ~ "ACROSIPHONIA",
                                                     TAXA == "CALLITHAMNION_PIKEANUM" ~ "CALLITHAMNION",
                                                     TRUE ~ TAXA))

# put all data frames into one giant one
Data_clean <- do.call("rbind", clean_17_01)

# make column for Year using data frame name
Data_clean$YEAR <- rep(names(clean_17_01), sapply(clean_17_01, nrow))
rownames(Data_clean) <- c()  # removes row names

# NOTE: don't forget to bind in 1999 and 2000!
AllData_clean_long <- rbind(Data_clean, clean_99_00)

# spread all giant dataframe back to each species having a column
AllData_clean <- AllData_clean_long %>%
                 tidyr::spread(key=TAXA, value=PER_COV_OR_COUNT)

# replace NAs with 0, because Terrie says missing values represent 0s for certain categories
AllData_clean[is.na(AllData_clean)] <- 0 
             

# write.csv(AllData_clean, file = "K_Bay_All_Sp_Yrs_Clean.csv", row.names=FALSE)






