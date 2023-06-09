################################################################
#####  Terrie Klinger's Kasitsna Bay Data                  #####
#####  All Invertebrate Data Cleaning Script               #####
#####  by Rachael E. Blake                                 #####
#####  1 May 2017 - updated Nov 2020                       #####
#####  Updated July 2022                                   #####
################################################################

library(readxl) ; library(tidyverse) ; library(reshape2) ; library(stringr)
library(here)

# read in excel file 
# this function creates a list of data frames, one for each excel sheet 
#' Title
#'
#' @param filename 
#'
#' @return
#' @export
#'
#' @examples
read_excel_allsheets <- function(filename) {
                        sheets <- readxl::excel_sheets(filename)
                        x <- lapply(sheets, function(X) 
                                    readxl::read_excel(filename, sheet = X, skip = 1,
                                                       col_types = "text"))
                        names(x) <- sheets
                        
                        return(x)
}

X_sheets_m <- read_excel_allsheets(here("ClearedPlots_1999-2022.xlsx"))

# remove the "notes" sheet and years of data we're not using
X_sheets_yrs <- X_sheets_m[c(2:22)]  # this includes 2002 - 2022 data

# make each data frame long instead of wide
X_long_all <- lapply(X_sheets_yrs, function(x) as.data.frame(t(x)))


#' Function to clean dataframes from excel
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
fix_data3 <- function(df){
             # make column names
             names(df) <- as.character(unlist(df[1,]))
             df <- df[-1,]
             df <- as.data.frame(df)
             
             # remove entirely blank column
             df <- df[!is.na(names(df))]
             
             # print(names(df))
             
             df1 <- df %>%
                    tibble::rownames_to_column(var = "TRIPLET") %>%   # row names to column 
                    setNames(toupper(names(.))) %>%  # make column names all upper case
                    dplyr::mutate(across(where(is.factor), as.character)) %>%  # converts all columns to character
                    tidyr::gather(-TRIPLET, 
                                  -QUAD, -TREATMENT, key = TAXA, value = PER_COV_OR_COUNT) 
             
             df2 <- df1 %>%  
                    dplyr::mutate(TRIPLET = gsub("\\.\\.\\..*$", "", TRIPLET), # split off extra numerals from "TRIPLET" column
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
                                                   TAXA == "_MARGARITIES_SMALL_IRIDESCENT_SNAIL" ~ "MARGARITIES_SMALL_IRIDESCENT_SNAIL",
                                                   TRUE ~ TAXA)# replace spaces with underscores
                                  ) %>% 
                    dplyr::filter(!TAXA %in% c("FUCUS_NUM_ADULTS", "FUCUS_SPORELINGS_NUM",
                                               "RALFSIA_HILD", "PETROCELIA", "PETROCELIS",
                                               "ERECT_CORALLINE", "BUCCINUM", "SPIRORBIDAE",
                                               "KATHARINA", "ANTHO_ARTEMESIA",
                                               "BARE_ROCK","ROCK","BOULDER_COBBLE","SAND_GRAVEL",
                                               "MELANOSIPHON_SOMETIMES_RECORDED_AS_MYELOPHYCUS",
                                               "BARNACLE_SPAT_PROBABLY_PRESENT_AT_LOW_DENSITY_AND_NOT_RECORDED",
                                               "LIKELY_MORE_OFTEN_PRESENT_THAN_RECORDED"
                                               # "GENERAL_NOTES_","MODERATE_SET_OF_MUSSELS",
                                               # "FUCUS_VERY_DENSE_VIRTUALLY_ALL_PLANTS_NEW_SINCE_WINTER_MOST_REPRODUCTIVE_FEW_SECOND_OR_MULTI_YEAR_PLANTS",
                                               # "RE_SAMPLING_DID_NOT_ESTIMATE_OR_RECORD_NCC_PETROCELIS_RALFSIA_HILDENBRANDIA_",
                                               # "NOTES_AF_TK_JJ_","_NO_LEATHESIA_RELOAD_TIDBITS_DOWNLOAD_CB_",
                                               # "TREATMENT_","NO_TAXA_FUCUS_","SUM_NO_SPECIES_FUCUS_",
                                               # "AVG_NO_SPECIES_FUCUS_",
                                               # "NOTE_WHEN_THE_NUMBER_OF_SPORELINGS_GERMLINGS_IS_IT_IS_LIKELY_AN_UNDERESTIMATE_SEE_THE_GERMLING_ESTIMATES_IN_THE_BARNACLE_COUNT_DATA",
                                               # "NOTE_THE_NUMBER_OF_ADULTS_IS_AN_ESTIMATE_ESP_ROUGH_ESTIMATE_AS_THE_NUMBER_OF_INDIVIDUALS_INCREASES_ESP_WHEN_OVER_APPROACHES_"
                                               )
                                  ) %>% 
                    dplyr::select(-TREATMENT, -TRIPLET)
                   
             # replace NAs with 0, because Terrie says missing values represent 0s for certain categories
             df2[is.na(df2)] <- 0 
             
             # return
             return(df2)
             }

# make list of just those dataframes we want to use
X_recent <- X_long_all[c("2022", "2021", "2020", "2019", 
                         "2018", "2017", "2016", "2015", "2014", "2013", 
                         "2012", "2011", "2010", "2009", "2008", "2007", 
                         "2006", "2005", "2004", "2003", "2002"#, "2001"
                         )] 
# 2018, 2012, 2010, 2011, 2008, 2003  # these need help in other dimension

# apply fix_data function to list of data frames
clean_recent <- lapply(X_recent, fix_data3)

# fix the 2006 duplicate species column issue 
clean_recent$`2006` <- clean_recent$`2006` %>%
                       dplyr::filter(!TAXA %in% c("ERECT_CORALLINE","ACROSIPHONIA","CALLITHAMNION")) %>%
                       dplyr::mutate(TAXA = case_when(TAXA == "ERECT_CORALLINE_" ~ "ERECT_CORALLINE",
                                                      TAXA == "ACROSIPHONIA_" ~ "ACROSIPHONIA",
                                                      TAXA == "CALLITHAMNION_PIKEANUM" ~ "CALLITHAMNION",
                                                      TRUE ~ TAXA))

# look at the taxa in each year
# names_all <- lapply(clean_recent, function(x) unique(x[,2]))
# names_all_df <- as.data.frame(do.call(cbind, names_all))
# alpha <- names_all_df %>% summarize(across(everything(), sort))
# which(names_all_df == "CLADOPHORA", arr.ind = TRUE)

# inspect each year for odd things to make sure we don't have notes, etc where data should be!
#' Title
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
inspect <- function(df){
           # print(names(df))
           q_test <- unique(df$QUAD)
           print(q_test)
           t_test <- unique(df$TAXA)
           print(t_test)
           is.numeric(df$PER_COV_OR_COUNT)
           n_test <- nchar(df$PER_COV_OR_COUNT)
           print(unique(n_test))
           }

lapply(clean_recent, inspect)


# take the sum within each quadrat for grouping taxa that are similar
#' Title
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
sum_taxa <- function(df){
            df3 <- df %>%   
                   # spread dataframe so I can work with it
                   tidyr::spread(key = TAXA, value = PER_COV_OR_COUNT) %>% 
                   mutate(across(everything(), as.numeric)) %>% 
                   rowwise(QUAD) %>% 
                   mutate(ULVA_ENTERO_SUM = sum(c(ULVA, ENTEROMORPHA)), # sum taxa together
                          PTERO_POLY_SUM = sum(c(PTEROSIPHONIA, POLYSIPHONIA))) %>% 
                   ungroup() %>% 
                   select(-ULVA, -ENTEROMORPHA, -PTEROSIPHONIA, -POLYSIPHONIA)
            
            if("MELANOSIPHON" %in% names(df3)){
               df4 <- df3 %>% 
                      mutate(MYEL_OR_MELANO = MELANOSIPHON) %>% 
                      select(-MELANOSIPHON)
            }else{
               df4 <- df3 %>% 
                      mutate(MYEL_OR_MELANO = MYELOPHYCUS) %>% 
                      select(-MYELOPHYCUS)
            }

            df5 <- df4 %>% 
                   tidyr::gather(-QUAD, key = TAXA, value = PER_COV_OR_COUNT) 
                    
            return(df5)       
            
            }

# need to iterate over every dataframe in the list for these groupings
sum_recent <- lapply(clean_recent, sum_taxa)

# put all data frames into one giant one
Data_clean <- do.call("rbind", sum_recent)

# make column for Year using data frame name
Data_clean$YEAR <- rep(names(sum_recent), sapply(sum_recent, nrow))
rownames(Data_clean) <- c()  # removes row names

# NOTE: don't forget to bind in 1999 and 2000!
# AllData_clean_long <- rbind(Data_clean, clean_99_00)

# spread all giant dataframe back to each species having a column
AllData_clean <- Data_clean %>%
                 tidyr::spread(key = TAXA, value = PER_COV_OR_COUNT)

# replace NAs with 0, because Terrie says missing values represent 0s for certain categories
AllData_clean[is.na(AllData_clean)] <- 0 
             

# write_csv(AllData_clean, here("./data_clean/K_Bay_All_Sp_Yrs_Clean.csv"))


#######################################################################
#######################################################################
# # split years 1999 and 2000 require more reformatting
# X_long_1999 <- X_long_all$`1999`
# X_long_2000 <- X_long_all$`2000`

# # format 1999 dataframe
# X_long_1999 <- tibble::rownames_to_column(X_long_1999)
# X_long_1999 <- mutate_all(X_long_1999, as.character)
# X_long_1999[1,2] <- as.character(X_long_1999[3,1])
# X_long_1999 <- X_long_1999[ , -c(1)]  # remove first column (isn't important)
# names(X_long_1999) <- as.character(unlist(X_long_1999[1, ])) # make column names
# X_long_1999 <- X_long_1999[-c(1:2), ]  # remove first two rows containing column names
# X_long_1999 <- X_long_1999[!is.na(names(X_long_1999))] # removing columns with NA as name 

# clean_1999 <- X_long_1999 %>%
#               # tibble::rownames_to_column() %>%  # making column of rownames
#               dplyr::select(-`Triplet/Tag No.`, -`Fucus wt wt (kg) scrape '99`, -`Ralfsia (%)`, -`Hildenbrandia (%)`, # -rowname, 
#                             -`Petrocelis (%)`, -`Total Algal Coverage (%)`, -`Barnacle adult (%)`,
#                             -`Total Animal Cover (%)`, -`Bare Rock (%)`, -`Total Coverage (%)`,
#                             -`Total Alg+Anim Cvr (%)`) %>%   # removing the columns
#               dplyr::rename(QUAD = `Species`,
#                             FUCUS_PERCOV_TOTAL = `Fucus (%)`,
#                             FUCUS_NUM_ADULTS = `Fucus (#)`,
#                             FUCUS_SPORELINGS_PERCOV = `Fucus germ (%)`,
#                             FUCUS_SPORELINGS_NUM = `Fucus germ (#)`,
#                             ACROSIPHONIA = `Acrosiphonia (%)`,
#                             CHAETOMORPHA = `Chaetomorpha (%)`,
#                             CRYPTOSIPHONIA = `Cryptosiphonia (%)`,
#                             ENDOCLADIA = `Endocladia (%)`,
#                             GLOIOPELTIS = `Gloiopeltis (%)`,
#                             HALOSACCION = `Halosaccion (%)`,
#                             MASTO_PAP = `Mastocarpus (%)`,
#                             MELANOSIPHON = `Melanosiphon (%)`, 
#                             ODONTHALIA = `Odonthalia (%)`,
#                             PALMARIA = `Palmaria c (%)`,  
#                             PILAYELLA = `Pilayella (%)`,
#                             PORPHYRA = `Porphyra (%)`,
#                             PTEROSIPHONIA = `Pterosiphonia (%)`,
#                             L.SITKANA = `Litt sitkana (#)`,
#                             L.SCUTULATA = `Litt scutulata (#)`,
#                             BARNACLES =  `Totl Barnacle Cvr (%)`,
#                             BARNACLE_SPAT = `Barnacle spat (%)`,
#                             MYTILUS = `Mytilus (%)`,
#                             NUCELLA = `Nucella (#)`,
#                             PAGURUS = `Pagurus (#)`,
#                             SIPHONARIA = `Siphonaria (#)`,
#                             ONCHIDELLA = `Onchidella (#)`,
#                             LOTTIIDAE_JUV = `Lottiidae juv (#)`,
#                             LOTTIA_P_BOREALIS = `Lottia pelta/borealis (#)`,
#                             L_DIGIT_STRIG = `L digit/strig (#)`,
#                             NEREIDAE = `Nereidae (#)`,
#                             EMPLECTONEMA = `Emplectonema (#)`,
#                             PARANEMERTES = `Paranemertes (#)`,
#                             AMPHIPORUS = `Amphiphorus (#)`,
#                             MARGARITES = `Margarites (#)`,
#                             CUCUMARIA = `Cucumaria (#)`,
#                             LEPTASTERIAS = `Leptasterias (#)`
#                             )
# 
# names(clean_1999) <- make.names(names(clean_1999))  # will turn names into accepted formats
  
# clean_1999n <- clean_1999 %>%
#                # dplyr::rename(TRIPLET = `NA.`) %>%
#                dplyr::mutate(YEAR = "1999",
#                              TREATMENT = ifelse(QUAD %in% c(102,105,108,116,120,122,170,505,532,
#                                                             537,542,546), "CONTROL",
#                                          ifelse(QUAD %in% c(101,107,113,117,118,123,504,523,531,
#                                                             538,541,545), "SCRAPE00",
#                                          ifelse(QUAD %in% c(103,106,114,115,119,121,503,522,533,
#                                                             536,540,547), "SCRAPE99", "")))) %>%
#                setNames(toupper(names(.))) %>% # make column names all upper case
#                dplyr::mutate_if(is.factor, as.character) %>%  # converts all columns to character
#                dplyr::mutate(across(c(3:4,7:38), as.numeric)) %>% # converts select columns to numeric
#                dplyr::mutate(LOTTIIDAE = LOTTIIDAE_JUV + LOTTIA_P_BOREALIS) %>%
#                dplyr::select(-LOTTIIDAE_JUV, -LOTTIA_P_BOREALIS) %>%
#                tidyr::gather(-QUAD, -TREATMENT, -YEAR, key=TAXA, value=PER_COV_OR_COUNT)
# 
# # replace NAs with 0, because Terrie says missing values represent 0s for some variables
# clean_1999n[is.na(clean_1999n)] <- 0
  

# # format 2000 dataframe
# names(X_long_2000) <- as.character(unlist(X_long_2000[1,])) # make column names 
# X_long_2000 <- X_long_2000[-1,]
# X_long_2000 <- X_long_2000[!is.na(names(X_long_2000))]  # removing two columns called `NA`
# # X_long_2000 <- X_long_2000[,-22]  # removing the other `NA`` column
            
# clean_2000 <- X_long_2000 %>%
#               tibble::rownames_to_column() %>%  # making column of rownames
#               dplyr::select(-rowname, -`RALFSIA/HILD`, -PETROCELIS, -SPIRORBIDAE,
#                             -KATHRINA, -`BARE ROCK`, -ROCK, -`BOULDER-COBBLE`,
#                             -`SAND-GRAVEL`) %>%   # removing columns
#               dplyr::rename(FUCUS_PERCOV_TOTAL = `FUCUS%TOTAL`,
#                             FUCUS_NUM_ADULTS = `FUCUS#ADULTS`,
#                             FUCUS_SPORELINGS_PERCOV = `FUCUS SPORELINGS%`,
#                             FUCUS_SPORELINGS_NUM = `FUCUS SPORELINGS#`,
#                             CLAD_SERICEA = `CLAD SERICEA`,
#                             MASTO_PAP = `MASTO PAP`,
#                             L.SITKANA = `L SITKANA`,
#                             L.SCUTULATA = `L SCUTULATA`,
#                             BARNACLE_SPAT = `BARNACLES SPAT`,
#                             MARGARITES = `MARGARITS JUV`        
#                             ) %>%  # rename columns that had special characters
#               setNames(toupper(names(.))) %>% # make column names all upper case
#               dplyr::filter(!(TREATMENT %in% c("avg, sd","avg,sd","AVG,STD"))) %>%
#               dplyr::mutate(YEAR = "2000") %>% 
#               dplyr::mutate(across(where(is.factor), as.character)) %>%  # converts all columns to character
#               dplyr::mutate(across(c(4:6,8:36), as.numeric)) %>% # converts select columns to numeric
#               tidyr::gather(-TRIPLET, -QUAD, -TREATMENT, -YEAR, key=TAXA, value=PER_COV_OR_COUNT)
# 
# # replace NAs with 0, because Terrie says missing values represent 0s for many columns
# clean_2000[is.na(clean_2000)] <- 0
# 
# 
# 
# ## Bind 1999 and 2000 together into one dataframe
# clean_99_00 <- full_join(clean_1999n, clean_2000, by=c("QUAD","TREATMENT","YEAR", # "TRIPLET",
#                                                        "TAXA","PER_COV_OR_COUNT"))
 

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


