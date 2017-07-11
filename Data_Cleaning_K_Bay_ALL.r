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
                            MASTOCARPUS = `Mastocarpus (%)`,
                            MELANOSIPHON = `Melanosiphon (%)`,
                            ODONTHALIA = `Odonthalia (%)`,
                            PALMARIA_C = `Palmaria c (%)`,
                            PILAYELLA = `Pilayella (%)`,
                            PORPHYRA = `Porphyra (%)`,
                            PTEROSIPHONIA = `Pterosiphonia (%)`,
                            L.SITKANA = `Litt sitkana (#)`,
                            L.SCUTULATA = `Litt scutulata (#)`,
                            BARNACLES =  `Totl Barnacle Cvr (%)`,
                            BARNACLES_SPAT = `Barnacle spat (%)`,
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
               dplyr::mutate(Year = "1999") %>%
               setNames(toupper(names(.))) # make column names all upper case
# replace NAs with 0, because Terrie says missing values represent 0s for some variables
clean_1999n[is.na(clean_1999n)] <- 0
  

# format 2000 dataframe
names(X_long_2000) <- as.character(unlist(X_long_2000[1,])) # make column names 
X_long_2000 <- X_long_2000[-1,]
X_long_2000 <- X_long_2000[ , !duplicated(colnames(X_long_2000))]  # removing one column called `NA`
            
clean_2000 <- X_long_2000 %>%
              tibble::rownames_to_column() %>%  # making column of rownames
              dplyr::select(-`NA`, -rowname, -`RALFSIA/HILD`, -`PETROCELIS`, -`SPIRORBIDAE`,
                            -`KATHRINA`, -`BARE ROCK`, 
                            -`ROCK`, -`BOULDER-COBBLE`, -`SAND-GRAVEL`) %>%   # removing columns
              dplyr::rename(FUCUS_PERCOV_TOTAL = `FUCUS%TOTAL`,
                            FUCUS_NUM_ADULTS = `FUCUS#ADULTS`,
                            FUCUS_SPORELINGS_PERCOV = `FUCUS SPORELINGS%`,
                            FUCUS_SPORELINGS_NUM = `FUCUS SPORELINGS#`,
                            CLAD_SERICEA = `CLAD SERICEA`,
                            MASTO_PAP = `MASTO PAP`,
                            L.SITKANA = `L SITKANA`,
                            L.SCUTULATA = `L SCUTULATA`,
                            BARNACLES_SPAT = `BARNACLES SPAT`,
                            MARGARITS_JUV = `MARGARITS JUV`
                            ) %>%  # rename columns that had special characters
              setNames(toupper(names(.))) %>% # make column names all upper case
              dplyr::filter(!(TREATMENT %in% c("avg, sd","avg,sd","AVG,STD"))) %>%
              dplyr::mutate_if(is.factor, as.character) %>%  # converts all columns to character
              dplyr::mutate_at(c(4:6,8:35), funs(as.numeric)) %>% # converts select columns to numeric
              dplyr::mutate(Year = "2000", Month = "06", Day = "05") 
              
# replace NAs with 0, because Terrie says missing values represent 0s for many columns
clean_2000[is.na(clean_2000)] <- 0

 

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
                    tidyr::gather(-TRIPLET, -QUAD, -TREATMENT, key=TAXA, value=PER_COV_OR_COUNT) %>%
                    # split off extra numerals from "TRIPLET" column
                    dplyr::mutate(TRIPLET = ifelse(!(TRIPLET %in% c(1:12)), 
                                                   sapply(strsplit(as.character(TRIPLET), split="__") , function(x) x[1]),
                                                   TRIPLET),
                                  TAXA = case_when(TAXA == "irridescent snail (Homalopoma?)" ~ "IRRIDESCENT_SNAIL_HOMALOPOMA",
                                                   TAXA == "Cryptonatica[Acrosiphonia]" ~ "CRYPTONATICA",
                                                   TAXA == "FUCUS SPORELINGS#" ~ "FUCUS_SPORELINGS_NUM",
                                                   TAXA == "FUCUS SPORELINGS%" ~ "FUCUS_SPORELINGS_PERCOV",
                                                   TAXA == "FUCUS#ADULTS" ~ "FUCUS_NUM_ADULTS",
                                                   TAXA == "FUCUS%TOTAL" ~ "FUCUS_PERCOV_TOTAL",
                                                   TRUE ~ TAXA),
                                  TAXA = str_replace_all(TAXA,"[^A-Z]+","_"),
                                  TAXA = case_when(TAXA == "L_SCUTULATA" ~ "L.SCUTULATA",
                                                   TAXA == "L_SITKANA" ~ "L.SITKANA",
                                                   TRUE ~ TAXA)# replace spaces with underscores
                                  ) %>% 
                    dplyr::filter(!TAXA %in% c("RALFSIA_HILD","ANTHOPL_ARTEMESIA","PETROCELIA",
                                               "PETROCELIS","SPIRORBIDAE","KATHARINA","KATHRINA",
                                               "BARE_ROCK","ROCK","BOULDER_COBBLE","SAND_GRAVEL",
                                               "GENERAL_NOTES_")) %>%
                    dplyr::mutate_at(vars(PER_COV_OR_COUNT), funs(as.numeric)) # converts select columns to numeric
             
             # replace NAs with 0, because Terrie says missing values represent 0s for certain categories
             df1[is.na(df1)] <- 0 
             
             # return
             return(df1)
}

# subset just years without too much cleaning needed
X_recent <- X_long_all[c("2017","2016","2015",#"2014","2013"#,
                         #"2012",
                         #"2011"#,"2010",
                         #"2009"#,"2008",
                         "2007","2006","2005","2004",
                         #"2003",
                         "2002"#,
                         #"2001"
                         )] 

#12, 09, 08, 03, 01  # these need help in other dimension

# apply fix_data function to list of data frames
clean_17_02 <- lapply(X_recent, function(x) fix_data3(x))







# put all data frames into one giant one
# note: don't forget to bind in 1999 and 2000!
AllData_clean <- do.call("rbind", clean_17_02)
  

# make column for Year using data frame name
AllData_clean$Year <- rep(names(clean_17_02), sapply(clean_17_02, nrow))





