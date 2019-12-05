############################################################
#####  Terrie Klinger's Kasitsna Bay Data              #####
#####  PDO data cleaning script                        #####
#####  by Rachael E. Blake                             #####
#####  1 May 2017                                      #####
############################################################

## load packages (order matters)
library(httr) ; library(plyr) ; library(XML) ; library(curl)
library(rvest) ; library(tidyr) ; library(stringr)


### Download the data

###  Pacific Decadal Oscillation Index (PDO): 
URL_pdo <- "http://jisao.washington.edu/pdo/PDO.latest"
pdo_raw <- read_html(URL_pdo)
pdo_pre <- pdo_raw %>% 
           html_node("p") %>%
           html_text()
pdo_cols <- scan(textConnection(pdo_pre), skip=29, nlines=1, what=character())# Get header row
pdo_df <- read.table(file=textConnection(pdo_pre), skip=31, nrows=119, stringsAsFactors=F, sep="", 
                     header=FALSE, col.names=pdo_cols, strip.white=TRUE, fill=TRUE)
pdo_df$YEAR <- substr(pdo_df$YEAR, 1, 4) # removes asterisks from years 2002-2015

#
pdo_mon <- pdo_df %>% 
           dplyr::rename(Year=YEAR) %>% # rename data columns         
           dplyr::filter(Year %in% c(1998:2016)) %>% # selects years 
           tidyr::gather(Month, PDO, -Year) # reshapes data to be column-wise

#
pdo_ann <- pdo_df %>% 
           dplyr::rename(Year=YEAR) %>% # rename data columns         
           dplyr::filter(Year %in% c(1998:2016)) %>% # selects years 
           tidyr::gather(Month, PDO, -Year) %>% # reshapes data to be column-wise
           dplyr::group_by(Year) %>%
           dplyr::summarize(PDO_anul_mn=mean(as.numeric(as.character(PDO)), na.rm = TRUE)) %>% # get annual means
           dplyr::ungroup()


