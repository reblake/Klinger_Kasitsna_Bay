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
URL_pdo <- "https://oceanview.pfeg.noaa.gov/erddap/tabledap/cciea_OC_PDO.htmlTable?&time%3E=1900-01-01&time%3C=2020-09-01"
pdo_raw <- read_html(URL_pdo)
pdo_tab <- pdo_raw %>% 
           # html_node("xml_children") %>%
           xml_child(2) %>% xml_child(3) %>% 
           html_text()
pdo_cols <- scan(textConnection(pdo_tab), nlines=2, what=character())# Get header row
pdo_df <- read.table(file=textConnection(pdo_tab), skip=4, stringsAsFactors=F, sep="", #nrows=119, 
                     header=FALSE, col.names=pdo_cols, strip.white=TRUE, fill=TRUE)
# pdo_df$YEAR <- substr(pdo_df$YEAR, 1, 4) # removes asterisks from years 2002-2015

#
pdo_mon <- pdo_df %>% 
           dplyr::rename(Year=YEAR) %>% # rename data columns         
           dplyr::filter(Year %in% c(1998:2019)) %>% # selects years 
           tidyr::gather(Month, PDO, -Year) # reshapes data to be column-wise

#
pdo_ann <- pdo_df %>% 
           dplyr::rename(Year=YEAR) %>% # rename data columns         
           dplyr::filter(Year %in% c(1998:2019)) %>% # selects years 
           tidyr::gather(Month, PDO, -Year) %>% # reshapes data to be column-wise
           dplyr::group_by(Year) %>%
           dplyr::summarize(PDO_anul_mn=mean(as.numeric(as.character(PDO)), na.rm = TRUE)) %>% # get annual means
           dplyr::ungroup()


