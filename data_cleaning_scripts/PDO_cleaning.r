############################################################
#####  Terrie Klinger's Kasitsna Bay Data              #####
#####  PDO data cleaning script                        #####
#####  by Rachael E. Blake                             #####
#####  1 May 2017                                      #####
############################################################

## load packages 
library(httr) ; library(tidyverse) ; library(XML) ; library(curl)
library(rvest) ; library(xml2)

### Download the data programmatically using web scraping (depends on the validity of the URL)
###  Pacific Decadal Oscillation Index (PDO): 
URL_pdo <- "https://oceanview.pfeg.noaa.gov/erddap/tabledap/cciea_OC_PDO.htmlTable?&time%3E=1900-01-01&time%3C=2021-12-31"
pdo_raw <- read_html(URL_pdo)
pdo_tab <- pdo_raw %>% 
           # html_node("xml_children") %>%
           xml_child(2) %>% xml_child(3) %>% 
           html_text()
pdo_cols <- scan(textConnection(pdo_tab), nlines = 2, what = character()) # Get header row
pdo_cols[1] <- paste(pdo_cols[1], "_UTC", sep = "") # Specify that time is UTC
pdo_df1 <- read.table(file = textConnection(pdo_tab), skip = 4, stringsAsFactors=F, sep="\n", #nrows=119, 
                      header = FALSE, strip.white = TRUE)#, col.names = pdo_cols)#, fill = TRUE)
# frustrated with this super manual way, but can't find a way to do it in the above read.table() call
V1 <- filter(pdo_df1, grepl("Z", V1))
V2 <- filter(pdo_df1, !grepl("Z", V1))
pdo_df <- data.frame(V1, V2)
names(pdo_df) <- pdo_cols


#
pdo_mon <- pdo_df %>% 
                mutate(Year = sapply(strsplit(as.character(time_UTC), split="-") , function(x) x[1]),
                       Year = as.numeric(Year),
                       Month = sapply(strsplit(as.character(time_UTC), split="-") , function(x) x[2]),
                       Month = as.numeric(Month)) %>% 
                filter(Year %in% c(1998:2021)) #%>% # selects years 


#
pdo_ann <- pdo_mon %>%          
           #tidyr::gather(Month, PDO, -Year) %>% # reshapes data to be column-wise
           dplyr::group_by(Year) %>%
           dplyr::summarize(PDO_anul_mn=mean(as.numeric(as.character(PDO)), na.rm = TRUE)) %>% # get annual means
           dplyr::ungroup()


# pdo_df$YEAR <- substr(pdo_df$YEAR, 1, 4) # removes asterisks from years 2002-2015