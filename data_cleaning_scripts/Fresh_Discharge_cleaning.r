#################################################################
#####  Terrie Klinger's Kasitsna Bay Data                   #####
#####  Hill model freshwater discharge data cleaning script #####
#####  by Rachael E. Blake                                  #####
#####  17 May 2017                                          #####
#################################################################

###########################
### load necessary packages
library(ncdf4) ; library(chron) ; library(tidyverse) ; library(forcats) ; library(glue) ; library(forcats)

###########################
### figure out how to pull in a subset of the full modeled data (too large for R)
### NOTE: this testing was done first in the 'testing_netcdf.R', and with Quentin Read's help
### in the 'netcdf_subsetting.Rmd' files in this repo
# baseurl <- 'http://thredds.aoos.org/thredds/dodsC/GOA_RUNOFF_DISCHARGE.ncml' # base URL without subsetting
# query_latlon <- glue('{baseurl}?lat,lon')  # paste URL to get latlon
# test_latlon <- nc_open(query_latlon) # read using ncdf4
# print(test_latlon)
# lat_deg_north <- ncvar_get(test_latlon, "lat") # get full latitude
# lon_deg_east <- ncvar_get(test_latlon, "lon") # get full longitude
# latlon_df <- data.frame(as.vector(lat_deg_north), as.vector(lon_deg_east)) # make lat-lon dataframe
# names(latlon_df) <- c("latitude_deg_north", "longitude_deg_east")
# 
# limits <- c(-151.40, 59.44, -151.63, 59.54) # limits of data needed

### try to find the limits programmatically
# get the indexes for latitude values in the desired range
# lat_m <- which(lat_deg_north >= limits[2] & lat_deg_north <= limits[4], arr.ind=TRUE)
# lat_df <- data.frame(lat_m)
# min(lat_df$row); max(lat_df$row)#; unique(lat_df$row)  # 1810 unique rows
# min(lat_df$col)-1; max(lat_df$col)-1#; sort(unique(lat_df$col)) 
# # get the indexes for longitude values in the desired range
# lon_m <- which(lon_deg_east >= limits[3] & lon_deg_east <= limits[1], arr.ind=TRUE)
# lon_df <- data.frame(lon_m)
# min(lon_df$row)-1; max(lon_df$row)-1#; sort(unique(lon_df$row))
# min(lon_df$col); max(lon_df$col)#;unique(lon_df$col)  # 900 unique cols

### NOTE: ended up just tweaking the query to the base URL, and then plotting to get the correct
### subset of data within the limits (defined above).  Programmatic way failed. 
### Had to arrive at final index numbers by trial and error.
### Final indexes for y were 353:363, and for x were 314:325 
# query_finale <- glue("{baseurl}?lat[353:1:363][314:1:325],lon[353:1:363][314:1:325],q[0:1:12784][353:1:363][314:1:325],time[0:1:12784]")
# test_dat <- nc_open(query_finale)
# lat_deg_north <- ncvar_get(test_dat, "lat") # get latitude
# long_deg_east <- ncvar_get(test_dat, "lon") # get longitude
# look at extent of file
# plot_df <- data.frame(as.vector(latitude_deg_north), as.vector(longitude_deg_east))
# names(plot_df) <- c("latitude_deg_north", "longitude_deg_east")
# ggplot(data = plot_df, aes(x=longitude_deg_east, y=latitude_deg_north)) + 
#        geom_tile() + coord_equal()

###########################
### read in the netcdf file 
# nc_kbay <- nc_open("GOA_RUNOFF_DISCHARGE.ncml.nc") # uses a static file created in 2017
# nc_kbay <- nc_open("GOA_FWDischarge_2013_2018/goa_dischargex_09012017_08312018.nc") # uses static file in Google Drive from Seth Danielson
# nc_kbay <- nc_open(query_finale) # download directly from OPeNDAP
# print(nc_kbay) # shows info about the file
# names(nc_kbay$dim) #display dimensions
# names(nc_kbay$var) #display variables

# get latitude
# latitude_deg_north <- ncvar_get(nc_kbay, "lat")
# lat_lname <- ncatt_get(nc_kbay, "lat", "long_name")
# lat_units <- ncatt_get(nc_kbay, "lat", "units")

### manually do stuff
# # get latitude
# latitude_deg_north <- ncvar_get(nc_kbay, "lat")
# lat_lname <- ncatt_get(nc_kbay, "lat", "long_name")
# lat_units <- ncatt_get(nc_kbay, "lat", "units")
# 
# # get longitude
# longitude_deg_east <- ncvar_get(nc_kbay, "lon")
# lon_lname <- ncatt_get(nc_kbay, "lon", "long_name")
# lon_units <- ncatt_get(nc_kbay, "lon", "units")

# look at extent of file; 2017 file was spatially subset
# ggplot(data = latlon, aes(x=longitude_deg_east, y=latitude_deg_north)) + geom_tile() + coord_equal() 

# # get time 
# time_date <- ncvar_get(nc_kbay, "time")
# time_date_units <- ncatt_get(nc_kbay, "time", "units")

# # get discharge
# mean_daily_discharge_m3s1 <- ncvar_get(nc_kbay, "q")
# discharge_lname <- ncatt_get(nc_kbay, "q", "long_name")
# discharge_units <- ncatt_get(nc_kbay, "q", "units")
# discharge_fillvalue <- ncatt_get(nc_kbay, "q", "_FillValue")
# 
# # get general attributes
# title <- ncatt_get(nc_kbay, 0, "title")
# summary <- ncatt_get(nc_kbay, 0, "summary")
# institution <- ncatt_get(nc_kbay, 0, "institution")
# history <- ncatt_get(nc_kbay, 0, "history")
# Conventions <- ncatt_get(nc_kbay, 0, "Conventions")
# 
# # close the connection to the netcdf file
# nc_close(nc_kbay)

### Function to get time and convert it to human-readable format

#' Title
#'
#' @param ncfile 
#'
#' @return
#' @export
#'
#' @examples
make_time_human <- function(ncfile){
                   # open file
                   nc_file <- nc_open(ncfile)   
                   # read time
                   time_date <- ncvar_get(nc_file, "time")    
                   time_date_units <- ncatt_get(nc_file, "time", "units")
                   # convert time variable to human-readable format
                   time_unit_str <- strsplit(time_date_units$value, " ")
                   date_str <- strsplit(unlist(time_unit_str)[3], "-")
                   start_month <- as.integer(unlist(date_str)[2])
                   start_day <- as.integer(unlist(date_str)[3])
                   start_year <- as.integer(unlist(date_str)[1]) 
                   # make date column   # NOTE Each array slice is a date!!
                   date <- chron(time_date, origin = c(start_month, start_day, start_year), 
                                 out.format = "m/d/year")   
                   # close file
                   nc_close(nc_file)
                   
                   return(date)
}
###

### Function to get year, month, and day from 2014-2018 files
#' Title
#'
#' @param ncfile 
#'
#' @return
#' @export
#'
#' @examples
make_dmy <- function(ncfile){
            # open file
            nc_file <- nc_open(ncfile) 
            # read year, month, day
            year <- ncvar_get(nc_file, "year") 
            month <- ncvar_get(nc_file, "month") 
            day <- ncvar_get(nc_file, "day") 
            # make dataframe
            dmy_df <- data.frame(year, month, day)
            dmy_df$dates <- paste(month, day, year, sep = "/")
            dmy_df$dates <- chron(dmy_df$date, out.format = "m/d/year")
            # close file
            nc_close(nc_file)
            
            return(dmy_df)
}
###

### Function to create a dataframe of latitudes and longitudes
#' Title
#'
#' @param row_n 
#' @param ncfile 
#'
#' @return
#' @export
#'
#' @examples
make_latlon_df <- function(ncfile, row_n){
                  # open file
                  nc_file <- nc_open(ncfile)
                  # read lat and lon
                  latitude_deg_north <- ncvar_get(nc_file, "lat")
                  longitude_deg_east <- ncvar_get(nc_file, "lon")
                  # make a lat lon dataframe
                  latlon <- as.data.frame(matrix(ncol = 0, nrow = row_n)) #108 or 132
                  latlon$latitude_deg_north <- as.vector(latitude_deg_north)
                  latlon$longitude_deg_east <- as.vector(longitude_deg_east)
                  # close file
                  nc_close(nc_file)
                  
                  return(latlon)
}
###


### Function to create a giant dataframe from slices of the netcdf file
#' create a giant dataframe from slices of the netcdf file
#'
#' @param slice
#' @param row_n 
#' @param latlon_df 
#' @param date_col 
#' @param ncfile 
#'
#' @return
#' @export
#'
#' @examples

slice_2_df <- function(row_n, date_col, slice, ncfile,  latlon_df){
              # make empty dataframe
              df1 <- as.data.frame(matrix(ncol = 0, nrow = row_n))  
              # make date column
              df1$date <- as.character(date_col[[slice]])
              # make day, month, year columns
              df1 <- df1 %>% 
                     mutate(Year = sapply(strsplit(as.character(date), split="/") , function(x) x[3]),
                            Month = sapply(strsplit(as.character(date), split="/") , function(x) x[1]),
                            Day = sapply(strsplit(as.character(date), split="/") , function(x) x[2]),
                            Month = as.factor(Month))  
              # open file
              nc_file <- nc_open(ncfile)
              # get discharge
              mean_daily_discharge <- ncvar_get(nc_file, "q")
              # replace FillValue with NA
              mean_daily_discharge[is.nan(mean_daily_discharge)] <- NA
              # make data column
              if(ncfile == "GOA_RUNOFF_DISCHARGE.ncml.nc"){
                             df1$mean_daily_discharge <- as.vector(mean_daily_discharge[,,slice])
                             }else{
                             df1$mean_daily_discharge <- as.vector(mean_daily_discharge[,slice])  
                             }
              
              # close file
              nc_close(nc_file)
  
              lldf1 <- cbind(latlon_df, df1)
              lldf1 <- dplyr::filter(lldf1, !is.na(mean_daily_discharge))
  
              return(lldf1)
}
###

### get time and date
date_f1 <- make_time_human(ncfile = "GOA_RUNOFF_DISCHARGE.ncml.nc")
date_f2 <- make_dmy(ncfile = "GOA_FWDischarge_2013_2018/goa_dischargex_09012014_08312015.nc")
date_f3 <- make_dmy(ncfile = "GOA_FWDischarge_2013_2018/goa_dischargex_09012015_08312016.nc")
date_f4 <- make_dmy(ncfile = "GOA_FWDischarge_2013_2018/goa_dischargex_09012016_08312017.nc")
date_f5 <- make_dmy(ncfile = "GOA_FWDischarge_2013_2018/goa_dischargex_09012017_08312018.nc")

### get lat and lon
latlon_f1 <- make_latlon_df(ncfile = "GOA_RUNOFF_DISCHARGE.ncml.nc", row_n = 108)
latlon_f2 <- make_latlon_df(ncfile = "GOA_FWDischarge_2013_2018/goa_dischargex_09012014_08312015.nc",
                            row_n = 14052)
latlon_f3 <- make_latlon_df(ncfile = "GOA_FWDischarge_2013_2018/goa_dischargex_09012015_08312016.nc",
                            row_n = 14052)
latlon_f4 <- make_latlon_df(ncfile = "GOA_FWDischarge_2013_2018/goa_dischargex_09012016_08312017.nc",
                            row_n = 14052)
latlon_f5 <- make_latlon_df(ncfile = "GOA_FWDischarge_2013_2018/goa_dischargex_09012017_08312018.nc",
                            row_n = 14052)

### get number of slices to use
# print(nc_kbay) # look at the "Size" of the 'time' dimension - use this as the number of slices
# the max number here is the known number of slices in the original array
# it also happens to be the number of calendar days represented by the dataset
# 5722 is the number of days between 1/1/1999 and 8/31/2014 inclusive
num_slices <- c(1:5722)   # use for processing the original downloaded file from 2017
# num_slices <- c(1:12785)  # programmatically downloaded file (contains data until 8/31/2014)
num_slices <- c(1:365)  # this is the dim of the time ; use for processing the David Hill files for 2014-2018


### get slices of discharge data using the `slice_2_df()` function
FWD_list_f1 <- lapply(num_slices, slice_2_df, ncfile = "GOA_RUNOFF_DISCHARGE.ncml.nc",
                      row_n = 108, latlon_df = latlon_f1, date_col = date_f1)  
FWD_f1 <- bind_rows(FWD_list_f1) %>% 
          # convert FWD_f1 from m3.s-1 to m3.d-1 (meters cubed per second to meters cubed per day)
          mutate(mean_daily_discharge_m3d1 = mean_daily_discharge*86400)
#
# REMEMBER to redefine the num_slices above for these subsequent files!
FWD_list_f2 <- lapply(num_slices, slice_2_df, 
                      ncfile = "GOA_FWDischarge_2013_2018/goa_dischargex_09012014_08312015.nc",
                      row_n = 14052, latlon_df = latlon_f2, date_col = date_f2$dates)  
FWD_f2 <- bind_rows(FWD_list_f2) %>% dplyr::rename(mean_daily_discharge_m3d1 = mean_daily_discharge)
#
FWD_list_f3 <- lapply(num_slices, slice_2_df, 
                      ncfile = "GOA_FWDischarge_2013_2018/goa_dischargex_09012015_08312016.nc",
                      row_n = 14052, latlon_df = latlon_f3, date_col = date_f3$dates)  
FWD_f3 <- bind_rows(FWD_list_f3) %>% dplyr::rename(mean_daily_discharge_m3d1 = mean_daily_discharge)
#
FWD_list_f4 <- lapply(num_slices, slice_2_df, 
                      ncfile = "GOA_FWDischarge_2013_2018/goa_dischargex_09012016_08312017.nc",
                      row_n = 14052, latlon_df = latlon_f4, date_col = date_f4$dates)  
FWD_f4 <- bind_rows(FWD_list_f4) %>% dplyr::rename(mean_daily_discharge_m3d1 = mean_daily_discharge)
#
FWD_list_f5 <- lapply(num_slices, slice_2_df, 
                      ncfile = "GOA_FWDischarge_2013_2018/goa_dischargex_09012017_08312018.nc",
                      row_n = 14052, latlon_df = latlon_f5, date_col = date_f5$dates)  
FWD_f5 <- bind_rows(FWD_list_f5) %>% dplyr::rename(mean_daily_discharge_m3d1 = mean_daily_discharge)

# remove some points that probably don't drain into this site
FWD_less <- FWD_f1 %>% 
            bind_rows(FWD_f2) %>% bind_rows(FWD_f3) %>% bind_rows(FWD_f4) %>% bind_rows(FWD_f5) %>% 
            filter(!(longitude_deg_east < -151.55 & latitude_deg_north < 59.48),
                   !(latitude_deg_north > 59.51 & longitude_deg_east > -151.45)) %>%
            mutate(Month_number = forcats::fct_recode(Month, "01"="Jan", "02"="Feb", "03"="Mar",
                                                             "04"="Apr", "05"="May", "06"="Jun",
                                                             "07"="Jul", "08"="Aug", "09"="Sep",
                                                             "10"="Oct", "11"="Nov", "12"="Dec"))

# check to see the data are approx. correct by plotting the lats and lons
q <- qplot(data = FWD_less, x = longitude_deg_east, y = latitude_deg_north)

# create dummy dataframe for empty 2018 rows (useful in plotting later)
dummy_2018 <- data.frame(Year = c("2018","2018","2018","2018","2019","2019","2019","2019","2019",
                                  "2019","2019","2019","2019","2019","2019","2019","2020","2020",
                                  "2020","2020","2020","2020","2020","2020","2020","2020","2020",
                                  "2020","2021","2021","2021","2021","2021","2021","2021","2021",
                                  "2021","2021","2021","2021"),
                         Month = c("09","10","11","12","01","02","03","04","05","06","07","08",
                                   "09","10","11","12","01","02","03","04","05","06","07","08",
                                   "09","10","11","12","01","02","03","04","05","06","07","08",
                                   "09","10","11","12"))

dummy_2018$Year_Month <- paste(dummy_2018$Year, dummy_2018$Month, sep="-")
dummy_2018$mean_monthly_discharge_m3s1 <- 0.0000001
dummy_2018$mean_monthly_anomaly <- 0.0000001
dummy_2018$Sign <- "B"
dummy_2018$Year_Month2 <- factor(dummy_2018$Year_Month)


# create annual means
FWD_anomaly <- FWD_less %>%
               dplyr::mutate(mean_overall = mean(mean_daily_discharge_m3d1),
                             daily_anomaly = mean_daily_discharge_m3d1 - mean_overall)

FWD_ann_mn <- FWD_anomaly %>%
              dplyr::group_by(Year) %>%
              dplyr::mutate(mean_daily_discharge_m3d1 = mean(mean_daily_discharge_m3d1),
                            SD_yearly_discharge_m3d1 = sd(mean_daily_discharge_m3d1),
                            SE_yearly_discharge_m3d1 = SD_yearly_discharge_m3d1/sqrt(n()),
                            mean_yearly_anomaly = mean(daily_anomaly)) %>%
              dplyr::ungroup() %>%
              dplyr::select(Year, mean_daily_discharge_m3d1, mean_yearly_anomaly,
                            SD_yearly_discharge_m3d1, SE_yearly_discharge_m3d1) %>%
              dplyr::distinct() %>%
              dplyr::mutate(Sign = ifelse(mean_yearly_anomaly>0, "A", "B"))
              
# create monthly means  
FWD_mon_mn <- FWD_anomaly %>%
              dplyr::group_by(Year, Month) %>%
              dplyr::mutate(mean_monthly_discharge_m3d1 = mean(mean_daily_discharge_m3d1),
                            mean_monthly_anomaly = mean(daily_anomaly),
                            Year_Month = paste(Year, Month_number, sep="-")) %>%
              dplyr::ungroup() %>%
              dplyr::select(Year, Month, Year_Month, mean_monthly_discharge_m3d1, mean_monthly_anomaly) %>%
              dplyr::distinct() %>%
              dplyr::mutate(Sign = ifelse(mean_monthly_anomaly>0, "A", "B")) %>%
              dplyr::bind_rows(dummy_2018) # this adds dummy data for 2018 for plotting purposes

write_csv(FWD_ann_mn, "./data_clean/FWD_annual_mn_clean.csv")  
write_csv(FWD_mon_mn, "./data_clean/FWD_monthly_mn_clean.csv")





