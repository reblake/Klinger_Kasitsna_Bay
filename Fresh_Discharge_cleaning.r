#################################################################
#####  Terrie Klinger's Kasitsna Bay Data                   #####
#####  Hill mode freshwater discharge data cleaning script  #####
#####  by Rachael E. Blake                                  #####
#####  17 May 2017                                          #####
#################################################################

### load necessary packages
library(ncdf4) ; library(chron)

### read in the netcdf file and parse and clean the parts
nc_kbay <- nc_open("GOA_RUNOFF_DISCHARGE.ncml.nc")
#print(nc_kbay)

# get time 
time_date <- ncvar_get(nc_kbay, "time")
time_date_units <- ncatt_get(nc_kbay, "time", "units")

# get discharge
mean_daily_discharge_m3s1 <- ncvar_get(nc_kbay, "q")
discharge_lname <- ncatt_get(nc_kbay, "q", "long_name")
discharge_units <- ncatt_get(nc_kbay, "q", "units")
discharge_fillvalue <- ncatt_get(nc_kbay, "q", "_FillValue")

# get latitude
latitude_deg_north <- ncvar_get(nc_kbay, "lat")
lat_lname <- ncatt_get(nc_kbay, "lat", "long_name")
lat_units <- ncatt_get(nc_kbay, "lat", "units")

# get longitude
longitude_deg_east <- ncvar_get(nc_kbay, "lon")
lon_lname <- ncatt_get(nc_kbay, "lon", "long_name")
lon_units <- ncatt_get(nc_kbay, "lon", "units")

# get general attributes
title <- ncatt_get(nc_kbay, 0, "title")
summary <- ncatt_get(nc_kbay, 0, "summary")
institution <- ncatt_get(nc_kbay, 0, "institution")
history <- ncatt_get(nc_kbay, 0, "history")
Conventions <- ncatt_get(nc_kbay, 0, "Conventions")

# close the connection to the netcdf file
nc_close(nc_kbay)

# convert time variable to human-readable format
time_unit_str <- strsplit(time_date_units$value, " ")
date_str <- strsplit(unlist(time_unit_str)[3], "-")
start_month <- as.integer(unlist(date_str)[2])
start_day <- as.integer(unlist(date_str)[3])
start_year <- as.integer(unlist(date_str)[1]) 

# make date column   # NOTE Each array slice is a date!!
date <- chron(time_date, origin=c(start_month, start_day, start_year), out.format="m/d/year")

# replace FillValue with NA
mean_daily_discharge_m3s1[is.nan(mean_daily_discharge_m3s1)] <- NA

### make a lat lon dataframe
latlon <- as.data.frame(matrix(ncol=0, nrow=108))
latlon$latitude_deg_north <- as.vector(latitude_deg_north)
latlon$longitude_deg_east <- as.vector(longitude_deg_east)

### Function to create a giant dataframe from slices of the netcdf file
#' create a giant dataframe from slices of the netcdf file
#'
#' @param slice 
#'
#' @return
#' @export
#'
#' @examples

slice_2_df <- function(slice){
              # make empty dataframe
              df1 <- as.data.frame(matrix(ncol=0, nrow=108))
              # make date column
              df1$date <- as.character(date[[slice]])
              # make data column
              df1$mean_daily_discharge_m3s1 <- as.vector(mean_daily_discharge_m3s1[,,slice])
  
              lldf1 <- cbind(latlon, df1)
              lldf1 <- dplyr::filter(lldf1, !is.na(mean_daily_discharge_m3s1))
  
              return(lldf1)
}
###

num_slices <- c(1:5722)   # this is the known nmber of slices in the original array

FWD_list <- lapply(num_slices, function(x) slice_2_df(x))

FWD <- bind_rows(FWD_list)

# remove some points that probably don't drain into this site
FWD_less <- FWD %>% 
            filter(!(longitude_deg_east < -151.55 & latitude_deg_north < 59.48),
                   !(latitude_deg_north > 59.51 & longitude_deg_east > -151.45))

# check to see the data are approx correct by plotting the lats and lons
q <- qplot(data=FWD_less, x=longitude_deg_east, y=latitude_deg_north)

# create annual means
FWD_anomaly <- FWD_less %>%
               dplyr::mutate(Year = sapply(strsplit(as.character(date), split="/") , function(x) x[3]),
                             Month = sapply(strsplit(as.character(date), split="/") , function(x) x[1]),
                             Day = sapply(strsplit(as.character(date), split="/") , function(x) x[2])) %>%
               dplyr::mutate(mean_overall = mean(mean_daily_discharge_m3s1),
                             daily_anomaly = mean_daily_discharge_m3s1 - mean_overall)

FWD_ann_mn <- FWD_anomaly %>%
              dplyr::group_by(Year) %>%
              dplyr::mutate(mean_yearly_discharge_m3s1 = mean(mean_daily_discharge_m3s1),
                            mean_yearly_anomaly = mean(daily_anomaly)) %>%
              dplyr::ungroup() %>%
              dplyr::select(Year, mean_yearly_discharge_m3s1, mean_yearly_anomaly) %>%
              dplyr::distinct() %>%
              dplyr::mutate(Sign = ifelse(mean_yearly_anomaly>0, "A", "B"))
              
# create monthly means  
FWD_mon_mn <- FWD_anomaly %>%
              dplyr::group_by(Year, Month) %>%
              dplyr::mutate(mean_monthly_discharge_m3s1 = mean(mean_daily_discharge_m3s1),
                            mean_monthly_anomaly = mean(daily_anomaly),
                            Year_Month = paste(Year, Month, sep=".")) %>%
              dplyr::ungroup() %>%
              dplyr::select(Year, Month, Year_Month, mean_monthly_discharge_m3s1, mean_monthly_anomaly) %>%
              dplyr::distinct() %>%
              dplyr::mutate(Sign = ifelse(mean_monthly_anomaly>0, "A", "B"))
  





