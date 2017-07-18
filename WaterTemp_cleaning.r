##################################################
##### Water Temperature Data 
##### from CO-OPS Tides and Currents website
##### Script by Rachael Blake, July 17, 2017
##################################################

library(httr) ; library(dplyr) 

### Have to download data one year at a time per the CO-OPS online download restrictions!  :-P


# Function to read in the 19 years of water temp data from Seldovia, AK - Station ID: 9455500
  
WaterTempDat <- function(data_url){
                dataGet <- GET(data_url)
                data1 <- content(dataGet, as='text')
                
                df <- read.table(file=textConnection(data1), sep=",", fill=TRUE,
                                 stringsAsFactors=FALSE, header=TRUE)
                
                df1 <- df %>%
                       select(-X,-N,-R) %>%
                       dplyr::rename(Date_Time = Date.Time,
                                     Water_Temp_C = Water.Temperature) %>%
                       dplyr::filter(!is.na(Water_Temp_C)) %>%
                       dplyr::mutate(Date = sapply(strsplit(as.character(Date_Time), split=" ") , function(x) x[1]),
                                     Year = sapply(strsplit(as.character(Date), split="-") , function(x) x[1]),
                                     Month = sapply(strsplit(as.character(Date), split="-") , function(x) x[2])) %>%
                       dplyr::mutate_at(c(5), funs(as.numeric)) %>% 
                       dplyr::group_by(Year, Month) %>%
                       dplyr::mutate(Water_Temp_Monthly = mean(Water_Temp_C, na.rm = T)) %>%
                       dplyr::ungroup() %>%
                       dplyr::group_by(Year) %>%
                       dplyr::mutate(Water_Temp_Yearly = mean(Water_Temp_C, na.rm = T)) %>%
                       dplyr::ungroup() %>%
                       dplyr::select(Year, Month, Water_Temp_Monthly, Water_Temp_Yearly) %>%
                       dplyr::distinct()
                
                return(df1)
                
                }
 

#######


URLS <- list("https://tidesandcurrents.noaa.gov/api/datagetter?begin_date=19990101%2000:00&end_date=19991231%2023:00&station=9455500&product=water_temperature&datum=mllw&units=metric&time_zone=lst_ldt&interval=h&application=web_services&format=csv",
             "https://tidesandcurrents.noaa.gov/api/datagetter?begin_date=20000101%2000:00&end_date=20001231%2023:00&station=9455500&product=water_temperature&datum=mllw&units=metric&time_zone=lst_ldt&interval=h&application=web_services&format=csv",
             "https://tidesandcurrents.noaa.gov/api/datagetter?begin_date=20010101%2000:00&end_date=20011231%2023:00&station=9455500&product=water_temperature&datum=mllw&units=metric&time_zone=lst_ldt&interval=h&application=web_services&format=csv",
             "https://tidesandcurrents.noaa.gov/api/datagetter?begin_date=20020101%2000:00&end_date=20021231%2023:00&station=9455500&product=water_temperature&datum=mllw&units=metric&time_zone=lst_ldt&interval=h&application=web_services&format=csv",
             "https://tidesandcurrents.noaa.gov/api/datagetter?begin_date=20030101%2000:00&end_date=20031231%2023:00&station=9455500&product=water_temperature&datum=mllw&units=metric&time_zone=lst_ldt&interval=h&application=web_services&format=csv",
             "https://tidesandcurrents.noaa.gov/api/datagetter?begin_date=20040101%2000:00&end_date=20041231%2023:00&station=9455500&product=water_temperature&datum=mllw&units=metric&time_zone=lst_ldt&interval=h&application=web_services&format=csv",
             "https://tidesandcurrents.noaa.gov/api/datagetter?begin_date=20050101%2000:00&end_date=20051231%2023:00&station=9455500&product=water_temperature&datum=mllw&units=metric&time_zone=lst_ldt&interval=h&application=web_services&format=csv",
             "https://tidesandcurrents.noaa.gov/api/datagetter?begin_date=20060101%2000:00&end_date=20061231%2023:00&station=9455500&product=water_temperature&datum=mllw&units=metric&time_zone=lst_ldt&interval=h&application=web_services&format=csv",
             "https://tidesandcurrents.noaa.gov/api/datagetter?begin_date=20070101%2000:00&end_date=20071231%2023:00&station=9455500&product=water_temperature&datum=mllw&units=metric&time_zone=lst_ldt&interval=h&application=web_services&format=csv",
             "https://tidesandcurrents.noaa.gov/api/datagetter?begin_date=20080101%2000:00&end_date=20081231%2023:00&station=9455500&product=water_temperature&datum=mllw&units=metric&time_zone=lst_ldt&interval=h&application=web_services&format=csv",
             "https://tidesandcurrents.noaa.gov/api/datagetter?begin_date=20090101%2000:00&end_date=20091231%2023:00&station=9455500&product=water_temperature&datum=mllw&units=metric&time_zone=lst_ldt&interval=h&application=web_services&format=csv",
             "https://tidesandcurrents.noaa.gov/api/datagetter?begin_date=20100101%2000:00&end_date=20101231%2023:00&station=9455500&product=water_temperature&datum=mllw&units=metric&time_zone=lst_ldt&interval=h&application=web_services&format=csv",
             "https://tidesandcurrents.noaa.gov/api/datagetter?begin_date=20110101%2000:00&end_date=20111231%2023:00&station=9455500&product=water_temperature&datum=mllw&units=metric&time_zone=lst_ldt&interval=h&application=web_services&format=csv",
             "https://tidesandcurrents.noaa.gov/api/datagetter?begin_date=20120101%2000:00&end_date=20121231%2023:00&station=9455500&product=water_temperature&datum=mllw&units=metric&time_zone=lst_ldt&interval=h&application=web_services&format=csv",
             "https://tidesandcurrents.noaa.gov/api/datagetter?begin_date=20130101%2000:00&end_date=20131231%2023:00&station=9455500&product=water_temperature&datum=mllw&units=metric&time_zone=lst_ldt&interval=h&application=web_services&format=csv",
             "https://tidesandcurrents.noaa.gov/api/datagetter?begin_date=20140101%2000:00&end_date=20141231%2023:00&station=9455500&product=water_temperature&datum=mllw&units=metric&time_zone=lst_ldt&interval=h&application=web_services&format=csv",
             "https://tidesandcurrents.noaa.gov/api/datagetter?begin_date=20150101%2000:00&end_date=20151231%2023:00&station=9455500&product=water_temperature&datum=mllw&units=metric&time_zone=lst_ldt&interval=h&application=web_services&format=csv",
             "https://tidesandcurrents.noaa.gov/api/datagetter?begin_date=20160101%2000:00&end_date=20161231%2023:00&station=9455500&product=water_temperature&datum=mllw&units=metric&time_zone=lst_ldt&interval=h&application=web_services&format=csv",
             "https://tidesandcurrents.noaa.gov/api/datagetter?begin_date=20170101%2000:00&end_date=20170630%2023:00&station=9455500&product=water_temperature&datum=mllw&units=metric&time_zone=lst_ldt&interval=h&application=web_services&format=csv"
             )


WT_df_list <- lapply(URLS, FUN=WaterTempDat) # for every element of the list of URLs run my function

WTemp_all <- bind_rows(WT_df_list) # bind the list of dataframes output by lapply() into one large dataframe

WTemp_Yr <- WTemp_all %>%
            dplyr::select(-Month, -Water_Temp_Monthly) %>%
            distinct()

WTemp_June <- WTemp_all %>%
              dplyr::select(-Water_Temp_Yearly) %>%
              dplyr::filter(Month %in% c(6)) %>%
              dplyr::rename(Water_Temp_June = Water_Temp_Monthly) %>%
              dplyr::select(-Month)

WTemp_Dec <- WTemp_all %>%
             dplyr::select(-Water_Temp_Yearly) %>%
             dplyr::filter(Month %in% c(12)) %>%
             dplyr::rename(Water_Temp_Dec = Water_Temp_Monthly) %>%
             dplyr::select(-Month) %>% 
             dplyr::mutate(WTemp_Dec_Lag = lag(Water_Temp_Dec))
  
  




  
  
  
  
  


