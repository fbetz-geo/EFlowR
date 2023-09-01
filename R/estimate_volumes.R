#'Estimate yearly water volumes from (monthly) discharge time series
#'@description This function allows to estimate the annual volume at a gauge from the discharge time series. Currently, this function
#'assumes monthly discharge time series, but daily time series will be supported soon.
#'@param discharge vector with (monthly) discharge values in m3/s
#'@param date character vector indicating the dates associated with discharge
#'@param date_format character defining the format of the date vector. See lubridate::parse_date_time for details
#'@author Florian Betz
#'@return data.frame of two colums, the year and the associated volume estimation
#'
estimate_volume<-function(discharge,date,date_format){
  df<-data.frame(dis=discharge,date=lubridate::parse_date_time(date,orders = date_format)) %>%

    #estimate the number of days per month and the year as separate colums
    mutate(days_per_month=lubridate::days_in_month(date),year=lubridate::year(date)) %>%

    #estimate monthly volumes
    mutate(monthly_volume=(days_per_month*24*60*60*discharge)/(1000*1000*1000)) %>%

    #group the data.frame by year and summarize for estimating the volume
    group_by(year) %>% summarize(volume=sum(monthly_volume))
}
