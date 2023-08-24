#'Annual Proportional Flow Deviation
#'@description computes the annual proportional flow deviation of monthly stream flow data
#'according to Gehrke et al. (1995)
#'@param flow vector of (monthly) streamflow data
#'@param dates vector of same length as flow containing the dates associated with the flow values
#'the format is expected to be interpretable by lubridate::parse_date_time to convert to date format
#'@param date_format character vector defining the date-time format of dates see lubridate::parse_date_time for details
#'@param ref_start starting date of the reference period; has to be in the same format as date_format
#'@param ref_end end date of the reference period; has to be in the same format as date_format
#'@author Florian Betz
#'@references Gehrke et al. (1995):River regulation and fish communities in the murray‐darling river system,
#' Australia Regulated Rivers: Res. Manage. 11 363–75,
#' https://doi.org/10.1002/rrr.3450110310
#' @return numeric value of the flow alteration ranging from 0 (unmodified) to 3.46 (fully modified)
#'

apfd<-function(flow,dates,ref_start,ref_end,date_format){

  #create data.frame for subsequent analysis
  dates<-lubridate::parse_date_time(x = dates,orders=date_format)
  df<-data.frame(dates=dates,flow=flow,month=month(dates))

  #Create reference data and compute monthly averages of flow for the reference period
  df_ref<-df %>% filter(between(dates,lubridate::parse_date_time(ref_start,orders=date_format),
                                lubridate::parse_date_time(ref_end,orders=date_format))) %>%

    #group by month and get monthly average flow per month
    dplyr::group_by(month) %>%
    dplyr::summarize(mean_flow=mean(flow,na.rm=TRUE))

  #Get subset for analysis and compute monthly averages of flow for analysis period
  df_an<-df %>% dplyr::filter(dates>lubridate::parse_date_time(ref_end,orders=date_format)) %>%

    #group by month and get monthly average flow per month
    dplyr::group_by(month) %>%
    dplyr::summarize(mean_flow=mean(flow,na.rm=TRUE))

  #Compute flow alteration metric
  apfd<-sum((abs(df_an$mean_flow-df_ref$mean_flow))/df_ref$mean_flow)
  return(apfd)


}
