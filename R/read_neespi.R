#' Function to read data from the NEESPI database
#' @description
#' This function reads the csv files of the Central Asian hydro and climate sites downloaded from https://neespi.sr.unh.edu/maps and
#' converts them into a data.frame with all required information for further processing
#' @param file path to the csv file
#' @author Florian Betz
#' @return data.frame with three columns: "date" in format "Date" containing the date; "month": the month (e.g. for hydrograph computation); value": the actual value of the data series
#'

read_neespi<-function(file){

  #Read the raw file
  file<-read.csv(file = file,skip = 1,header = FALSE,sep="\t")
  df<-data.frame(date=readr::parse_date(substr(file$V1,1,7),format="%Y-%m")) %>% mutate(month=lubridate::month(date)) %>% mutate(value=file$V3)
  return(df)
}
