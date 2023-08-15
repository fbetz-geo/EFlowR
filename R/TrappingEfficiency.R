#' Compute sediment trapping efficiency of reservoirs
#' @description Compute sediment trapping efficiency of reservoirs
#' @param capacity storage capacity of the reservoir in cubic kilometer (km3)
#' @param discharge annual discharge at the reservoir in km3;    
#' if given as annual average in m3 per second, it can be converted by setting is_m3_s to TRUE
#' @param drain_area drainage area of the catchment of the reservoir in m2; only used if method="brown"
#' @param is_m3_s logical; if FALSE, the function assumes discharge to be in km3;    
#' if TRUE, then discharge is assumend to be in m3 per second (e.g. annual average) and converted to km3
#' @param method which method to use for the computation; can be "brune", "brown" or "dendy"
#' @author Florian Betz
#' @references tba
#' @return numeric value giving the trapping efficiency from 0 (no sediment trapped) to 1    
#' (potentially all sediment is trapped)
#' 

te<-function(capacity,discharge,drain_area,is_m3_s=FALSE,method=c("brune","brown","dendy")){
  
  #convert discharge from m3/s to km3 per year if necessary
  if (is_m3_s) {
    discharge<-(discharge*365*24*60*60)/(1000*1000*1000)
  }
  
  #Brune method
  if (method=="brune") {
    te<-1-(0.05/(sqrt(capacity/discharge)))}
  
  #Brown method, not requiring discharge
  if (method=="brown") {
    
    #convert from km3 to m3
    capacity<-capacity*1000*1000*1000
    
    #Compute trapping efficiency
    te<-1-(1/(1+(0.00021*capacity/drain_area)))
  }
  
  #Dendy method
  if (method=="dendy") {
    te<-0.97^0.1^(log(capacity/discharge))
  }
  
  #return result
  return(te)
  }

