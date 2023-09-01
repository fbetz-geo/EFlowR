#'Estimating annual volume from longterm discharge or vice versa
#'@description provides an estimate of annual water volume from average discharge or vice versa
#'This requires some caution as it is a rough estimation only when calculated based on average annual flow
#'@param discharge numeric value of the discharge in m3/s, only needed for estimating volume (the default)
#'@param volume numeric value of the annual water volume in km3, only needed for estimating annual average discharge
#'@param conversion either "to_volume" or "to_discharge", giving the direction of calculation
#'@note arguments need to specified explicitly, otherwise wrong estimations might be the consequence.
#'@author Florian Betz
#'@return numeric value giving the annual water volume or the annual average discharge
#'

convert<-function(discharge,volume,conversion="to_volume"){
  if (conversion=="to_volume") {
    return((discharge*60*60*24*365)/(1000*1000*1000))
  }

if (conversion=="to_discharge") {
  return((volume*1000*1000*1000)/(60*60*24*365))
}
}
