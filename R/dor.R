#'Degree of Regulation
#'@description computes the degree of regulation (Lehner et al. 2011) using
#'reservoir capacities and annual streamflow as input
#'@param cap reservoir capacity in cubic kilometer (km3)
#'@param flow annual inflow into the reservoir (km3)
#'@author Florian Betz
#'@references Lehner et al. (2011): High-resolution mapping of the world's reservoirs
#'and dams for sustainable river-flow management Front. Ecol. Environ. 9 494–502,
#'https://doi.org/10.1890/100125
#'@return numeric value with the degree of regulation

dor<-function(cap, flow)
{
  return(cap/flow)
}
