#'Formula for calculating the Percent Marketable Roots - Commercial Index (CI)
#'
#'@param nocr Number of commercial roots
#'@param nonc Number of non commercial roots
#'@return Return percent marketable roots - commercial index 'ci'
#'@author Omar Benites
#'@details Formula for calculating the percent marketable roots - commercial index
#'@keywords sweetpotato, agronomy,harvest,quantitative-continuous,yield
#'@family sweetpotato,quality,harvest
#'@export 
#'
ci<- function(nocr,nonc){
  if(missing(nocr)){
    stop("Please enter number of commercial roots 'nocr'")
  }
  ci <- (nocr/apply(cbind(nocr,nonc), 1, sbsum))*100
  return(ci)
}
