#'Formula for calculating the Percentage of Storage Root Dry Matter Content (DM) 
#'
#'@param dmf Fresh weight of roots for dry matter assessment
#'@param dmd Dry weight of dmf samples
#'@return Return the percentage of storage root dry matter content 'dm'
#'@author Omar Benites
#'@details Formula for calculating the percentage of storage root dry matter content
#'@keywords sweetpotato, agronomy,harvest,quantitative-continuous,yield
#'@family sweetpotato,quality,harvest
#'@export 
#'
dm <- function(dmd,dmf){
            if(missing(dmf)){
              stop("Fresh weight of roots for dry matter assessment 'dmf'")
            }
            if(missing(dmd)){
              stop("Please enter dry weight of dmf samples 'dmd'")
            }
            dm <- (dmd/dmf)*100 
            return(dm)
}
