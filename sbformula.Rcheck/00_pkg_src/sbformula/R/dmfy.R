#'Formula for calculating the Dry matter foliage yield (DMFY)
#'
#'@param vw Vine weight
#'@param pls net plot size
#'@param dmvd Dry weight of DMVD samples
#'@param dmvf Fresh weight vines for dry matter assessment
#'@return Return the root foliage ratio 'dmfy'
#'@author Omar Benites
#'@details Formula for calculating dry matter foliage yield
#'@keywords sweetpotato, agronomy,harvest,quantitative-continuous,yield
#'@family sweetpotato,quality,harvest
#'@export 
#'
dmfy <- function(vw,pls,dmvd,dmvf){

              if(missing(pls)){
                stop("Please enter 'vw'")
              }
              if(missing(vw)){
                stop("Please enter 'pls'")
              }
              if(missing(dmvd)){
                stop("Please enter 'dmvd'")
              }
              if(missing(dmvf)){
                stop("Please enter 'dmvf'")
              }              
              dmfy <- (vw*10/pls)*dmvd/dmvf
              return(dmfy)
}




