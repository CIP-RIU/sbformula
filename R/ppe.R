#'Formula for calculating the Percentage plants emerged (PPE)
#'
#'This function allow to calculate the percentage plants emerged.
#'@param npe  Number of plants emerged
#'@param ntp  Number of tubers planted
#'@return A vector that contains the Percentage of Plants Emerged
#'@author Omar Benites 
#'@details Formula for calculating the Percentage plants emerged
#'@keywords potato, agronomy,phenology,quantitative-continuous,percentage,any
#'@references International Potato Center (CIP).2014: Procedures for Standard Evaluation and Data Management of Advanced Potato Clones
#'Module 2. Healthy Tuber Yield Trials International Cooperators Guide.
#'@export

ppe <- function(npe,ntp){
  
  if (missing(npe)){
    #stop("Please enter the Number of plants emerged 'npe'")
    ppe <- NA
  }
  if (missing(ntp)){
    #stop("Please enter the Number of tubers planted 'ntp'")
    ppe <- NA
  }
  ppe  <- (npe*100)/ntp
  return(ppe)
} 
