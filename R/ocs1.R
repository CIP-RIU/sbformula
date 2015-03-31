#'Formula for calculating the Oil Content Sample 1 Percentage (OCS1)
#'
#'@param iws1 Initial weight sample 1
#'@param fws1 Fresh weight of tuber sample 1
#'@return Return the Oil content sample 1 percentage
#'@author Omar Benites
#'@details Formula for calculating the Oil content sample 1 percentage
#'@references International Potato Center (CIP).2014: Procedures for Standard Evaluation and Data Management of Advanced Potato Clones
#'Module 2. Healthy Tuber Yield Trials International Cooperators Guide.
#'@keywords potato, agronomy,post-harvest,quantitative-continuous,yield,late blight,plant
#'@family potato,yield,late blight,post-harvest,plant
#'@export 
#'
ocs1 <- function(iws1,fws1){
  
  if (missing(iws1)){
    stop("Please enter the Initial weight sample 1 'iws1'")
  }
  if (missing(fws1)){
    stop("Please enter the Fresh weight of tuber sample 1 'fws1'")
  }
  ocs1  = 100 - ((iws1/fws1)*100)
  return(ocs1)
  
}