#'Formula for calculating the Average Marketable Tuber Weight (ATMW)
#'
#'@param mtwp marketable tuber weight/plot
#'@param nmtp number marketable tubers/plot
#'@return Return the average marketable tuber weight
#'@author Omar Benites
#'@details Formula for calculating the average marketable tuber weight
#'@references International Potato Center (CIP).2014: Procedures for Standard Evaluation and Data Management of Advanced Potato Clones
#'Module 2. Healthy Tuber Yield Trials International Cooperators Guide.
#'@keywords potato, agronomy,harvest,quantitative-continuous,yield,late blight,plant
#'@family potato,yield,late blight,harvest,plant
#'@export 
#'
atmw <- function(mtwp,nmtp){
  
  if (missing(mtwp)){
    stop("Please enter the marketable tuber weight/plot 'mtwp'")
  }
  if (missing(nmtp)){
    stop("Please enter the number marketable tubers/plot 'nmtp'")
  }
  atmw <- (mtwp/nmtp)*1000
  return(atmw)
}
