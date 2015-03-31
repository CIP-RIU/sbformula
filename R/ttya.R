#'Formula for calculating the Total Tuber Yield Adjusted (TTYA)
#'
#'@param ttwpl Total tuber weight/plant
#'@param plantden Plant density
#'@return Return the total tuber yield adjusted
#'@author Omar Benites
#'@details Formula for calculating the total tuber yield adjusted
#'@references International Potato Center (CIP).2014: Procedures for Standard Evaluation and Data Management of Advanced Potato Clones
#'Module 2. Healthy Tuber Yield Trials International Cooperators Guide.
#'@keywords potato, agronomy,harvest,quantitative-continuous,yield,late blight,plant
#'@family potato,yield,late blight,harvest,plant.
#'@export 
#'
ttya <- function(ttwpl,plantden){
  
  if (missing(ttwpl)){
    stop("Please enter the Total tuber weight/plot 'ttwpl'")
  }
  if (missing(plantden)){
    stop("Please enter the plant density ")
  }
  ttya <- (ttwpl*plantden)/1000
  return(ttya)
} 
