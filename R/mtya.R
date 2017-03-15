#'Formula for calculating the Marketable tuber yield adjusted (MTYA)
#'
#'@param mtwpl Marketable tuber weight/plant
#'@param plantden Plant density
#'@return Return the total tuber yield adjusted
#'@author Omar Benites
#'@details Formula for calculating marketable tuber yield adjusted
#'@references International Potato Center (CIP).2014: Procedures for Standard Evaluation and Data Management of Advanced Potato Clones
#'Module 2. Healthy Tuber Yield Trials International Cooperators Guide.
#'@keywords potato, agronomy,harvest,quantitative-continuous,yield,late blight,plant
#'@family potato,yield,late blight,harvest,plant
#'@export 
#'
mtya <- function(mtwpl,plantden){
  
  if (missing(mtwpl)){
    #stop("Please enter the Total tuber weight/plot 'mtwpl'")
    mtya <- NA
  }
  if (missing(plantden)){
    #stop("Please enter the plant density ")
    mtya <- NA
  }
  mtya <- (mtwpl*plantden)/1000
  return(mtya)
}
