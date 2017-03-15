#'Formula for calculating the Total tuber weight/plant (TTWPL)
#'
#'@param ttwp Total tuber weight/plot
#'@param nph Number of plants harvested
#'@return Return the 
#'@author Omar Benites
#'@details Formula for calculating the Total tuber weight/plant
#'@references International Potato Center (CIP).2014: Procedures for Standard Evaluation and Data Management of Advanced Potato Clones
#'Module 2. Healthy Tuber Yield Trials International Cooperators Guide.
#'@keywords potato, agronomy,harvest,quantitative-continuous,yield,late blight,plot
#'@family potato,yield,late blight,harvest,plot.
#'@export 
#'
ttwpl <- function(ttwp,nph){
  
  if (missing(ttwp)){
    #stop("Please enter the Total tuber weight/plot 'ttwp'")
    ttwpl <- NA
  }
  if (missing(nph)){
    #stop("Please enter Number of plants harvested 'nph'")
    ttwpl <- NA
  }
  ttwpl <- ttwp/nph
  return(ttwpl)
} 
