#'Formula for calculating the Total Tuber Yield No Adjusted (TTYNA)
#'
#'@param ttwp Total tuber weight/plot
#'@param pls Net plot size
#'@return Return the Total tuber yield no adjusted  ttyna
#'@author Omar Benites
#'@details Formula for calculating the Total tuber yield no adjusted
#'@references International Potato Center (CIP).2014: Procedures for Standard Evaluation and Data Management of Advanced Potato Clones
#'Module 2. Healthy Tuber Yield Trials International Cooperators Guide.
#'@keywords potato, agronomy,harvest,quantitative-continuous,yield,late blight,plant
#'@family potato,yield,late blight,harvest,plant.
#'@export 
#'
ttyna <- function(ttwp,pls){
  
  if (missing(ttwp)){
    #stop("Please enter the Total tuber weight/plot 'ttwp'")
    ttyna <- NA
  }
  if (missing(pls)){
    #stop("Please enter the net plot size ")
    ttyna <- NA
  }
  ttyna <- (ttwp/pls)*10
  return(ttyna)
} 
