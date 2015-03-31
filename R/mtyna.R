#'Formula for calculating the Marketable Tuber Yield No Adjusted (MTYNA)
#'
#'@param mtwp Marketable tuber weight/plot
#'@param pls Net plot size
#'@return Return the Total tuber yield no adjusted
#'@author Omar Benites
#'@details Formula for calculating the total tuber yield no adjusted
#'@references International Potato Center (CIP).2014: Procedures for Standard Evaluation and Data Management of Advanced Potato Clones
#'Module 2. Healthy Tuber Yield Trials International Cooperators Guide.
#'@keywords potato, agronomy,harvest,quantitative-continuous,plant
#'@family potato,harvest,plant
#'@export 
#'
mtyna <- function(mtwp,pls){
  
  if (missing(mtwp)){
    stop("Please enter the Total tuber weight/plot 'mtwp'")
  }
  if (missing(pls)){
    stop("Please enter the net plot size")
  }
  mtyna <-  (mtwp/pls)*10
  return(mtyna)
}
