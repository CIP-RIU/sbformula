#'Formula for calculating the Marketable Tuber Weight/Plant (MTWPL)
#'
#'@param mtwp Marketable tuber weight/plot
#'@param nph Number of plants harvested
#'@return Return the Marketable Tuber Weight/Plot
#'@author Omar Benites
#'@details Formula for calculating the Marketable tuber weight/plant
#'@references International Potato Center (CIP).2014: Procedures for Standard Evaluation and Data Management of Advanced Potato Clones
#'Module 2. Healthy Tuber Yield Trials International Cooperators Guide.
#'@keywords potato, agronomy,harvest,quantitative-continuous,yield,late blight,plant
#'@family potato,yield,late blight,harvest,plant.
#'@export 
#'
mtwpl <- function(mtwp,nph){
  
      if (missing(mtwp)){
        #stop("Please enter the Marketable tuber weight/plot 'mtwp'")
        mtwpl <- NA
      }
      if (missing(nph)){
        #stop("Please enter Number of plants harvested 'nph'")
        mtwpl <- NA
      }
      mtwpl  <-  mtwp/nph
      return(mtwpl)
} 
