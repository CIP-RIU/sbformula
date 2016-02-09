#'Function for calculating the Percentage weight loss of sprouted tuber (PW_SPT)
#'
#'@param itw Initial tuber weight
#'@param ftw Final tuber weight
#'@return Return the percentage weight loss of sprouted tuber
#'@details This function returns the percentage weight loss of sprouted tuber
#'@author Omar Benites
#'@references Procedures for standard evaluation and data management of advanced potato clones: Assessment of Dormancy and Sprouting
#'Behavior of Elite and Advanced Clones.International Potato Center (CIP).
#'@family Sprout-dormancy, evaluation, potato
#'@export

pw_spt <- function(itw,ftw){
  
  if(missing(itw)){
    stop("Please enter the Initial tuber weight 'itw'")
  }
  
  if(missing(ftw)){
    stop("Please enter the Final tuber weight 'ftw'")
  }
  
  pw_spt = (apply(cbind(itw,-ftw),1,sbsum)/itw)*100
  return(pw_spt)  
}
