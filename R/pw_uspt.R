#'Function for calculating the percentage weight loss of unsprouted tuber (PW_USPT)
#'@param itw Initial tuber weight
#'@param intw  Intermediate tuber weight
#'@return Return the percentage weight loss of unsprouted tuber
#'@details This function calculate the percentage weight loss of unsprouted tuber
#'@author Omar Benites
#'@references Procedures for standard evaluation and data management of advanced potato clones: Assessment of Dormancy and Sprouting
#'Behavior of Elite and Advanced Clones.International Potato Center (CIP).
#'@family Sprout-dormancy, evaluation, potato
#'@export

pw_uspt <- function(itw,intw){
  
  if(missing(itw)){
    stop("Please enter the initial tuber weight 'itw'")
  }
  if(missing(intw)){
    stop("Please enter the intermediate tuber weight 'intw'")
  }
  
  pw_uspt <- (apply(cbind(itw,-intw),1,sbsum)/itw)*100
  return(pw_uspt)
}
