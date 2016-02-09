#'Function for calculating the Average Sprout thickness (AVTHSP)
#'
#'@param thlsp1 Thickness of lateral sprout Number
#'@param thlsp2 Thickness of lateral sprout Number
#'@return avthsp Return the average sprout thickness
#'@details This function calculates the average sprout thickness (Thickness of lateral sprout Number)
#'@description 45 days after recording dormancy period  (at storage temperature >15 celcius degrees) and 60 days after recording dormancy period  under cold storage (2-4 Celcius degrees). 
#'The date for recording this variable will differ among clones as it depends on the date dormancy period of a clone was recorded.  
#'Measure should be  performed in at most three sprouts per tuber in multiple sprouting tubers(include the longest one) Suggestion: use a Vernier calliper. An average is calculated per tuber
#'@author Omar Benites
#'@references Procedures for standard evaluation and data management of advanced potato clones: Assessment of Dormancy and Sprouting
#'Behavior of Elite and Advanced Clones.International Potato Center (CIP).
#'@family Sprout-dormancy, evaluation, potato
#'@export

avthsp <- function(thlsp1,thlsp2){
  
  avthsp  <-  apply(cbind(thlsp1,thlsp2),1,mean, na.rm=T)
  
  return(avthsp)
}
