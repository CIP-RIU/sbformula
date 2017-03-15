#'Function for calculating the Average Length of lateral sprout (AVLGLATSP)
#'
#'@param ... Length of the lateral sprout  
#'@return avlgatsp
#'@description This functions calculate the average length of lateral sprout 
#'@author Omar Benites
#'@references Procedures for standard evaluation and data management of advanced potato clones: Assessment of Dormancy and Sprouting
#'Behavior of Elite and Advanced Clones.International Potato Center (CIP).
#'@family Sprout-dormancy, evaluation, potato
#'@export

avlglatsp <- function(...){
  #avlglatsp  <-  apply(cbind(lglatsp1,lglatsp2,lglatsp3,lglatsp4,lglatsp5,lglatsp6),1,mean,na.rm=T)
  avlglatsp  <-  apply(cbind(...), 1, mean, na.rm=T)
  return(avlglatsp)
}
