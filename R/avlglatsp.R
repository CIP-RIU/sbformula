#'Function for calculating the Average Length of lateral sprout (AVLGLATSP)
#'
#'@param lglatsp1 Length of the lateral sprout Number 1
#'@param lglatsp2 Length of the lateral sprout Number 2
#'@param lglatsp3 Length of the lateral sprout Number 3
#'@param lglatsp4 Length of the lateral sprout Number 4
#'@param lglatsp5 Length of the lateral sprout Number 5
#'@param lglatsp6 Length of the lateral sprout Number 6
#'@return avlgatsp
#'@description This functions calculate the average length of lateral sprout 
#'@author Omar Benites
#'@references Procedures for standard evaluation and data management of advanced potato clones: Assessment of Dormancy and Sprouting
#'Behavior of Elite and Advanced Clones.International Potato Center (CIP).
#'@family Sprout-dormancy, evaluation, potato
#'@export

avlglatsp <- function(lglatsp1,lglatsp2,lglatsp3,lglatsp4,lglatsp5,lglatsp6){
  avlglatsp  <-  apply(cbind(lglatsp1,lglatsp2,lglatsp3,lglatsp4,lglatsp5,lglatsp6),1,mean,na.rm=T)
  return(avlglatsp)
}
