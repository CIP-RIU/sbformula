#'Calculate the biochemical content for micro and macro nutrients 
#'  
#' @param biochem biochemical element
#' @param dm dry matter
#' @return Return the biochemichal content
#' @author Omar Benites
#' @details calculation of the biochemical content
#' @references Assessing potato clone for quality and nutrition
#' @family quality, nutrition, evaluation, potato
#' @export 
#

 biochem_cont <- function(biochem, dm){
  
  if(missing(biochem)){
    message("Please enter the Chlorophyll Content sample 1 'ChC1'")
  }
  if(missing(dm)){
    message("Please enter the Chlorophyll Content sample 2 'ChC2'")
  }
  
    bc <- biochem*dm/1000
  #eturn(bc)
  
} 