#'Function to detect if a cipnumer (the passport data) is well written following the CIP syntax
#'
#'@param cipnumber The cipnumber data.  
#'@return A list of values which are correct cipnumber or not.
#'@details A cipnumber is the formal name for every genotipe or variety which has been developed and CIP centers.
#'The syntax is (CIP)(1 until 6 numbers)(dot)(1 until max 3 digits)
#'@author Omar Benites
#'@keywords cipnumber,   
#'@export

cip_number_check <- function(cipnumber){
  
  pattern <- "^(CIP)[0-9]{6}\\.([0-9]{1,3})$"
  check <- stringr::str_detect(cipnumber,pattern)
  cipnumber_ok <- cipnumber[check]
  cipnumber_wrong <- cipnumber[!check]
  out <- list(cipnumber_ok=cipnumber_ok, cipnumber_wrong=cipnumber_wrong)
  
}
