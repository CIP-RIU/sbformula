#'Quality control for new fieldbook traits.
#'
#'@param data The data frame of the fieldbook
#'@param genotipes The genotipes column name
#'@param trait The trait column name
#'@param lfactor The factor column name
#'@param ll lower limit of values
#'@param ul upper limit of values
#'@details This function return the values which are out of range. It does not requiere a data dictionary.
#'Make data qualit control for new variables using a lower limit and upper defined by users.
#'@author Omar Benites
#'@export

#Data Quality of Fieldbook using just lower limit and upper limit (without data dictionary)
sb_newqualityvar <- function(data,genotipes,trait,lfactor=NULL,ll,ul){
  
  datos<-data
  xvar <- trait
  lfactor <- lfactor
  genotipes <- genotipes 
  lowerl <- as.numeric(ll)
  upperl <- as.numeric(ul)
  
  datos[,genotipes] <- as.factor(datos[,genotipes])
  
  if(!is.null(lfactor)){
    datos[,lfactor] <-as.factor(datos[,lfactor]) 
  }
  
  if(is.null(lfactor)){
    select_fb <- dplyr::select_(datos,genotipes,xvar)
  }
  
  if(!is.null(lfactor)){
    select_fb <- dplyr::select_(datos,genotipes,xvar,lfactor) 
  }
  
  out_rule <- datos[,xvar]>=lowerl & datos[,xvar]<=upperl #the good rule, so we denied with !
  if(any(out_rule)){ #If any value of 'out_rule' is true (ie. has outliers)
    
    out_pos <- rownames(datos[!out_rule,])
    out_values <- datos[out_pos,c(genotipes,xvar)]
    #out_genotipes <- datos[out_pos,"INSTN"]
    
    out_print <- paste("Please validate the column '", xvar, " 'the row '",out_pos, " 'the '", genotipes, " is " ,out_values[,genotipes],
                       "' the value' ", round(out_values[,xvar],3), " .The range is from ",lowerl, " to ", upperl ,sep="")
    
    # lowerl <- 30
    # upperl <- 35
    
    #  oor_low <- dplyr::filter(select_fb, datos[,xvar] < lowerl) #values out of range - lower
    #  oor_up  <- dplyr::filter(select_fb, datos[,xvar] > upperl) #values out of range - upper  
    
    return(out_print) 
  }
  
  #out_print <- "There is no values out of range"
  #return(out_print)
}






