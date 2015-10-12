#'Quality control for fieldbook traits 
#'
#'@param data The data frame of the fieldbook
# @param genotipes The genotipes column name
#'@param trait The trait column name
# @param lfactor The factor column name
#'@param datadict The data frame of the data dictionary
#'@return Return the values which are out of range using a data diccionary.
#'@details This function return the values which are out of range. It use a data dicctionary (based on Crop Ontology) to define the lower limit
#'and the upper limit for a quality control more precisely.
#'@author Omar Benites, Vilma Hualla.
#'@export
#'
# fp <- "Z:\\hidap\\inst\\hidap\\ontologies\\ontologies_potato.xlsx" 
# ddict <- readxl::read_excel(path = fp,sheet = "Template for submission",skip = 5)

# xvar="NTP"
# NTP <- iris$Sepal.Length
# pomar<- iris$Sepal.Width
# NTP <- c(NTP,NA,NA,NA,12,12,122)
# pomar <- c(pomar, 1, 1, 1, 1, 1, 1)
# #datos <- data.frame(col1,col2)
# datos <- data.frame(NTP,pomar)

sb_qualityvar <- function(data,trait,datadict) {
     
  if(is.null(data)){
    stop("Your data is empty. Please try again")
  }
  
  if(is.null(trait)){
    stop("Your enter the trait abreviation")
  }
  
  if(!(trait %in% names(data))){
    stop("The trait is not in data headers")
  }
  
  if(is.null(datadict)){
    stop("Your data dictionary is empty. Please try again")
  }
  
  #the type of variable
  trait <- as.character(trait)
  xvar <- trait
  datos <- data
  ddict <- datadict
  tp <- as.character(ddict[ddict$ABBR==xvar,c("TYPE")])
  #print(tp) #type of variable
    
  if(has.data(datos[,xvar])){  #begin has.data
  
  if(tp=="Continuous" || tp=="Discrete"){
    
    lowerl <- as.numeric(ddict[ddict$ABBR==xvar,c("LOWER")])
    upperl <- as.numeric(ddict[ddict$ABBR==xvar,c("UPPER")])
    #datos <- datos[complete.cases(datos),] #to skip the NA values
    #print(datos)
    out_rule <- !(datos[,xvar]>=lowerl & datos[,xvar]<=upperl) # if it's TRUE: out of range values; but it's FALSE: values in range
    #print(out_rule)
    
      if(any(out_rule)){ #If any value of 'out_rule' is true (ie. has out of range values)
        out_pos <- rownames(datos[out_rule,])
        out_values <- datos[out_pos,c(xvar)]
       
        out_print <- paste("Please validate the column '",xvar,"' in row '",out_pos,"' the value '",out_values,
                           "' is not between '",lowerl,"' and '",upperl, sep="")
        out_print <- out_print[!stringr::str_detect(out_print,"the value 'NA'")]
      
        #print(out_print)
        #print("paso")
      }
      if(all(!out_rule)){out_print <- paste(xvar,"ok",paste="")}
  }
  
  if(tp=="Categorical"){
    
    categoric_scale <- ddict[ddict$ABBR == xvar, c("CLASS1","CLASS2","CLASS3","CLASS4","CLASS5","CLASS6","CLASS7","CLASS8","CLASS9","CLASS10")]
    pattern <- "= .*$" #to delete all the patters after '=' symbol
    categoric_scale <- gsub(pattern = pattern, replacement = "", x = categoric_scale)
    #categoric_scale <- as.numeric(stringr::str_trim(categoric_scale[!is.na(categoric_scale)],side = "both"))
    
    categoric_scale <- suppressWarnings(as.numeric(categoric_scale))
    categoric_scale <- as.numeric(stringr::str_trim(categoric_scale[!is.na(categoric_scale)],side = "both"))
       
    if(length(categoric_scale)>0){
        out_scale <- !is.element(datos[,xvar],categoric_scale)
     
        if(any(out_scale)){
          
        out_pos <- rownames(datos[out_scale,])  #position of the values out of range
        out_values <- datos[out_pos,c(xvar)]
        #out_scale <- datos[out_pos,c("col1","col2")]
        #out_scale <- out_scale[complete.cases(out_scale),] #to remove NA values  
             
        out_print <- paste("Please validate the column '",xvar,"' that in row '",out_pos,"' the value '",out_values,
                           "' is not one of '", paste(categoric_scale,collapse=", "),"'", sep="")
        out_print <- out_print[!stringr::str_detect(out_print,"the value 'NA'")]
        
        #print(out_print)     
      }
        if(all(!out_scale)){out_print <- paste(xvar,"ok",paste="")}      
    }
  }
  out_print
  #print(out_print)
  
  
  } 
  
  #end has.data
}


# OLD CODE ==========
# sb_qualityvar <- function(data,genotipes,trait,lfactor=NULL,datadict){
#   
#   datos<-data
#   xvar <- trait
#   lfactor <- lfactor
#   genotipes <- genotipes 
#   datos[,genotipes] <- as.factor(datos[,genotipes])
#   
#   if(!is.null(lfactor)){
#     datos[,lfactor] <-as.factor(datos[,lfactor]) 
#   }
#   
#   if(is.null(lfactor)){
#     select_fb <- dplyr::select_(datos,genotipes,xvar)
#   }
#   
#   if(!is.null(lfactor)){
#     select_fb <- dplyr::select_(datos,genotipes,xvar,lfactor) 
#   }
#   
#   abbr <- datadict$ABBR
#   if(!(xvar %in% abbr)){
#     cat("We can not find the trait in the data dictionary")
#   }
#   
#   lowerl <- as.numeric(datadict[datadict$ABBR==xvar,c("LOWER")])
#   upperl <- as.numeric(datadict[datadict$ABBR==xvar,c("UPPER")])
#   #   
#   #   lowerl <- 30
#   #   upperl <- 35
#   
#   out_rule <- datos[,xvar]>=lowerl & datos[,xvar]<=upperl #the good rule, so we denied with !
#   if(any(out_rule)){ #If any value of 'out_rule' is true (ie. has outliers)
#     
#     out_pos <- rownames(datos[!out_rule,])
#     out_values <- datos[out_pos,c(genotipes,xvar)]
#     #out_genotipes <- datos[out_pos,"INSTN"]
#     
#     out_print <- paste("Please validate the column '", xvar, " 'the row '",out_pos, " 'the '", genotipes, " is " ,out_values[,genotipes],
#                        "' the value' ", round(out_values[,xvar],3), " .The range is from ",lowerl, " to ", upperl ,sep="")
#     
#     # lowerl <- 30
#     # upperl <- 35
#     
#     #  oor_low <- dplyr::filter(select_fb, datos[,xvar] < lowerl) #values out of range - lower
#     #  oor_up  <- dplyr::filter(select_fb, datos[,xvar] > upperl) #values out of range - upper  
#     
#     return(out_print) 
#   }
#   
#   #out_print <- "There is no values out of range"
#   #return(out_print)
# }
