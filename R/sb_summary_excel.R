#' #'Function to calculate summary statistics for fieldbook data from excel files
#' #'@param data A data frame
#' #'@param idx The name or position of the measured variable that will be summarized.
#' #'@param groupfactors A vector containing names of columns that contain grouping variables
#' #'@param na.rm A boolean that indicates whether to ignore NA's
#' #'@param datadict The data frame of the data dictionary for potato and sweetpotato
#' #'@return A data frame with the count, mean and standard desviation
#' #'@author Omar Benites
#' #'@details This function is capable of divide the information in categorial or quantitative data based on a data dictionary for potato and sweepotato.
#' #'If it is categorical, returns the count #'and mode. And if it is quantitative, returns the count, media and standart desviation. 
#' #'@references Progress in developing a potato ontology for breeders. Reinhard Simon, Vilma Hualla, E. Salas, Rene Gomez, Raul Cordova and Stef de Haan.
#' #'Crop Ontology 2014.
#' #'@keywords stats, summary
#' #'@family stats,summary
#' #'@export  
#' 
#' sb_summary_excel <- function(data=NULL,idx, groupfactors=NULL, na.rm=FALSE, datadict=NULL) {
#'   #require(doBy)
#'   # New version of length which can handle NA's: if na.rm==T, don't count them
#'   if(missing(data)){
#'     stop("Please enter your data")
#'   }
#'   if(missing(idx)){
#'     stop("Please enter the name or position of the measured variable")
#'   }
#'   if(missing(groupfactors)){
#'     stop("Please enter the name of columns that contain grouping variables")
#'   }
#'   
#'   length2 <- function (x, na.rm=FALSE) {
#'     if (na.rm) sum(!is.na(x))
#'     else length(x)
#'   }
#'   
#'   datos <- data
#'   vvv <- datos[,idx]
#'   lbl <- names(datos[idx])   #extract the trait's label name
#'   measurevar <- lbl    #the trait's name
#'   tp <- as.character(datadict[datadict$ABBR==measurevar,c("TYPE")])    #the type of variable
#'   
#'   if(tp=="Continuous" || tp=="Discrete"){
#'   # filter continuous and discrete data
#'     formula <- as.formula(paste(measurevar, paste(groupfactors, collapse=" + "), sep=" ~ "))
#'     datac <- doBy::summaryBy(formula, data=data, FUN=c(length2,mean,sd), na.rm=na.rm)
#'     # Rename columns
#'     names(datac)[ names(datac) == paste(measurevar, ".length2", sep="") ] <- paste(measurevar,"_n",sep="")
#'     names(datac)[ names(datac) == paste(measurevar, ".mean",    sep="") ] <- paste(measurevar,"_Mean",sep="")  
#'     names(datac)[ names(datac) == paste(measurevar, ".sd",      sep="") ] <- paste(measurevar,"_sd",sep = "")
#'   }  #Cuantitativa
#'   
#'   if(tp=="Categorical"){
#'   #filter categorical data
#'     formula <- as.formula(paste(measurevar, paste(groupfactors, collapse=" + "), sep=" ~ "))
#'     datac <- doBy::summaryBy(formula, data=data, FUN=c(length2,themode)) #quit the na.rm
#'     names(datac)[ names(datac) == paste(measurevar, ".length2", sep="") ] <- paste(measurevar,"_n",sep="")
#'     names(datac)[ names(datac) == paste(measurevar, ".themode", sep="") ] <- paste(measurevar,"_Mode",sep="")                               
#'   }  #Cualitativa
#'   
#'   return(datac)
#' }
#' 
#' 
#' # fp <- file.choose()
#' # fp1 <- file.choose()
#' # datos <- xlsx::read.xlsx(fp1,sheetName = "Fieldbook")
#' # ddict <- openxlsx::read.xlsx(xlsxFile = fp,startRow = 6,sheet = "Template for submission",detectDates = TRUE) 
