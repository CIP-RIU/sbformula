#'Boxplots for plant breeding
#'
#'@param data The fieldbook data
#'@param trait The column's name of the trait on fieldbook
#'@param genotipes The genotipes of the fieldbook
#'@param factors The column's name of the factor on fieldbook
#'@param full_name The full name of the trait
#'@param units The units of the trait (If it is a measure)
#'@return A boxplot chart and the list of outliers
#'@details A box chart implementation for plant breeding which allows to complete information about the trait. 
#'If data has a factor (ex. type of irrigation), the function catch the factor labels and plot them. Futhermore,
#'It returns a list of outliers.
#'@author Omar Benites
#'@family boxplot,summary,outliers
#'@export  

sboxcharts <- function(data,trait=NA,genotipes=NULL,factors=NULL,full_name=NULL,units=NULL){
  
  datos <- data
  geno <- as.character(genotipes)
  trait <- as.character(trait)
  
  if(is.null(factors)){
    datos <- datos[,c(geno,trait)]
  }
    
  if(!is.null(factors)){
    
    if(!(factor %in% names(datos))){
      stop("Your factor name is not in the dataset")
    }
    
    lfactor <- factors
    datos <-datos[,c(geno,lfactor,trait)] 
  }
  
  if(!(trait %in% names(datos))){
    stop("Your variable name is not in the dataset")
  }
  #datos <- datos[,trait]
  
  #fc = file.path(getwd(),"temp","charts")
  #if(!file.exists(fc)) dir.create(fc,rec=T)
  #unlink(file.path(fc, paste("*.*")))
  #print(str(ncol(data)))
  #k <- 1 # for statistical designs that do not requiere factor
#   if(!is.null(factors)){#if there is a split plot k=2, because the first column is FACTOR 
#     #datos <- datos[,c(geno,trait)]
#     datos <-  datos[,c(geno,trait,factor)]
#     #k <- 2
#   }
  
#  for (i in k:ncol(data)){
    #print(names(data)[i])
    #img = make_chart_name(data = data, i = i)
   # img = make_chart_name(traits = trait)
    #nmd = names(data)[i]
    #fulln = paste(dict[dict$ABBR==nmd,"VAR"]," (",nmd,")",sep="")
    #units = dict[dict$ABBR==nmd,"UNIT"]
    full_name <- as.character(full_name)
    units <- as.character(units)

    #png(filename=img, width=400, height=400)
    
    if(!is.factor(datos[,trait])){	
      
#     if(!is.null(factor)) {boxplot(datos[,trait] ~ lfactor,data = datos,main=paste("Boxplot of"),sub=full_name,ylab=units)}
#     if(is.null(factor))  {boxplot(datos[,trait], main = paste("Boxplot of"), sub=full_name, ylab=units) }
#       
#      if(!is.null(factor)) {bp <- car::Boxplot(datos[,trait] ~ lfactor, main= paste("Boxplot of"), sub= full_name, ylab=units,labels=datos$PLOT,Id.method=c("y"))}
#      if(is.null(factor))  {bp <- car::Boxplot(datos[,trait] , main= paste("Boxplot of"), sub= full_name, ylab=units,labels=DATA$num,Id.method=c("y"))}   
#       
      if(!is.null(factors)){bp <- with(datos, car::Boxplot(datos[,trait] ~ datos[,factors], main = ("Boxplot of ") , ylab = units, xlab = full_name ,labels = rownames(datos)))}
      if(is.null(factors)) {bp <- with(datos, car::Boxplot(datos[,trait] , main = ("Boxplot of ") , ylab=units, xlab=full_name ,labels = rownames(datos))) }
      
      if(!is.null(bp)){
        out_pos <- bp
        outliers <- datos[out_pos,]
        return(outliers)
        #datos %>% dplyr::select(.,locality,replication) %>% dplyr::filter(.,replication %in% c(3))          
      }
    
    } else {
      #	x = as.integer(as.character(data[,i]))
      xx = sort(table(datos[,trait]))
      cex = 1
      if(length(xx)>30) cex=0.7
      # 			fr= table(x)
      # 			xx = cbind(as.integer(names(fr)), as.numeric(fr))
      # 			rownames(xx) = xx[,1]
      # 			xx = xx[,-1]
      
      #desc = dict[dict$ABBR==nmd,"DESC"]
      
      dotchart(xx, main="Dotchart for",sub=full_name, xlab="Frequency [absolute]", ylab=units, cex=cex)
    }
    
    #devAskNewPage(ask=FALSE)
    #dev.off()
          
}

