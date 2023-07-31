
# Global definitions of the limits of seasons
springEquinox1999   <- as.Date("1999-03-21", format="%Y-%m-%d")
summerSolstice1999  <- as.Date("1999-06-21", format="%Y-%m-%d")
autumnalEquinox1999 <- as.Date("1999-09-23", format="%Y-%m-%d")
winterSolstice1999  <- as.Date("1999-12-21", format="%Y-%m-%d")
springEquinox2000   <- as.Date("2000-03-21", format="%Y-%m-%d")
summerSolstice2000  <- as.Date("2000-06-21", format="%Y-%m-%d")
autumnalEquinox2000 <- as.Date("2000-09-23", format="%Y-%m-%d")
winterSolstice2000  <- as.Date("2000-12-21", format="%Y-%m-%d")


is.na.oneCheck<- function(object) {
  # Return TRUE if the object is NA. 
  # In case of lists or vectors, it returns TRUE if they are empty or
  # all their elements are NA.
  #
  return (all(is.na(object)))
}

moveDatesTo2000<-function(dateVector) {
  # Change the year of each date so as to move them between [1999-03-21,2000-03-21)
  #
  require(lubridate)
  
  year(dateVector)<-2000
  for (i in 1:length(dateVector)) {
    if(dateVector[[i]]>=springEquinox2000) {
      year(dateVector[[i]])<-1999
    }
  }
  
  return(dateVector)
}

summariseDataset <- function(data,dateColumn,classColumn) {
  # Summarize dataset for logging purposes
  # (class balance status, number or rows and cols, etc.)
  #
  labelMinority = getLabelMinorityClass(data[[classColumn]])

  numMin <- sum(data[[classColumn]]==labelMinority)
  numMaj <- sum(data[[classColumn]]!=labelMinority)
  if (numMin==numMaj) {
    balStr <- paste0(" Class levels balanced (",numMin, " each)")
  } else {
    balStr <- paste0(" Class levels Maj:",numMaj," Min(",labelMinority,"):",numMin)
  }
  
  numCol <- ncol(data)
  numRow <- nrow(data)
  strDate <- "(No date column)"
  if (!is.null(dateColumn)) {
    dateFrom <- min(data[[dateColumn]])
    dateTo   <- max(data[[dateColumn]])
    strDate <- paste0(dateFrom," - ",dateTo)
  }
  return(paste0("[",strDate," nrow:",numRow, " ncol:",numCol," ",balStr,"]"))
}

getLabelMinorityClass<-function(vectorData) {
  # Assuming 'vectorData' to be vector of type factor and with only 2 labels,
  # it returns the label with less occurrences
  sumTable <- table(vectorData)
  return(rownames(sumTable)[which.min(sumTable)])
}

getLabelMajorityClass<-function(vectorData) {
  # Assuming 'vectorData' to be vector of type factor and with only 2 labels,
  # it returns the label with less occurrences
  sumTable <- table(vectorData)
  return(rownames(sumTable)[which.max(sumTable)])
}

doLog<- function(text, funcName = NA, logFile = NA) {
  # Store the text into a log file 
  #
  # Params:
  #
  #  text    : Text to write on the log
  #  funcName: Function from which the call is made. It will be added at the begining of the text 
  #  logFile : Name of the log file. It must be present ONLY in the first call to this function 
  #            (i.e. when the value is NA, it continues adding to the previous file. when it is not NA, a new file is created). 
  #
  new = FALSE
  if (!is.na(logFile)) {
    new = TRUE
    LogFile__ <<- logFile
  }
  
  if (!exists("LogFile__"))
    stop("No log file created previously. To do it, the function must be called with a value in 'logFile' first")
  
  if (is.na(funcName)) {
    funcName <- "[Main]"
  } else {
    funcName <- paste0("[",funcName,"]")
  }
  
  msg <-paste0(format(Sys.time(),"%d-%m-%y %H:%M:%S")," - ",funcName,": ",text)
  
  msg <- ifelse(grepl(".*\n$",msg), msg,paste0(msg,"\n"))
  
  cat(msg, file = LogFile__, append = !new)
}

logActive <- function() {
  # Returns TRUE if it founds an active log (see doLog function)
  return(exists("LogFile__"))
}



unbalancedSampling <- function(data, labelMinority, ratioToOne) {
  # Generates a vector with TRUE/FALSE for sampling according to "data" vector
  # It will perform a downsampling of the class indicated as 'minority' down to reach 
  # the desired ratio. All the elements from the majority class will be selected.
  #
  #
  #
  # data:          Vector with factors (i.e. usually the outcome variable)
  # labelMinority: Label of the factor that will be considered as the minority class
  # ratioToOne:    Final number of elements the majority class for each one of the minority class
  
  output      <- as.character(data)!=labelMinority
  nMajElem    <- sum(output)
  nMinElem    <- round(nMajElem/ratioToOne)
  allMinIndex <- which (as.character(data)==labelMinority)
  selIndex    <- sample(allMinIndex,nMinElem)
  output[selIndex] <- TRUE
  return(output)
  
}

getSeason<-function(dateVector) {
  require(lubridate)

  ws <- as.Date("2000-12-21", format="%Y-%m-%d") # Winter solstice
  se <- as.Date("2000-03-21", format="%Y-%m-%d") # Spring equinox
  ss <- as.Date("2000-06-21", format="%Y-%m-%d") # Summer solstice
  ae <- as.Date("2000-09-23", format="%Y-%m-%d") # Autumnal equinox
  l<-lapply(dateVector,function(f){
    # Convert dates from any year to 20000 It is a lap year, so no problem with 29-2
    d <- f
    year(d) <- 2000
    
    ifelse (d >= ws | d < se, return("WINTER"),
    ifelse (d >= se & d < ss, return("SPRING"), 
    ifelse (d >= ss & d < ae, return("SUMMER"),
                              return("AUTUMN"))))
  })
  lf <- unlist(l)
  lf <- factor(lf, levels = c("SPRING", "SUMMER", "AUTUMN", "WINTER"))
  return (lf)
}

getSeasonLevels <- function() {
  return (c("SPRING", "SUMMER", "AUTUMN", "WINTER"))
}

getLastLevelAlpha <- function(factorData) {
# Return the last level of a factor column when ordered alphabetically
  
 dataLevels<-sort(levels(factorData))
 return (dataLevels[length(dataLevels)])
}

dummify<-function(dataset, ignoreCols = c(), asIntegers = FALSE, reFactor = TRUE) {
  # Converts all the factor variables in the dataset into dummy variables
  #
  #
  # Args:
  #   dataset:    dataset to dummify
  #   ignoreCols: vector with the names of the cols to ignore (will be included without modifications )
  #   asIntegers: instead of TRUE/FALSE, sets to 1 or 0 the columns
  #   reFactor: Puede darse el caso de que alguna columna factor haya perdido todos los valores para una
  #             determinada etiqueta. Sobre todo puede ocurrir que pase de 3 valores a una de 2. Para esos casos,
  #             si esta opción es TRUE se le vuelve a hacer un "factor" a la columna para quitarle el caso que no valga
  # Returns:
  #   The dataset
  #
  # Returns a dataset with the factor variables dropped and converted into dummy vars

  df <- list()
  for (col in names(dataset)) {
    currCol <- dataset[[col]]
    if(is.factor(currCol) & !(col %in% ignoreCols)) { # Is a factor variable --> Dummify
      if (reFactor) {
        currCol <- factor(currCol)
      }
      currLevels<-levels(currCol)
      if (length(currLevels)==2) { # Only one variable needed
        newname<-paste0(col,currLevels[1])
        if (asIntegers) {
          newvalues<-ifelse(currCol == currLevels[1],1,0)
          df[[newname]] <- newvalues
        } else {
          newvalues<-currCol == currLevels[1]
          df[[newname]] <- as.logical(newvalues)
        }
      } else { # Many levels. One  variable per level
        for (l in levels(dataset[[col]])) {
          newname<- paste0(col,l)
          if (asIntegers) {
            newvalues<-ifelse(dataset[[col]] == l,1,0)
            df[[newname]] <- newvalues
          } else {
            newvalues<-dataset[[col]] == l
            df[[newname]] <- as.logical(newvalues)
          }

        }
      }
    } else if (asIntegers && is.logical(currCol)) {
      newvalues<-ifelse(currCol,1,0)
      df[[col]] <- newvalues
    } else { # IS NOT a factor variable --> include as is
      df[[col]] <- dataset[[col]]
    }
  }

  return(as.data.frame(df))
}
  
groupFactors <- function(factorColumn,  numPreservedGroups, label) {
  # Group the factors with less representation in factorColumn
  # numPreservedGroups: Number of levels to be preserved in the output column
  # label: Label of the extra factor that will group those with less representation
  #
  preservedLevels <- rownames(tail(sort(table(factorColumn)),numPreservedGroups))
  output <- as.character(factorColumn)
  output[!(output %in% preservedLevels)]<-label
  output <- factor(output)
  return (output)
  
}

factorizeAge <- function(ageVector) {
     l<-lapply(ageVector,function(age) {
       ifelse(                 age <   1,return("NEWBORN"),   
       ifelse(((age >=  1) && (age < 16)),return("PEDIATRIC"),
       ifelse(((age >= 16) && (age < 65)),return("ADULT"),
       ifelse((age >= 65),                return("ELDERLY"),
       return("UNKNOWN"))))) 
     })
     lf <- unlist(l)
     lf <- factor(lf, levels = c("NEWBORN", "PEDIATRIC", "ADULT", "ELDERLY"))
     return (lf)
   }
