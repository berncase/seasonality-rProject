##
# Functions related with the preprocessing process (filters, balancing, etc.)
#
#
# Requires
# sourcing of "functions/utils.r"

#//////////////////////////////////////////////////////////////////////////
# BALANCING ---------------------------------------------------------------
#//////////////////////////////////////////////////////////////////////////

undersample <- function(data, classColumn, ratio = 1) {
  # Undersamples the dataset in order to remove the imbalance between classes 
  #
  # 
  #
  # Args: 
  #   data:    original dataset
  #   classColumn: Column with the class
  #   ratio: ratio mayority Class/minority class to use. 
  #     1 = 50% (perfect balance), 2 -> there are gone to be 2 samples of 
  #     the mayority class per 1 of the minority, etc. 
  #     If ratio == NULL | 0, no balance is performed 
  #
  # Returns:
  #   The new balanced dataset 
  
  classes = levels(as.factor(data[[classColumn]]))
  class1label = classes[1]
  class2label = classes[2]  
  
  dataClass1 = data[data[[classColumn]]==class1label,]
  dataClass2 = data[data[[classColumn]]==class2label,]
  
  if (nrow(dataClass1)<=nrow(dataClass2)) {
    minorityClassData <- dataClass1
    mayorityClassData <- dataClass2
  } else {
    minorityClassData <- dataClass2
    mayorityClassData <- dataClass1
  }
  
  if (!(ratio==0 || (nrow(mayorityClassData)/nrow(minorityClassData))<ratio)) {
    numSamples = nrow(minorityClassData)*ratio
    
    mayoritySampledData <-mayorityClassData[ sample(nrow(mayorityClassData),numSamples, replace = FALSE),]
  } else {
    mayoritySampledData <- mayorityClassData
  }
  
  result<-as.data.frame(rbind(minorityClassData,mayoritySampledData))
  
  return ( result )
}

oversample <- function(data, classColumn, ratio = 1) {
  # Oversamples the dataset in order to remove the imbalance between classes 
  #
  # Args: 
  #   data:    original dataset
  #   classColumn: Column with the class
  #   ratio: ratio mayority Class/minority class to use. 
  #     1 = 50% (perfect balance), 2 -> there are gone to be 2 samples of 
  #     the mayority class per 1 of the minority, etc. 
  #     If ratio == NULL | 0, no balance is performed (useful if only cleaning is
  #     required :-) 
  #   cleanUniqueCols: remove cols that remain with only 1 value
  #
  # Returns:
  #   The new balanced dataset 
  
  classes = levels(as.factor(data[[classColumn]]))
  class1label = classes[1]
  class2label = classes[2]  
  
  dataClass1 = data[data[[classColumn]]==class1label,]
  dataClass2 = data[data[[classColumn]]==class2label,]
  
  if (nrow(dataClass1)<=nrow(dataClass2)) {
    minorityClassData <- dataClass1
    mayorityClassData <- dataClass2
  } else {
    minorityClassData <- dataClass2
    mayorityClassData <- dataClass1
  }
  
  if (!(ratio==0 || (nrow(mayorityClassData)/nrow(minorityClassData))<ratio)) {
    numSamples = round(nrow(mayorityClassData)/ratio) - nrow(minorityClassData)
    
    minoritySampledData <-minorityClassData
    
    minoritySampledData <-rbind(minoritySampledData, minorityClassData[ sample(nrow(minorityClassData),numSamples, replace = TRUE),])
  } else {
    minoritySampledData <- minorityClassData
  }
  
  result<-as.data.frame(rbind(minoritySampledData,mayorityClassData))
  
  return ( result )
}

balance <- function(data,classColumn, method)   {
  ### 
  # Apply the balancing strategy indicated in 'method' to the dataset
  # Returns the dataset already balanced
  #
  switch(method, 
     none= {
       return(data)
     },
     under1={
       return(
         undersample(
           data = data, 
           classColumn = classColumn,
           ratio = 1)
       )
     },
     under2={
       return(
         undersample(
           data = data, 
           classColumn = classColumn,
           ratio = 2)
       )
     },
     
     over1={
       return(
         oversample(
           data = data, 
           classColumn = classColumn,
           ratio = 1)
       )
       
     },
     over2={
       return(
         oversample(
           data = data, 
           classColumn = classColumn,
           ratio = 2)
       )
       
     },{
       msg <- paste0("Balancing method '",method,"' unknown")
       if (logActive()) doLog(msg,funcName = "balance")
       stop(msg)
     }
  )
}


#//////////////////////////////////////////////////////////////////////////
# FILTERING ---------------------------------------------------------------
#//////////////////////////////////////////////////////////////////////////


applyFilterPvalue <- function(data, classColumn, pvalueThreshold) {
  ### Apply Fisher test to nominal/boolean columns or the t-test to numerical columns
  # If the obtained p-value is less than 'pvalueThreshold' then the column is maintained in the output dataset
  # 
  # Note: data[[classColumn]] MUST BE A FACTOR
  #
  columns <- setdiff(colnames(data),c(classColumn))
  levelNames <- levels(data[[classColumn]])
  tests <-  lapply(columns,function(col) { 
    if (length(unique(data[[col]]))==1) { 
      return(0.0) 
    } else if (is.factor(data[[col]]) || is.logical(data[[col]])) { 
      ft <- fisher.test(x=data[[col]], y = data[[classColumn]], conf.int = FALSE)
      return(ft$p.value)
    } else if (is.numeric(data[[col]])) {
      set1 <- data[[col]][data[[classColumn]]==levelNames[1]]
      set2 <- data[[col]][data[[classColumn]]==levelNames[2]]
      tt <- t.test(x = set1, y = set2)
      return (tt$p.value)
    }
  })
  ps <- unlist(tests)
  selCols <- columns[ps<pvalueThreshold]
  output <- data[,colnames(data) %in% c(selCols,classColumn),drop=FALSE]
  return (output)
}

featureSelection <- function(data, classColumn, method) {
  ### 
  # Apply the feature selection strategy indicated in 'method' to the dataset
  # Returns the dataset with only the selected columns (and the ones for date and class)
  #
  switch(method,
         none = { 
           return(data) 
         },
         pvalue05 = {
           return(applyFilterPvalue(data = data,classColumn =  classColumn,pvalueThreshold = 0.05))
         },
         fcbf ={
           require(Biocomb)
           dft <- data[,!(colnames(data) %in% c(classColumn))]
           for (i in 1:ncol(dft)) {
             if (is.logical(dft[[i]])) {
               dft[[i]]<-factor(dft[[i]])
             }
           }
           nominalCols <- which(unlist(lapply(dft,is.factor)))
           dft$class <- data[[classColumn]]
           
           # According to Yu 2004: 
           # "Therefore, when we do not have prior knowledge about data, an easy and
           #   safe way of applying FCBF is to set [threshold] to the default value 0."
           #   The implementation of the MDL disc.method seems to have problems with small numerical values.
           #   In order to avoid data manipulation, we use equal frequency instead
           fs <- select.fast.filter(dft,disc.method = "equal frequency", threshold = 0, attrs.nominal = nominalCols)
           
           selection <- data[,colnames(data) %in% c(colnames(dft)[fs$NumberFeature],classColumn)]
           return(selection)
         },{
           stop(paste0("Feature selection method '",method,"' unknown."))
         }
  )
}