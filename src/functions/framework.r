#
#
# Multiple functions to carry out the experiments
#
#
# Require sourcing of 
#   "functions/utils.r"
#   "functions/preprocessing.r"


#//////////////////////////////////////////////////////////////////////////
# EXPERIMENT MANAGEMENT ---------------------------------------------------
#//////////////////////////////////////////////////////////////////////////

getIdExperiment <- function(paramsExperiment) {
# Each experiment is defined by a list of parameters
# This function resturns a string 
#  

  # The experiments are repeated many times with different samplings of training/validations dataset. 
  # The param 'it' indicates the current resample
  it = ""
  if (!is.na.oneCheck(paramsExperiment$it)) {
    it = sprintf("IT%03d_",paramsExperiment$it)
  }
    
  wSlide = ""
  if ( (!is.null(paramsExperiment$wSlide)) && (!is.na.oneCheck(paramsExperiment$wSlide)) )  {
    wSlide = sprintf("_WIT%02d",paramsExperiment$wSlide)
  }
  
  wUnit = ""
  if ( (!is.null(paramsExperiment$wUnit)) && (!is.na.oneCheck(paramsExperiment$wUnit)) )  {
    wUnit = paste0("_WU",paramsExperiment$wUnit)
  }
  
  wStep = ""
  if ( (!is.null(paramsExperiment$wStep)) && (!is.na.oneCheck(paramsExperiment$wStep)) )  {
    wStep = paste0("_WST",paramsExperiment$wStep)
  }
  
  
  ensembleChunk = ""
  if ( (!is.null(paramsExperiment$ensembleChunk)) && (!is.na.oneCheck(paramsExperiment$ensembleChunk)) ) {
    ensembleChunk = sprintf("_CNK%02d",paramsExperiment$ensembleChunk)
  }
  
  ensembleIt = ""
  if ( (!is.null(paramsExperiment$ensembleIt)) && (!is.na.oneCheck(paramsExperiment$ensembleIt)) ) {
    ensembleIt = sprintf("_EIT%02d",paramsExperiment$ensembleIt)
  }
  
  str<-paste0(it,"D",paramsExperiment$drift,wUnit,wStep,wSlide,ensembleIt,ensembleChunk,"_M",paramsExperiment$model,"_B",paramsExperiment$imbalance,"_F",paramsExperiment$fsel)
  return(str)
}

getDirectoryExperiment <- function(paramsExperiment) {
# Each set of experiments is stored into different directories to facilitate their storage
# This function returns the path (based on Windows) in which the results of the indicated experiment should be stored
#
# paramsExperiment$modelsPath should contain the base directory for storing experiment results
#
  sepDir = "/"
  if (grepl(".*\\\\$",paramsExperiment$modelsPath,fixed = FALSE) )  { 
    sepDir = "\\"
  }
  
  return(paste0(paramsExperiment$modelsPath,"D",paramsExperiment$drift,"_M",paramsExperiment$model,"_B",paramsExperiment$imbalance,"_F",paramsExperiment$fsel,sepDir))
}


loadOrExecute <- function(file, action, loadOnly = FALSE, saveResults = TRUE) {
  # Try to load an experiment result (model or prediction)
  # If fails, try to execute the action (i.e. execute the experiment or make prediction)
  # If everything fails, return NA
  #
  # Parameters:
  # file   : name of the file to load (with its path but without extension)
  # action : Piece of code to execute in case the file does not exists
  # saveResults : if TRUE (default), the result of the action (if executed) is saved into the indicated file
  #

  if (file.exists(paste0(file,".rds"))) {
    result <- readRDS(file = paste0(file,".rds"))
    if (logActive()) doLog(paste0("File ",file," loaded suscessfully"),funcName="loadOrExecute")
    return (result)
  } else {
    if (loadOnly) {
      return(NA)
    } else {
      result <- tryCatch({
        action
      }, error = function(e) {
        msg = paste0("Error when executing action for file ",file,". Error ",e)
        if (logActive()) doLog(msg,funcName="loadOrExecute")
        warning(msg)
        return(NA)
      })
    }

    if (!is.na.oneCheck(result) && saveResults) {
      saveRDS(object = result, file = paste0(file,".rds"))
      if (logActive()) doLog(paste0("File ",file," generated suscessfully"),funcName="loadOrExecute")
    }
    return(result)
  }
}

loadModelExperiment <- function(paramsExperiment) {
  # Load a model according to which is indicated in paramsExperiment
  
  fileModel <- paste0(getDirectoryExperiment(paramsExperiment),"Model_"     ,getIdExperiment(paramsExperiment),"_",paramsExperiment$dataset,".rds")
  model = readRDS(file = fileModel)
  return(model)
}

loadPredictionExperiment <- function(paramsExperiment) {
  # Load the predictions resulting from the experiment indicated in paramsExperiment
  
  filePred <-   paste0(getDirectoryExperiment(paramsExperiment),"Predi_",getIdExperiment(paramsExperiment),"_",paramsExperiment$dataset,".rds")
  pred = readRDS(file = filePred)
  return(pred)
}

executeExperiment<- function(trainData, testData, dateColumn, classColumn, paramsExperiment, loadOnly = FALSE, seed = 1, verbose = FALSE) {
  # Execute an experiment according to the values indicated in paramsExperiment
  # 
  
  if (startsWith(paramsExperiment$drift,"monthlyWindow")) {
    windowSize<-as.integer(substr(paramsExperiment$drift,14,15))
    result <- executeExperiment.monthlyWindow(trainData, testData, dateColumn, classColumn, paramsExperiment, loadOnly, window = windowSize, seed = seed, verbose = verbose)
  } else {
    switch(paramsExperiment$drift, 
           none             = { result <- executeExperiment.ignoreDrift      (trainData, testData, dateColumn, classColumn, paramsExperiment, loadOnly, seed = seed, verbose = verbose) },
           seasonAsFeature  = { result <- executeExperiment.seasonAsFeature  (trainData, testData, dateColumn, classColumn, paramsExperiment, loadOnly, seed = seed, verbose = verbose) },
           modelPerSeason   = { result <- executeExperiment.modelPerSeason   (trainData, testData, dateColumn, classColumn, paramsExperiment, loadOnly, seed = seed, verbose = verbose) },
           seasonalWindow   = { result <- executeExperiment.seasonalWindow   (trainData, testData, dateColumn, classColumn, paramsExperiment, loadOnly, seed = seed, verbose = verbose) },
           monthlyEnsemble  = { result <- executeExperiment.monthlyEnsemble  (trainData, testData, dateColumn, classColumn, paramsExperiment, loadOnly, seed = seed, verbose = verbose) },
           seasonalEnsemble = { result <- executeExperiment.seasonalEnsemble (trainData, testData, dateColumn, classColumn, paramsExperiment, loadOnly, seed = seed, verbose = verbose) },
           {
             msg <- paste0("[ERROR] Unknown drift approach '",paramsExperiment$drift,"'")
             if (logActive()) doLog(msg, funcName = "executeExperiment")
             warning(msg)
             return(NA)
           }
    )
  }
  return(result)
}

getIdxResample <- function(data, classColumn, seed, minElemsClass = 2, paramsExperiment) {
  # Generate a sampling with replacement from 'data'
  # 
  #
  # It is forced that the sample must have at least 'minElemsClass' observations of each class level. It gives up after trying for 10 times
  #  
  #
  # Returns which indexes belong to the sample
  #
  numTries <- 1
  numData <- nrow(data)
  set.seed(seed)
  while (numTries<=10) {
    idxs <- sample.int(numData,replace=TRUE) 
    
    classes = summary(data[idxs,classColumn])
    if ((classes[1]>=minElemsClass) && (classes[2]>=minElemsClass))
      return(idxs)
    numTries <- numTries+1
  }
  return(NA)
}

getIdxResampleMonthly <- function(initialData, classColumn, dateColumn, seed, minElemsClass = 2, paramsExperiment) {
  # Generate a sampling with replacement from 'intitialData'
  # 
  #
  # It is forced that the sample must have at least 'minElemsClass' observations of each class level in each month of the timestamp column 'dateColumn'.
  # It gives up after trying for 100 times
  #  
  #
  # Returns which indexes belong to the sample
  #
  require(lubridate)
  numData <- nrow(initialData)
  monthData <- month(initialData[[dateColumn]])
  minorClass  <- getLabelMinorityClass(initialData[[classColumn]])
  idxMinorClass <- c()
  for (idxMonth in 1:12) {
    minorIdxs <- which(monthData==idxMonth & initialData[[classColumn]] ==  minorClass)
    set.seed(seed)
    if (length(minorIdxs)<minElemsClass) {
      return(NA) # We are unable to put minElemsClass of each class per month in this resample
    } else if (length(minorIdxs)==minElemsClass) {
      # Only minElemsClass are available this month. We pick them
      randomNumMinorSamples = minElemsClass
    } else {
      # We have plenty of them, so we choose a random number of them
      set.seed(seed)
      randomNumMinorSamples <- sample(minElemsClass:length(minorIdxs),1)
    }
    set.seed(seed)
    idxMinorClass<-c(idxMinorClass, sample(minorIdxs,randomNumMinorSamples))
  }
  majorIdxs <- which(initialData[[classColumn]] !=  minorClass)
  set.seed(seed)
  idxMajorClass <- sample(majorIdxs,numData-length(idxMinorClass),replace=TRUE)
  return(c(idxMajorClass,idxMinorClass))
}

executeExperiment.resample <- function(idResample, trainData, testData, dateColumn, classColumn, paramsExperiment, loadOnly = FALSE, verbose = FALSE) {
  # Create a resampling for trainData and use them to launch the experiment.
  #
  # Note: the same seed is always used for the sample idResample
  #
  paramsExperiment$it = idResample
  minElemsClass<-3 # Min elements of each class level that must exists in each resample /month of the resample
  if (paramsExperiment$drift %in% c("monthlyWindow03","monthlyEnsemble")) {
    idxs  = getIdxResampleMonthly(trainData, classColumn, dateColumn, (1000+idResample), minElemsClass = minElemsClass, paramsExperiment)
  } else {
    idxs  = getIdxResample(trainData, classColumn, (1000+idResample), minElemsClass = minElemsClass, paramsExperiment)
  }
  
  if (!is.na.oneCheck(idxs)) { 
    results = executeExperiment(trainData[idxs,], testData, dateColumn, classColumn, paramsExperiment, loadOnly, verbose = verbose)
  } else {
    if (logActive()) doLog(paste0("[ERROR] Unable to create resample with seed ",(1000+idResample)," for experiment ", getIdExperiment(paramsExperiment), " and data ",summariseDataset(trainData, NULL, classColumn)), funcName = "executeExperiment.resample")
    results = NA
  }
  return(list(idResample = idResample, results = results))
}


executeExperiment.bootstrap.parallel <- function(rep = 100, trainData, testData, dateColumn, classColumn, paramsExperiment, nCores=2, verbose = FALSE, functionPath = "./src/functions/") {
  # Create 'rep' resamplings for trainData and use them to launch 'rep' experiments.
  # This function uses the parallel library 
  #
  #
  require(parallel)
  cl <- makeCluster(nCores)
  if (logActive()) clusterExport(cl,"LogFile__")
  results<-tryCatch({
    parLapply(cl,1:rep,function(rep,...) {
      
      source(paste0(functionPath,"utils.r"))
      source(paste0(functionPath,"preprocessing.r"))
      source(paste0(functionPath,"framework.r"))

      res = tryCatch({
        executeExperiment.resample(idResample = rep , ...)
      },error = function(e) {
        if(logActive()) doLog(paste0("[ERROR] executeExperiment.resample: ",e), funcName = "executeExperiment.bootstrap.parallel")
        return(NA)
      })
      return(res)
    } 
    , trainData = trainData, testData = testData, dateColumn = dateColumn, classColumn = classColumn, paramsExperiment = paramsExperiment, verbose = verbose)
  }, error = function(e) {
    if(logActive()) doLog(paste0("[ERROR] bootstrap parallel: ",e),funcName = "executeExperiment.bootstrap.parallel")
    return(NA)
  }, finally = function() {stopCluster(cl)})
  
  return(results)  
  
}


executeExperiment.bootstrap <- function(rep = 100, trainData, testData, dateColumn, classColumn, paramsExperiment, loadOnly = FALSE, verbose = FALSE ) {
  # Non-parallel version of executeExperiment.bootstrap.parallel 
  #
  #
  results<-tryCatch({
    lapply(1:rep,function(rep,...) {
      res=executeExperiment.resample(idResample = rep , ...)
      return(res)
    } 
    , trainData = trainData, testData = testData, dateColumn = dateColumn, classColumn = classColumn, paramsExperiment = paramsExperiment, loadOnly = loadOnly, verbose = verbose)
  }, error = function(e) {
    if(logActive()) doLog(paste0("[ERROR] bootstrap: ",e),funcName = "executeExperiment.bootstrap")
    return(NA)
  })
  
  return(results)  
  
}

retrieveResults<-function(paramsExperiment) {
  # Load the model and predictions indicated in paramsExperiment and calculate AUC
  #
  # Delegate on different methods depending on the type of model and the seasonality strategy followed.
  # The structure of the returned object varies depending on the parameters of the experiment.
  #
  result<-NA
  if (startsWith(paramsExperiment$drift,"monthlyWindow")) {
    windowSize<-as.integer(substr(paramsExperiment$drift,14,15))
    result <- retrieveResults.slidingWindow(paramsExperiment, nWindows = 12)
  } else {
    switch(paramsExperiment$drift, 
           none             = { result <- retrieveResults.ignoreDrift      (paramsExperiment) },
           seasonAsFeature  = { result <- retrieveResults.seasonAsFeature  (paramsExperiment) },
           modelPerSeason   = { result <- retrieveResults.modelPerSeason   (paramsExperiment) },
           seasonalWindow   = { result <- retrieveResults.slidingWindow    (paramsExperiment, nWindows = 4) },
           monthlyEnsemble  = { result <- retrieveResults.ensemble (paramsExperiment) },
           seasonalEnsemble = { result <- retrieveResults.ensemble (paramsExperiment) },
           {
             msg <- paste0("[ERROR] Unknown drift approach '",paramsExperiment$drift,"'")
             if (logActive()) doLog(msg, funcName = "retrieveResults")
             warning(msg)
             return(NA)
           }
    )
  }
  return(result)
}


retrieveResults.bootstrap<-function(nBoot, paramsExperiment) {
  # Load the model, predictions and results of 'nBoot' experiments made with resamples of a dataset,
  # following the type of model and seasonality approach indicated in 'paramsExperiment'
  #
  #  
  results<-tryCatch({
    lapply(1:nBoot,function(rep,paramsExperiment) {
      paramsExperiment$it <- rep
      res=retrieveResults(paramsExperiment)
      return(res)
    } 
    , paramsExperiment = paramsExperiment)
  }, error = function(e) {
    if(logActive()) doLog(paste0("[ERROR] bootstrap: ",e),funcName = "retrieveResults.bootstrap")
    return(NA)
  })
  
  return(results)  
}

retrieveResults.bootstrap.parallel<-function(nBoot, paramsExperiment, nCores = 1, functionPath = "./src/functions/") {
  # Load the model, predictions and results of 'nBoot' experiments made with resamples of a dataset,
  # following the type of model and seasonality approach indicated in 'paramsExperiment'
  #
  # It uses 'parallel' library in order to parallelize the process 
  #  
  require(parallel)
  cl <- makeCluster(nCores)
  if (logActive()) clusterExport(cl,"LogFile__")
  results<-tryCatch({
    parLapply(cl,1:nBoot,function(rep,...) {
      
      source(paste0(functionPath,"utils.r"))
      source(paste0(functionPath,"preprocessing.r"))
      source(paste0(functionPath,"framework.r"))
      
      paramsExperiment$it <- rep
      res = tryCatch({
        retrieveResults(paramsExperiment)
      },error = function(e) {
        if(logActive()) doLog(paste0("[ERROR] Error retrieving results for experiment",getIdExperiment(paramsExperiment)," resample ",rep,": ",e),funcName = "retrieveResults.bootstrap.parallel")
        return(NA)
      })
      return(res)
    } 
    , paramsExperiment = paramsExperiment)
  }, error = function(e) {
    if(logActive()) doLog(paste0("[ERROR] Error retrieving results for experiment",getIdExperiment(paramsExperiment)," :",e),funcName = "retrieveResults.bootstrap.parallel")
    return(NA)
  }, finally = function() {stopCluster(cl)})
  
  return(results)  
}





#//////////////////////////////////////////////////////////////////////////
# BASIC MODEL CREATION ----------------------------------------------------
#//////////////////////////////////////////////////////////////////////////

createModel.glmnet<-function(trainData, dateColumn, classColumn, paramsExperiment, seed = 1, verbose=FALSE) {
  # Creates a model using glmnet
  #
  require(caret)
  require(glmnet)
  
  idExperiment  <- getIdExperiment(paramsExperiment)
  dfTrainSubset <- trainData
  
  
  if (verbose && logActive()) doLog(paste0(idExperiment," - Starting model creation. Initial train data:",summariseDataset(trainData,dateColumn,classColumn)),funcName = "createModel.glmnet")
  
  # Remove date column
  dfTrainSubset[[dateColumn]] <- NULL
  
  # Perform balancing option
  set.seed(seed)
  dfTrainSubset <- balance(data = dfTrainSubset, classColumn = classColumn, method = paramsExperiment$imbalance)
  
  if (verbose && logActive()) doLog(paste0(idExperiment," - After balancing: ",summariseDataset(dfTrainSubset,dateColumn = NULL,classColumn)),funcName = "createModel.glmnet")

  # Remove any zero-var (constant) column generated (it may happen after undersampling)
  zcols               <- apply(dfTrainSubset,2,function(x) length(unique(x))==1)
  dfTrainSubset       <- dfTrainSubset[,!zcols, drop = FALSE]
  
  # Perform feature selection 
  set.seed(seed)
  dfTrainSubset <- featureSelection(data = dfTrainSubset, classColumn = classColumn, method = paramsExperiment$fsel)
  
  if (verbose && logActive()) doLog(paste0(idExperiment," - After feature selection: ",summariseDataset(dfTrainSubset,dateColumn = NULL,classColumn)),funcName = "createModel.glmnet")
  
  # Remove any zero-var (constant) column generated
  zcols               <- apply(dfTrainSubset,2,function(x) length(unique(x))==1)
  dfTrainSubset       <- dfTrainSubset[,!zcols, drop = FALSE]

  if (verbose && logActive()) doLog(paste0(idExperiment," - After removing zero-var columns: ",summariseDataset(dfTrainSubset,dateColumn = NULL,classColumn)),funcName = "createModel.glmnet")
  
  dfTrainNoClass <- dfTrainSubset[,!(colnames(dfTrainSubset) %in% c(classColumn)), drop = FALSE]
  if (ncol(dfTrainNoClass)==0) { # No feature remains after the preprocessing techniques. No model created 
    if (logActive()) doLog(paste0("ERROR: ",idExperiment," No feature remains after the preprocessing techniques. No model generated."),funcName = "createModel.glmnet")
    return (NA)
  } else if (ncol(dfTrainNoClass)== 1) {     # glmnet requires at least 2 features to work. If only one remains, glm is used instead
    if (verbose && logActive()) doLog(paste0(idExperiment," - Using glm because one only feature remains (",colnames(dfTrainNoClass)[1],") after filtering") ,funcName = "createModel.glmnet")
    
    model <- glm(
      as.formula(paste0(classColumn,"~.")),data = dfTrainSubset, 
      family = binomial())
  } else {
    # First, we try to fit the model using 10-fold cross-validation
    nfolds <- 10
    set.seed(seed)
    folds <- createFolds(factor(dfTrainSubset[[classColumn]]), k = nfolds, list = FALSE)
    model<-tryCatch({ 
        cv.glmnet(as.matrix(dfTrainNoClass), dfTrainSubset[[classColumn]],nfolds = nfolds, foldid = folds, family = "binomial", alpha = 0.99999) 
      },error=function(e) {
        if (logActive()) doLog(paste0("Warning: In experiment ",idExperiment,", we failed to create the model using 10-fold CV. Trying with LOOCV. Error: ",e),funcName = "createModel.glmnet")
        return(NA)
      })
    if (is.na.oneCheck(model)) {
      # If we failed, let try with LOOCV
      nfolds <- nrow(dfTrainSubset)
      set.seed(seed)
      folds <- createFolds(factor(dfTrainSubset[[classColumn]]), k = nfolds, list = FALSE)
      model<-tryCatch({ 
        cv.glmnet(as.matrix(dfTrainNoClass), dfTrainSubset[[classColumn]],nfolds = nfolds, foldid = folds, family = "binomial", alpha = 0.99999) 
      },error=function(e) {
        if (logActive()) doLog(paste0("ERROR: In experiment ",idExperiment,", we failed to create the model using 10-fold CV and LOOCV. No model was created. Error: ",e),funcName = "createModel.glmnet")
        return(NA)
      })
    }
  }
  if (logActive() && !is.na.oneCheck(model)) doLog(paste0(idExperiment," - Model created sucessfully"),funcName = "createModel.glmnet")
  
  return(model);
}

createModel.C5.0 <- function(trainData, dateColumn, classColumn, paramsExperiment, seed = 1, verbose=FALSE) {
  # Creates a model using C5.0
  #
  require(caret)
  require(C50)
  
  idExperiment  <- getIdExperiment(paramsExperiment)
  dfTrainSubset <- trainData
  
  
  if (verbose && logActive()) doLog(paste0(idExperiment," - Starting model creation. Initial train data:",summariseDataset(trainData,dateColumn,classColumn)),funcName = "createModel.glmnet")
  
  # Remove date column
  dfTrainSubset[[dateColumn]] <- NULL
  
  # Perform balancing option
  set.seed(seed)
  dfTrainSubset <- balance(data = dfTrainSubset, classColumn = classColumn, method = paramsExperiment$imbalance)
  
  if (verbose && logActive()) doLog(paste0(idExperiment," - After balancing: ",summariseDataset(dfTrainSubset,dateColumn = NULL,classColumn)),funcName = "createModel.glmnet")
  
  # Remove any zero-var (constant) column generated (it may happen after undersampling)
  zcols               <- apply(dfTrainSubset,2,function(x) length(unique(x))==1)
  dfTrainSubset       <- dfTrainSubset[,!zcols, drop = FALSE]
  
  # Perform feature selection 
  set.seed(seed)
  dfTrainSubset <- featureSelection(data = dfTrainSubset, classColumn = classColumn, method = paramsExperiment$fsel)
  
  if (verbose && logActive()) doLog(paste0(idExperiment," - After feature selection: ",summariseDataset(dfTrainSubset,dateColumn = NULL,classColumn)),funcName = "createModel.glmnet")
  
  # Remove any zero-var (constant) column generated
  zcols               <- apply(dfTrainSubset,2,function(x) length(unique(x))==1)
  dfTrainSubset       <- dfTrainSubset[,!zcols, drop = FALSE]
  
  if (verbose && logActive()) doLog(paste0(idExperiment," - After removing zero-var columns: ",summariseDataset(dfTrainSubset,dateColumn = NULL,classColumn)),funcName = "createModel.glmnet")
  
  dfTrainNoClass <- dfTrainSubset[,!(colnames(dfTrainSubset) %in% c(classColumn)), drop = FALSE]
  if (ncol(dfTrainNoClass)==0) { # No feature remains after the preprocessing techniques. No model created 
    if (logActive()) doLog(paste0("ERROR: ",idExperiment," No feature remains after the preprocessing techniques. No model generated."),funcName = "createModel.glmnet")
    return (NA)
  } 
  
  set.seed(seed)
  model <- tryCatch({ 
    # C5.0 Do not accept logical features. We change them to 0/1
    dfTrainNoClass <- dummify(dataset = dfTrainNoClass, ignoreCols = c(), asIntegers = TRUE,reFactor = FALSE)
    C5.0(dfTrainNoClass, dfTrainSubset[[classColumn]], control = C5.0Control(winnow = TRUE))
  },error=function(e) {
    if (logActive()) doLog(paste0("ERROR: In experiment ",idExperiment,", we failed to create the model using C5.0. No model was created. Error: ",e),funcName = "createModel.C5.0")
    return(NA)
  })

  if (logActive() && !is.na.oneCheck(model)) doLog(paste0(idExperiment," - Model created sucessfully"),funcName = "createModel.glmnet")
  
  return(model);
    
}

createModel <- function(trainData, dateColumn, classColumn, paramsExperiment, seed = 1, verbose=FALSE) {
  switch(paramsExperiment$model,
     glmnet = {
       model <- createModel.glmnet(trainData, dateColumn, classColumn, paramsExperiment, seed, verbose)
       return (model)
     },
     C50 = {
       model <- createModel.C5.0(trainData, dateColumn, classColumn, paramsExperiment, seed, verbose)
       return (model)
     },
     {
       msg <- paste0("[ERROR] Model type '",paramsExperiment$model,"' unknown.")
       if (logActive()) doLog(msg, funcName = "createModel")
       warning(msg)
       return(NA)
     }
  )
}

#//////////////////////////////////////////////////////////////////////////
# BASIC PREDICTION --------------------------------------------------------
#//////////////////////////////////////////////////////////////////////////

doPredict <- function(testData, dateColumn, classColumn, model, paramsExperiment) {
  
  switch(paramsExperiment$model,
         glmnet =  {
           prediction <- doPredict.glmnet(testData, dateColumn, classColumn, model, paramsExperiment)
           return(prediction)
         },
         C50 =  {
           prediction <- doPredict.C5.0(testData, dateColumn, classColumn, model, paramsExperiment)
           return(prediction)
         },
         {
           msg <- paste0("[ERROR] Model '",paramsExperiment$model,"' unknown.")
           if (logActive()) doLog(msg, funcName="predict")
           warning(msg)
           return(NA)
         }
  )
}

doPredict.glmnet <- function(testData, dateColumn, classColumn, model, paramsExperiment) {
  # Uses model (assuming is a model created with glmnet) to predict the class for observations in testData
  #
    require(glmnet)
  
  lambda = "lambda.1se"

  if (inherits(model,"cv.glmnet")) { # The model was created with glmnet
    cf <- coef(model, s = lambda)
    pred <- predict(model, as.matrix(testData[,(colnames(testData) %in% rownames(cf)), drop = FALSE]), type = "response", s = lambda)
    predDf <- NA
    if (!is.na.oneCheck(pred)) {
      predDf<-data.frame(prediction = pred[,1],observation=testData[[classColumn]])
    }
  } else { # The model was created with glm
    pred <- predict(model, testData, type = "response")
    predDf <- NA
    if (!is.na.oneCheck(pred)) {
      predDf<-data.frame(prediction = pred,observation=testData[[classColumn]])
    }
  }

  
  return(predDf)
}

doPredict.C5.0 <- function(testData, dateColumn, classColumn, model, paramsExperiment) {
  # Uses model (assuming is a model created with C5.0) to predict the class for observations in testData
  #
  require(C50)
  
  testDataNoBool <- dummify(dataset = testData, ignoreCols = c(dateColumn,classColumn), asIntegers = TRUE,reFactor = FALSE)
  
  pred <- predict(model, newdata = testDataNoBool, type = "prob")
  predDf <- NA
  if (!is.na.oneCheck(pred)) {
    predDf<-data.frame(prediction = pred[,1],observation=testData[[classColumn]])
  }
  
  return (predDf)
}

#//////////////////////////////////////////////////////////////////////////
# RESULTS EVALUATION ------------------------------------------------------
#//////////////////////////////////////////////////////////////////////////

calculateROC.Standard<-function(results, paramsExperiment) {
  # Calculate AUC given predictions:
  # results$prediction  -> Estimated by the model
  # results$observation -> Real class from the data
  
  modelRoc <- tryCatch({
    etiq<-levels(factor(results$observation))
    pROC::roc(predictor=results$prediction, response=results$observation, levels = sort(etiq), quiet=TRUE)
  },error = function(e) {
    if (logActive()) doLog(paste0("[ERROR] calculateROC.Standard ", getIdExperiment(paramsExperiment),": ",e), funcName = "calculateROC")
    return(NA)
  })
  return (modelRoc)
}


calculateROC.PRAUC<-function(results,paramsExperiment) {
  # Calculate the PR-AUC
  require(PRROC)
  
  
  modelRoc <- tryCatch({
    foregroundScores <- results$prediction[results$observation==getLabelMinorityClass(results$observation)] 
    backgroundScores <- results$prediction[results$observation==getLabelMajorityClass(results$observation)]
    
    resCurve <- pr.curve(foregroundScores,backgroundScores)
    # Create similar output for auc as traditional pROC::roc
    res<-list()
    res$auc <- resCurve$auc.integral
    res
  },error = function(e) {
    if (logActive()) doLog(paste0("[ERROR] calculateROC.PRAUC ", getIdExperiment(paramsExperiment),": ",e), funcName = "calculateROC")
    return(NA)
  })
  
  return (modelRoc)
  
}

calculateROC<-function(results, paramsExperiment) {
  switch(paramsExperiment$aucType,
         standard = {
           modelRoc <- calculateROC.Standard(results, paramsExperiment)
           return (modelRoc)
         },
         prauc = {
           modelRoc <- calculateROC.PRAUC(results, paramsExperiment)
           return (modelRoc)
         },
         {
           msg <- paste0("[ERROR] AUC type for recovering results '",paramsExperiment$model,"' unknown.")
           if (logActive()) doLog(msg, funcName = "calculateROC")
           warning(msg)
           return(NA)
         }
  )
}

getCoefsModel.glmnet<-function(model, lambda = "lambda.1se") {
  # Retrieve the coefficients used in the model (only those whose weight is !=0)
  #
  if (inherits(model,"cv.glmnet")) {
    cf <- coef(model, s = lambda)
    validCoefs = cf[(cf[,1] != 0), 1,drop = FALSE]
  } else {
    validCoefs=as.data.frame(coef(model))
  }
  coefsList <- as.list(validCoefs)
  
  if (length(validCoefs)==1) {
    return(list(coef=c("(Intercept)"), value=coefsList[[1]]))
  } else {
    return(list(coef=rownames(validCoefs), value=unlist(coefsList)))
  }
}

getCoefsModel.C5.0<-function(model) {
  # Retrieve the coefficients used in the splits of the tree
  #
  require(C50)
  
  splits<-C5imp(model,metric ="splits")
  validItems <- !is.na(splits$Overall) & splits$Overall>0
  validCoefs <- rownames(splits)[validItems]
  value <- splits$Overall[validItems]
  return(list(coef=validCoefs, value=value))
}

getCoefsModel<-function(paramsExperiment, model, lambda="lambda.1se") {
  switch(paramsExperiment$model,
         glmnet = {
           coefs <- getCoefsModel.glmnet(model,lambda)
           return (coefs)
         },
         C50 = {
           coefs <- getCoefsModel.C5.0(model)
           return (coefs)
         },
         {
           msg <- paste0("[ERROR] Model type '",paramsExperiment$model,"' unknown.")
           if (logActive()) doLog(msg, funcName = "getCoefsModel")
           warning(msg)
           return(NA)
         }
  )
}

#//////////////////////////////////////////////////////////////////////////
# TRADITIONAL APPROACHES FOR SEASONALITY ----------------------------------
#//////////////////////////////////////////////////////////////////////////

executeExperiment.ignoreDrift <- function(trainData, testData, dateColumn, classColumn, paramsExperiment, loadOnly, seed = 1, verbose=FALSE) {
  # Creates a model based on trainData and uses it to predict the class for testData. 
  # Stores both the model(s) and predictions on disk
  # Ignores any concept drift (i.e. no extra techniques are used to deal with it)
  #
  # Also used when techniques for dealing with seasonality have been previously 
  # applied to a dataset (e.g. partition to generate a model per season) and the next
  # step requires to generate a model without further modifications.
  #
  # returns a list with the model and the prediction
  
  # Prepare directories and files 
  fileModel <- paste0(getDirectoryExperiment(paramsExperiment),"Model_",getIdExperiment(paramsExperiment),"_",paramsExperiment$dataset)
  filePred  <- paste0(getDirectoryExperiment(paramsExperiment),"Predi_",getIdExperiment(paramsExperiment),"_",paramsExperiment$dataset)
  dir.create(getDirectoryExperiment(paramsExperiment), showWarnings = FALSE) # if extists, nothing happens
  
  model <- loadOrExecute(file = fileModel, action = {
    createModel(trainData = trainData, dateColumn = dateColumn, classColumn = classColumn, paramsExperiment = paramsExperiment, seed = seed,verbose=verbose)
  }, loadOnly = loadOnly)
  
  if (is.na.oneCheck(model)) {
    if (logActive()) doLog(paste0("[ERROR] Model ", getIdExperiment(paramsExperiment), " not created."), funcName = "executeExperiment.ignoreDrift")
    return(list(model = NA, prediction = NA))
  }
  
  prediction <- loadOrExecute(file = filePred, action = {
    doPredict(testData, dateColumn, classColumn, model, paramsExperiment)
  }, loadOnly = loadOnly)
  
  if (is.na.oneCheck(prediction)) {
    if (logActive()) doLog(paste0("[ERROR] Unable to estimate predictions for ", getIdExperiment(paramsExperiment)), funcName="executeExperiment.ignoreDrift")
    return(list(model = model, prediction = NA))
  }
  
  if (verbose && logActive()) doLog(paste0("Experiment ", getIdExperiment(paramsExperiment), " finished successfully."), funcName="executeExperiment.ignoreDrift")
  
  return(list(model = model, prediction = prediction))
}


executeExperiment.seasonAsFeature <- function(trainData, testData, dateColumn, classColumn, paramsExperiment, loadOnly, seed = 1, verbose=FALSE) {
  # Creates a model based on trainData adding the season as another feature and uses it to predict the class for testData. Stores both model and predictions on disk
  # 
  # returns a list with the model and the prediction
  
  trainDataSeason        <- trainData
  trainDataSeason$season <- getSeason(trainDataSeason[[dateColumn]])
  testDataSeason         <- testData
  testDataSeason$season  <- getSeason(testDataSeason[[dateColumn]])
  
  trainDataSeason <- dummify(trainDataSeason,ignoreCols = c(dateColumn,classColumn))
  testDataSeason  <- dummify(testDataSeason,ignoreCols = c(dateColumn,classColumn))
  
  
  # With the season added as a new feature, we follow a usual modelling strategy
  result <- executeExperiment.ignoreDrift(trainDataSeason, testDataSeason, dateColumn, classColumn, paramsExperiment, loadOnly, seed = seed, verbose=verbose) 
  
  return(result)
}

executeExperiment.modelPerSeason <- function(trainData, testData, dateColumn, classColumn, paramsExperiment, loadOnly, seed = 1, verbose=FALSE) {
  # Creates a different model for each season based on trainData and them to predict the class for testData. Stores both models and predictions on disk
  # 
  # returns a list with the four models and the predictions
  
  trainDataSeason        <- trainData
  trainDataSeason$season <- getSeason(trainDataSeason[[dateColumn]])
  testDataSeason         <- testData
  testDataSeason$season  <- getSeason(testDataSeason[[dateColumn]])
  
  seasons <- levels(trainDataSeason$season)
  idxSeason <- 1
  models      <- list(NA,NA,NA,NA)
  predictions <- list(NA,NA,NA,NA)
  paramsExperimentSeason <- paramsExperiment
  for (idxSeason in 1:length(seasons)) {
    season <- seasons[[idxSeason]]
    
    subsetTrain <- trainDataSeason[trainDataSeason$season==season,]
    subsetTrain$season <- NULL
    subsetTest  <- testDataSeason[testDataSeason$season==season,]
    subsetTest$season  <- NULL
    
    paramsExperimentSeason$drift <- paste0("ModelPerSeason_",season)
    
    result <- executeExperiment.ignoreDrift(subsetTrain, subsetTest, dateColumn, classColumn, paramsExperimentSeason, loadOnly, seed = seed, verbose=verbose) 
    
    models[[idxSeason]]     <- result$model
    predictions[[idxSeason]]<- result$prediction
  }
  
  if (verbose && logActive() && !anyNA(predictions, recursive = TRUE)) doLog(paste0("Experiment ", getIdExperiment(paramsExperiment), " finished successfully."), funcName="executeExperiment.modelPerSeason")
  
  return (list(model = models, prediction = predictions))
}

retrieveResults.ignoreDrift<-function(paramsExperiment) {
  # Load the model and predictions indicated in paramsExperiment and calculate AUC
  #
  #  [[resample]]$coefs[[submodel(none)]]$coef
  #  [[resample]]$coefs[[submodel(none)]]$value
  #
  
  idExperiment <- getIdExperiment(paramsExperiment)
  
  model <- tryCatch({
    loadModelExperiment(paramsExperiment)
  },error = function (e) {
    if (logActive()) doLog(paste0("[ERROR] Error loading model ",idExperiment,": ",e), funcName = "retrieveResults.ignoreDrift")
    return(NA)
  })
  
  pred <- tryCatch({
    loadPredictionExperiment(paramsExperiment)
  },error = function (e) {
    if (logActive()) doLog(paste0("[ERROR] Error loading predictions for ",idExperiment,": ",e), funcName = "retrieveResults.ignoreDrift")
    return(NA)
  })
  
  modelRoc <- tryCatch({
    calculateROC(pred,paramsExperiment)
  }, error = function(e) {
    if (logActive()) doLog(paste0("[ERROR] Error calculating AUC for ",idExperiment,": ",e), funcName = "retrieveResults.ignoreDrift")
    return(NA)
  })
  
  coefs <- as.list(rep(NA,1))
  if (!is.na.oneCheck(model)) {
    coefs[[1]]    <- getCoefsModel(paramsExperiment, model)
  }
  
  aucTest <-  NA
  if (!is.na.oneCheck(modelRoc)) {
    aucTest  <- modelRoc$auc  
  }
  
  return (list(coefs = coefs, aucTest = aucTest))
}


retrieveResults.seasonAsFeature<-function(paramsExperiment) {
  # Load the model and predictions indicated in paramsExperiment and calculate AUC
  #
  #  [[resample]]$coefs[[submodel(none)]]$coef
  #  [[resample]]$coefs[[submodel(none)]]$value
  #
  
  # The model is similar to the one without considering drift, so its results are retrieved in a similar way
  results <- retrieveResults.ignoreDrift(paramsExperiment)
  
  return (results)
}

retrieveResults.modelPerSeason<-function(paramsExperiment) {
  # Load the model and predictions indicated in paramsExperiment and calculate AUC
  #
  #  [[resample]]$coefs[[submodel(season)]]$coef
  #  [[resample]]$coefs[[submodel(season)]]$value
  #
  
  seasons <- getSeasonLevels()
  idxSeason <- 1
  models      <- list(NA,NA,NA,NA)
  predictions <- list(NA,NA,NA,NA)
  paramsExperimentSeason <- paramsExperiment
  for (idxSeason in 1:4) {
    season <- seasons[[idxSeason]]
    
    paramsExperimentSeason$drift <- paste0("ModelPerSeason_",season)
    
    idExperiment <- getIdExperiment(paramsExperimentSeason)
    
    model <- tryCatch({
      loadModelExperiment(paramsExperimentSeason)
    },error = function (e) {
      if (logActive()) doLog(paste0("[ERROR] Error loading model ",idExperiment,": ",e), funcName = "retrieveResults.modelPerSeason")
      return(NA)
    })
    
    pred <- tryCatch({
      loadPredictionExperiment(paramsExperimentSeason)
    },error = function (e) {
      if (logActive()) doLog(paste0("[ERROR] Error loading predictions for ",idExperiment,": ",e), funcName = "retrieveResults.modelPerSeason")
      return(NA)
    })
    
    models[[idxSeason]]     <- model
    predictions[[idxSeason]]<- pred
  }
  
  
  fullPreds<-do.call(rbind,predictions)
  
  idExperiment <- getIdExperiment(paramsExperiment)
  
  modelRoc <- tryCatch({
    calculateROC(fullPreds,paramsExperiment)
  }, error = function(e) {
    if (logActive()) doLog(paste0("[ERROR] Error calculating AUC for ",idExperiment,": ",e), funcName = "retrieveResults.modelPerSeason")
    return(NA)
  })
  
  coefs <- as.list(rep(NA,4))
  for (mod in 1:4) {
    model <- models[[mod]]
    if (!is.na.oneCheck(model)) {
      coefs[[mod]] = getCoefsModel(paramsExperiment, model)
    } else {
      coefs[[mod]] = NA
    }
  }
  
  aucTest <-  NA
  if (!is.na.oneCheck(modelRoc) && !anyNA(predictions,recursive = TRUE)) {
    aucTest  <- modelRoc$auc  
  }
  
  return (list(coefs = coefs, aucTest = aucTest))
  
}

#//////////////////////////////////////////////////////////////////////////
# SLIDING WINDOWS ---------------------------------------------------------
#//////////////////////////////////////////////////////////////////////////


executeExperiment.monthlyWindow <- function(trainData, testData, dateColumn, classColumn, paramsExperiment, loadOnly, window, seed = 1, verbose = FALSE) {
  # This approach uses a circular window centered on the month of the data to predict to create models.
  #
  # Therefore, it creates 12 different models based on the data within each window

  require(lubridate)
  # Prepare the directory for the whole experiment
  dir.create(getDirectoryExperiment(paramsExperiment), showWarnings = FALSE)
  
  # Move all timestamps to year 2000
  dtr2000 <- trainData
  year(dtr2000[[dateColumn]])<-2000
  dtt2000 <- testData
  year(dtt2000[[dateColumn]])<-2000
  
  if(verbose && logActive()) doLog(paste0(getIdExperiment(paramsExperiment)," - Starting monthly circular window algorithm"),funcName = "executeExperiment.monthlyWindow")

  prediction <- NULL
  monthTrain <- month(dtr2000[[dateColumn]])
  monthTest  <- month(dtt2000[[dateColumn]])
  
  paramsExperimentWindow <- paramsExperiment
  models      <- as.list(rep(NA,12))
  predictions <- as.list(rep(NA,12))
  for (idx in 1:12) {
    selectedMonths <- ((idx-1)-((window-1)/2.0)):((idx-1)+((window-1)/2.0))
    selectedMonths <- (selectedMonths %% 12) + 1
    subsetTrain <- dtr2000[monthTrain %in% selectedMonths,]
    subsetTest  <- dtt2000[monthTest==idx,]
    
    if(verbose && logActive()) doLog(paste0(getIdExperiment(paramsExperiment)," ---- Window ",idx," Train: ",summariseDataset(subsetTrain,dateColumn,classColumn), " Test: ",summariseDataset(subsetTest,dateColumn,classColumn) ))
    
    # 'Slide' of the window. We consider that it moves a month forward in each iteration
    paramsExperimentWindow$wSlide <- idx
    
    result <- executeExperiment.ignoreDrift(subsetTrain, subsetTest, dateColumn, classColumn, paramsExperimentWindow, loadOnly, seed = seed, verbose=verbose) 
    
    models[[idx]]     <- result$model
    predictions[[idx]]<- result$prediction
  }

  if (verbose && logActive() && !anyNA(predictions, recursive = TRUE)) doLog(paste0("Experiment ", getIdExperiment(paramsExperiment), " finished successfully."), funcName="executeExperiment.monthlyWindow")

  return (list(model = models, prediction = predictions))
  
}


executeExperiment.seasonalWindow <- function(trainData, testData, dateColumn, classColumn, paramsExperiment, loadOnly, seed = 1, verbose = FALSE) {
  # This approach uses a circular window centered on the season of the data to predict to create models.
  #
  # Therefore, it creates 4 different models based on the data within each window
  
  require(lubridate)
  
  # Prepare the directory for the whole experiment
  dir.create(getDirectoryExperiment(paramsExperiment), showWarnings = FALSE)
  
  # Move all timestamps between [21-03-1999,21-03-2000)
  dtr2000 <- trainData
  dtr2000[[dateColumn]]<-moveDatesTo2000(dtr2000[[dateColumn]])
  dtt2000 <- testData
  dtt2000[[dateColumn]]<-moveDatesTo2000(dtt2000[[dateColumn]])
  
  if(verbose && logActive()) doLog(paste0(getIdExperiment(paramsExperiment)," - Starting seasonal circular window algorithm"), funcName = "executeExperiment.seasonalWindow")
  
  trainWindowIdxs = list()
  # Spring: train data from winter, spring and summer
  trainWindowIdxs[[1]] <- (dtr2000[[dateColumn]] >= winterSolstice1999) | (dtr2000[[dateColumn]] < autumnalEquinox1999)
  # Summer: train data from spring, summer and autumn
  trainWindowIdxs[[2]] <- (dtr2000[[dateColumn]] < winterSolstice1999)
  # Autumn: train data from summer, autumn and winter
  trainWindowIdxs[[3]] <- (dtr2000[[dateColumn]] >= summerSolstice1999)
  # Winter: train data from autumn, winter and spring
  trainWindowIdxs[[4]] <- (dtr2000[[dateColumn]] >= autumnalEquinox1999) | (dtr2000[[dateColumn]] < summerSolstice1999)
  
  testWindowIdxs = list()
  testWindowIdxs[[1]] <- (dtt2000[[dateColumn]] < summerSolstice1999)
  testWindowIdxs[[2]] <- (dtt2000[[dateColumn]] >= summerSolstice1999)  & (dtt2000[[dateColumn]] < autumnalEquinox1999)
  testWindowIdxs[[3]] <- (dtt2000[[dateColumn]] >= autumnalEquinox1999) & (dtt2000[[dateColumn]] < winterSolstice1999)
  testWindowIdxs[[4]] <- (dtt2000[[dateColumn]] >= winterSolstice1999)
  
  paramsExperimentWindow <- paramsExperiment
  models      <- as.list(rep(NA,4))
  predictions <- as.list(rep(NA,4))
  for (idx in 1:4) {
    subsetTrain <- dtr2000[trainWindowIdxs[[idx]],]
    subsetTest  <- dtt2000[testWindowIdxs[[idx]],]
    
    if(verbose && logActive()) doLog(paste0(getIdExperiment(paramsExperiment)," ---- Window ",idx," Train: ",summariseDataset(subsetTrain,dateColumn,classColumn), " Test: ",summariseDataset(subsetTest,dateColumn,classColumn) ))
    
    # 'Slide' of the window. We consider that it moves a month forward in each iteration
    paramsExperimentWindow$wSlide <- idx
    
    result <- executeExperiment.ignoreDrift(subsetTrain, subsetTest, dateColumn, classColumn, paramsExperimentWindow, loadOnly, seed = seed, verbose=verbose) 
    
    models[[idx]]     <- result$model
    predictions[[idx]]<- result$prediction
  }
  
  if (verbose && logActive() && !anyNA(predictions, recursive = TRUE)) doLog(paste0("Experiment ", getIdExperiment(paramsExperiment), " finished successfully."), funcName="executeExperiment.seasonalWindow")
  
  return (list(model = models, prediction = predictions))
}

retrieveResults.slidingWindow <- function(paramsExperiment, nWindows = 12) {
  # Load the model and predictions indicated in paramsExperiment and calculate AUC
  #
  #  [[resample]]$coefs[[submodel(window)]]$coef
  #  [[resample]]$coefs[[submodel(window)]]$value
  #
  
  currentWindow <- 1
  paramsExperimentWindow <- paramsExperiment

  predictions <- as.list(rep(NA,nWindows))
  coefsWindow <- as.list(rep(NA,nWindows))
  for (currentWindow in 1:nWindows) {
    
    paramsExperimentWindow$wSlide <- currentWindow
    
    idExperiment <- getIdExperiment(paramsExperimentWindow)
    
    model <- tryCatch({
      loadModelExperiment(paramsExperimentWindow)
    },error = function (e) {
      if (logActive()) doLog(paste0("[ERROR] Error loading model ",idExperiment,": ",e), funcName = "retrieveResults.slidingWindow")
      return(NA)
    })
    
    predictions[[currentWindow]] <- tryCatch({
      loadPredictionExperiment(paramsExperimentWindow)
    },error = function (e) {
      if (logActive()) doLog(paste0("[ERROR] Error loading predictions for ",idExperiment,": ",e), funcName = "retrieveResults.slidingWindow")
      return(NA)
    })
    
    coefsWindow[[currentWindow]] = getCoefsModel(paramsExperimentWindow, model)
  }
  
  fullPreds<-do.call(rbind,predictions)
  
  modelRoc <- tryCatch({
    calculateROC(fullPreds,paramsExperiment)
  }, error = function(e) {
    if (logActive()) doLog(paste0("[ERROR] Error calculating ROC for ",idExperiment,": ",e), funcName = "retrieveResults.slidingWindow")
    return(NA)
  })
  
  aucTest <-  NA
  if (!is.na.oneCheck(modelRoc) && !anyNA(predictions,recursive = TRUE)) {
    aucTest  <- modelRoc$auc  
  }
  
  return (list(coefs = coefsWindow, aucTest = aucTest))
  
}

#//////////////////////////////////////////////////////////////////////////
# ENSEMBLES ---------------------------------------------------------------
#//////////////////////////////////////////////////////////////////////////

createMonthlyEnsemble <- function (data, dateColumn, classColumn, paramsExperiment, loadOnly, seed = 1, verbose = FALSE) {
  # Given a dataset, creates a ensemble of models with it, one model per month.
  #
  
  require(lubridate)
  require(caret)
  
  dir.create(getDirectoryExperiment(paramsExperiment), showWarnings = FALSE)
  
  data2000 <- data
  year(data2000[[dateColumn]])<-2000
  monthData<- month(data2000[[dateColumn]])
  
  # We select one of the class values to be used as reference for "positive" prediction in all the models
  # glmnet creates models assuming as positive the last factor in alphabetical order, so we follow the same
  # strategy
  lastLevelAlpha =getLastLevelAlpha(data2000[[classColumn]])
  
  # rows de la matriz: modelos
  # cols de la matriz: mes
  tableMSE = matrix(nrow=12,ncol=12)
  models<-list()
  paramsExperimentEnsemble <- paramsExperiment
  for (currentMonth in 1:12) {
    
    idxChunk <- monthData == currentMonth
    
    if (verbose && logActive()) doLog(paste0("- Creating model for month ",currentMonth,": ",summariseDataset(data[idxChunk,],dateColumn, classColumn)), funcName = "createMonthlyEnsemble")
    
    paramsExperimentEnsemble$ensembleChunk <- currentMonth
    
    fileModel <- paste0(getDirectoryExperiment(paramsExperimentEnsemble),"Model_",getIdExperiment(paramsExperimentEnsemble),"_",paramsExperimentEnsemble$dataset)
    
    model <- loadOrExecute(file = fileModel, action = {
      createModel(trainData = data2000[idxChunk,], dateColumn = dateColumn, classColumn = classColumn, paramsExperiment = paramsExperimentEnsemble, seed = seed, verbose = verbose)
    }, loadOnly = loadOnly)
    
    if (is.na.oneCheck(model)) {
      if (logActive()) doLog(paste0("[ERROR] Unable to create model for experiment ", getIdExperiment(paramsExperimentEnsemble)), funcName = "createMonthlyEnsemble")
      return(NA)
    }
    
    models[[currentMonth]]<-model
    
    # We obtaind the RMSE of the model when predicting data from each different month
    for(idxPredMonth in 1:12) {
      
      idxPredChunk <- monthData == idxPredMonth
      
      
      predChunk <- doPredict(data[idxPredChunk, ], dateColumn, classColumn, model, paramsExperimentEnsemble)
      
      obs <- ifelse(predChunk$observation==lastLevelAlpha,1.0,0.0)
      rmse <- caret::postResample(predChunk$prediction,obs)["RMSE"]
      
      tableMSE[currentMonth,idxPredMonth] <- rmse
    }
    
  }
  
  # Finally, we estimate the weight of each model when predicting over each month according to its RMSE
  invMSE <- matrix(unlist(lapply(tableMSE,function(mse) { ifelse(mse!=0,1.0/mse,1.0/0.000000001) })),nrow=12,ncol=12)
  sumInvMSEMonth<- unlist((lapply(1:12, function(monthId) { sum(invMSE[,monthId])})))
  w <-matrix(unlist(lapply(1:12,function(monthId) {invMSE[,monthId]/sumInvMSEMonth[monthId]})),nrow=12,ncol=12)
  
  
  ensembleModel = list(chunkMSE = tableMSE, models=models, w = w)
  
  return (ensembleModel)
}  

predict.monthlyEnsemble <- function(data,ensemble, dateColumn, classColumn, paramsExperiment){
  # Use the ensemble model in  'ensemble' to estimate the class in 'data'
  #
  require(rubridate)
  
  dtt2000 <- data
  year(dtt2000[[dateColumn]])<-2000
  
  preds <- NULL
  obs <- NULL
  monthData <- month(dtt2000[[dateColumn]])
  for (monthId in 1:12) {
    
    subsetData <- dtt2000[monthData==monthId,]
    
    paramsExperiment$ensembleChunk <- monthId
    
    partialPred = rep(0,nrow(subsetData))
    for (modelId in 1:12) {
      modelPred <- doPredict(subsetData, dateColumn, classColumn, ensemble$models[[monthId]], paramsExperiment)
      
      partialPred <- partialPred+ ensemble$w[modelId,monthId]*modelPred$prediction
    }
    preds <- c(preds,partialPred)
    obs <- c(obs,as.character(modelPred$observation))
  }
  obs <- as.factor(obs)
  
  prediction <- data.frame(prediction = preds, observation = obs)
  
  return(prediction)
}

executeExperiment.monthlyEnsemble<-function(trainData, testData, dateColumn, classColumn, paramsExperiment, loadOnly, seed=1, verbose = FALSE) {
  # Create an ensemble of models. It will have a model per month, trained only with data from the particular month.
  #

  # Prepare the directory for the whole experiment
  dir.create(getDirectoryExperiment(paramsExperiment), showWarnings = FALSE)
  
  fileEnsemble <- paste0(getDirectoryExperiment(paramsExperiment),"Ensemble_",getIdExperiment(paramsExperiment),"_",paramsExperiment$dataset)
  filePred     <- paste0(getDirectoryExperiment(paramsExperiment),"Predi_",getIdExperiment(paramsExperiment),"_",paramsExperiment$dataset)
  
  ensembleModel <- loadOrExecute(file = fileEnsemble, action = {
    createMonthlyEnsemble(trainData,dateColumn,classColumn,paramsExperiment,loadOnly,seed,verbose)
  }, loadOnly = loadOnly)
  
  predictions   <- loadOrExecute(file = filePred, action = {
    predict.monthlyEnsemble(testData,ensembleModel,dateColumn,classColumn,paramsExperiment)
  }, loadOnly = loadOnly)
  
  
  if (verbose && logActive() && !anyNA(predictions, recursive = TRUE)) doLog(paste0("Experiment ", getIdExperiment(paramsExperiment), " finished successfully."), funcName="executeExperiment.monthlyEnsemble")
  
  return (list(model = ensembleModel, prediction = predictions))
}

createSeasonalEnsemble <- function (data, dateColumn, classColumn, paramsExperiment, loadOnly, seed = 1, verbose = FALSE) {
  # Create an ensemble of models, one model per season
  #
  require(lubridate)
  require(caret)
  
  dir.create(getDirectoryExperiment(paramsExperiment), showWarnings = FALSE)
  
  # Change the year of each date so as to move them between [1999-03-21,2000-03-21)
  data2000 <- data
  flattenDate <- moveDatesTo2000(data2000[[dateColumn]])

  # Calculate the indexes of data that belongs to each season
  chunkIdxs = list()
  chunkIdxs[[1]] <- (flattenDate < summerSolstice1999)
  chunkIdxs[[2]] <- (flattenDate >= summerSolstice1999) & (flattenDate < autumnalEquinox1999)
  chunkIdxs[[3]] <- (flattenDate >= autumnalEquinox1999) & (flattenDate < winterSolstice1999)
  chunkIdxs[[4]] <- (flattenDate >= winterSolstice1999)
  
  # We select one of the class values to be used as reference for "positive" prediction in all the models
  # glmnet creates models assuming as positive the last factor in alphabetical order, so we follow the same
  # strategy
  lastLevelAlpha =getLastLevelAlpha(data2000[[classColumn]])
  
  tableMSE = matrix(nrow=4,ncol=4)
  models<-list()
  paramsExperimentEnsemble <- paramsExperiment
  for (currentSeason in 1:4) {
    
    idxChunk <- chunkIdxs[[currentSeason]]
    
    if (verbose && logActive()) doLog(paste0("- Season ",currentSeason,": ",summariseDataset(data[idxChunk,],dateColumn, classColumn)), funcName = "creaEnsembleSeason")
    
    paramsExperimentEnsemble$ensembleChunk <- currentSeason
    fileModel <- paste0(getDirectoryExperiment(paramsExperimentEnsemble),"Model_",getIdExperiment(paramsExperimentEnsemble),"_",paramsExperimentEnsemble$dataset)

    model <- loadOrExecute(file = fileModel, action = {
      createModel(trainData = data2000[idxChunk,], dateColumn = dateColumn, classColumn = classColumn, paramsExperiment = paramsExperimentEnsemble, seed = seed, verbose = verbose)
    }, loadOnly = loadOnly)
    
    if (is.na.oneCheck(model)) {
      if (logActive()) doLog(paste0("[ERROR] Unable to create the model for the experiment ", getIdExperiment(paramsExperimentEnsemble)), funcName = "creaEnsembleSeason")
      return(NA)
    }
    
    models[[currentSeason]]<-model
    
    # Test the model with data from the four seasons
    for(idxPredSeason in 1:4) {
      predChunk <- doPredict(data[chunkIdxs[[idxPredSeason]], ], dateColumn, classColumn, model, paramsExperimentEnsemble)
      
      obs <- ifelse(predChunk$observation==lastLevelAlpha,1.0,0.0)
      rmse <- caret::postResample(predChunk$prediction,obs)["RMSE"]
      
      tableMSE[currentSeason,idxPredSeason] <- rmse
    }
  } 

  # Finally, we estimate the weight of each model when predicting over each season according to its RMSE
  invMSE <- matrix(unlist(lapply(tableMSE,function(mse) { ifelse(mse!=0,1.0/mse,1.0/0.000000001) })),nrow=4,ncol=4)
  sumInvMSESeason<- unlist((lapply(1:4, function(seasonId) { sum(invMSE[,seasonId])})))
  w <-matrix(unlist(lapply(1:4,function(seasonId) {invMSE[,seasonId]/sumInvMSESeason[seasonId]})),nrow=4,ncol=4)
  
  ensembleModel = list(chunkMSE = tableMSE, models=models, w=w)
  
  return (ensembleModel)
  
}  

predict.ensembleSeason <- function(data,ensemble, dateColumn, classColumn, paramsExperiment){
  # Use the ensemble model in  'ensemble' to estimate the class in 'data'
  #
  
  # Change the year of each date so as to move them between [1999-03-21,2000-03-21)
  dtt2000 <- data
  flattenDate <- moveDatesTo2000(dtt2000[[dateColumn]])
  
  # Calculate the indexes of data that belongs to each season
  chunkIdxs = list()
  chunkIdxs[[1]] <- (flattenDate < summerSolstice1999)
  chunkIdxs[[2]] <- (flattenDate >= summerSolstice1999) & (flattenDate < autumnalEquinox1999)
  chunkIdxs[[3]] <- (flattenDate >= autumnalEquinox1999) & (flattenDate < winterSolstice1999)
  chunkIdxs[[4]] <- (flattenDate >= winterSolstice1999)
  

  preds <- NULL
  obs <- NULL
  paramsExperimentEnsemble<-paramsExperiment
  for (seasonId in 1:4) {
    
    subsetData <- dtt2000[chunkIdxs[[seasonId]],]
    
    paramsExperimentEnsemble$ensembleChunk <- seasonId
    
    partialPred = rep(0,nrow(subsetData))
    for (modelId in 1:4) {
      modelPred <- doPredict(subsetData, dateColumn, classColumn, ensemble$models[[seasonId]], paramsExperimentEnsemble)
      
      partialPred <- partialPred+ ensemble$w[modelId,seasonId]*modelPred$prediction
    }
    preds <- c(preds,partialPred)
    obs <- c(obs,as.character(modelPred$observation))
  }
  obs <- as.factor(obs)
  
  prediction <- data.frame(prediction = preds, observation = obs)
  
  return(prediction)
}

executeExperiment.seasonalEnsemble<-function(trainData, testData, dateColumn, classColumn, paramsExperiment, loadOnly, seed=1, verbose = FALSE) {
  # Create an ensemble of models based on trainData, one per season, and use it to predict testData
  #
  #

  dir.create(getDirectoryExperiment(paramsExperiment), showWarnings = FALSE)
  
  fileEnsemble <- paste0(getDirectoryExperiment(paramsExperiment),"Ensemble_",getIdExperiment(paramsExperiment),"_",paramsExperiment$dataset)
  filePred     <- paste0(getDirectoryExperiment(paramsExperiment),"Predi_",getIdExperiment(paramsExperiment),"_",paramsExperiment$dataset)
  
  ensembleModel <- loadOrExecute(file = fileEnsemble, action = {
    createSeasonalEnsemble(trainData,dateColumn,classColumn,paramsExperiment,loadOnly,seed,verbose)
  }, loadOnly = loadOnly)
  
  predictions <- loadOrExecute(file = filePred, action = {
    predict.ensembleSeason(testData,ensembleModel,dateColumn,classColumn,paramsExperiment)
  }, loadOnly = loadOnly)
  
  
  if (verbose && logActive() && !anyNA(predictions, recursive = TRUE)) doLog(paste0("Experiment ", getIdExperiment(paramsExperiment), " finished successfully."), funcName="executeExperiment.seasonalEnsemble")
  
  return (list(model = ensembleModel, prediction = predictions))

}


retrieveResults.ensemble <- function(paramsExperiment) {
  # Load the model and predictions indicated in paramsExperiment and calculate AUC
  #
  #  [[resample]]$coefs[[submodel(month/season)]]$coef
  #  [[resample]]$coefs[[submodel(month/season)]]$value
  #
  #  
  
  require(lubridate)
  
  coefs    <- NULL
  auc      <- NULL
  
  idExperiment <- getIdExperiment(paramsExperiment)
  
  if (!dir.exists(getDirectoryExperiment(paramsExperiment))) {
    if (logActive()) doLog(paste0("Unable to find the directory of the experiment ",idExperiment), funcName = "retrieveResults.ensemble")
    return(NA)
  }
  
  predictions <- NULL
  
  fileEnsemble <- paste0(getDirectoryExperiment(paramsExperiment),"Ensemble_",idExperiment,"_",paramsExperiment$dataset)
  ensembleModel <- loadOrExecute(file = fileEnsemble, action = {}, loadOnly = TRUE)
  
  if (is.na.oneCheck(ensembleModel)) {
    if (logActive()) doLog(paste0("unable to load model ",idExperiment, " file: ",fileEnsemble))
    return(NA)
  }
  
  coefsEnsemble <- as.list(rep(NA,length(ensembleModel$models)))
  for (mod in 1:length(ensembleModel$models)) {
    model <- ensembleModel$models[[mod]]
    if (!is.na.oneCheck(model)) {
      coefsEnsemble[[mod]] = getCoefsModel(paramsExperiment, model)
    } else {
      coefsEnsemble[[mod]] = NA
    }
  }
  
  predictions <- tryCatch({
    loadPredictionExperiment(paramsExperiment)
  },error = function (e) {
    if (logActive()) doLog(paste0("[ERROR] Unable to load predictions for ",idExperiment,": ",e), funcName = "retrieveResults.ensemble")
    return(NA)
  })
  
  modelRoc <- tryCatch({
    calculateROC(predictions,paramsExperiment)
  }, error = function(e) {
    if (logActive()) doLog(paste0("[ERROR] Unable to calculate ROC for ",idExperiment,": ",e), funcName = "retrieveResults.ensemble")
    return(NA)
  })
  
  aucTest <-  NA
  if (!is.na.oneCheck(modelRoc)) {
    aucTest  <- modelRoc$auc  
  }
  
  return (list(coefs = coefsEnsemble, aucTest = aucTest))
}