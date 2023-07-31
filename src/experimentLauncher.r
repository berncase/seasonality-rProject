cat("!!!!! IMPORTANT NOTE - DISCLAIMER !!!!!!\r\n")
cat("THE EXECUTION OF THIS SCRIPT WILL SAVE A HUGE NUMBER OF .rds FILES IN\r\n")
cat("THE DIRECTORY INDICATED IN THE modelsPath VARIABLE (defined in code)\r\n")
cat("MOREOVER, IT GENERATES MULTI-THREADED TASKS TO PERFORM EXPERIMENTS THAT MAY\r\n")
cat("OVERLOAD THE COMPUTER!!!!!!!\r\n")
cat("USE AT YOUR OWN RISK!!!!\r\n")
cat("\r\n") 
cat("See more details about the methods employed in the experiments in the paper:\r\n")
cat("\r\n")
cat("Cánovas-Segura B, Morales A, Juarez JM, Campos M.\r\n")
cat("Approaches for Dealing with Seasonality in Clinical Prediction Models for Infections.\r\n")
cat("Applied Sciences. 2023; 13(14):8317. https://doi.org/10.3390/app13148317")
cat("\r\n")

source("src/functions/utils.r")
source("src/functions/preprocessing.r")
source("src/functions/framework.r")
source("src/functions/prettyprint.r")

library("caret")

# -- Configuration of experiments --------------

# Folder with the source datasets
datasetPath <- "data/"

# Folder in which the results of the experiments will be stored
modelsPath  <- "models/" 


# Codes of the imbalance methods to be tested. See more details on the reference paper
# imbalanceMethods  <- c("none","under1","under2","over1","over2")
# In this script, we only use three of them for simplicity
imbalanceMethods  <- c("none", "over1")

## Codes of the feature selection methods to be tested. See more details on the reference paper
# fSelectionMethods <- c("none","pvalue05","fcbf")
# In this script, we only use three of them for simplicity
fSelectionMethods <- c("none","fcbf")

## Codes of the base modelling techniques to be used. See more details on the reference paper
models            <- c("glmnet","C50")

## Codes of the method tested to deal with seasonality. See more details on the reference paper
#driftMethods      <- c("none","seasonAsFeature","modelPerSeason","monthlyWindow03","monthlyWindow05","monthlyWindow07","monthlyWindow09","seasonalWindow","monthlyEnsemble","seasonalEnsemble")
# In this script, we only use some of them for simplicity
driftMethods      <- c("none","seasonAsFeature","modelPerSeason","monthlyWindow05","seasonalEnsemble")

# Name of the files with the datasets. the extension '.rdf' will we added automatically
#datasets     <- c("synth_condensed","synth_sinusoidal","mimic_acinetobacter","mimic_spneumoniae")
# We cannot share data extracted from MIMICS, therefore only the synthetic datasets are included in this script
datasets     <- c("synth_sinusoidal")

# Columns in the datasets with the outcome class and the timestamp
#classColumns <- c("class","class","class","class")
#dateColumns  <- c("test_date","test_date","test_time","test_time")
classColumns <- c("class","class")
dateColumns  <- c("test_date")


# Number of resamples done to the train/validation dataset
# In the original paper, 100 resamples were performed. This means a HUGE amount of models and processing time
# nBootstraps <- 100 
# We reduced them in this sample
nBootstraps <- 5


# Number of cores to use. Set to 1 to do not use parallelization 
nCores = 6

# -- Warning message ----------------------


cat("Number of experiments to perform:\r\n")
cat(" - Methods to deal with imbalance:\r\n")
cat(paste0('    + ', ppName(imbalanceMethods), ""), sep = '\r\n')
cat(" - Methods for feature selection:\r\n")
cat(paste0('    + ', ppName(fSelectionMethods), ""), sep = '\r\n')
cat(" - Methods for dealing with seasonality:\r\n")
cat(paste0('    + ', ppName(driftMethods), ""), sep = '\r\n')
cat(" - Base modelling techniques:\r\n")
cat(paste0('    + ', ppName(models), ""), sep = '\r\n')
cat(" - Datasets to use:\r\n")
cat(paste0('    + ', ppName(datasets), ""), sep = '\r\n')
cat(paste(' - Number of resamples per experiment:', nBootstraps, '\r\n'))
cat('\r\n')
cat('\r\n')
cat(paste("A total number of", length(imbalanceMethods)*length(fSelectionMethods)*length(driftMethods)*length(models)*nBootstraps, "experiments/models are going to be generated on", normalizePath(modelsPath),"\r\n"))
userContinue <- readline("Are your sure you want to continue? Enter 'y' to proceed, any other thing to stop.")
if (userContinue!='y') stop("Exiting without doing experiments.")

# -- Log configuration --------------------
#
# The errors that may appear during the execution of the experiments can be stored
# in a log within the 'modelsPath' directory. 

logFile = paste0(modelsPath,"executionLog_",format(Sys.time(),format="%Y%m%d%H%M"),".log")

# Comment this line to disable logging
doLog("Starting experiment execution",logFile = logFile)


# -- Experiment initialization ------------

paramsExperiment = list(
  imbalance   = "none", 
  drift       = "none", 
  fsel        = "none", 
  model       = "glmnet", 
  modelsPath  = modelsPath,
  dataset     = NA, 
  it          = NA,
  wSlide      = NA,
  ensembleChunk = NA)


# -- Experiment loop -----------------------

for (idxDataset in 1:length(datasets)) {
  dataFile <- datasets[idxDataset]
  dataset <- readRDS(paste0(datasetPath,dataFile,".rds"))
  
  dateColumn  <- dateColumns[[idxDataset]]
  classColumn <- classColumns[[idxDataset]]
  
  set.seed(1)
  trainIndex <- createDataPartition(dataset[[classColumn]], p=.8,list = FALSE, times = 1)
  dtrain <- dataset[trainIndex,]
  dtest <- dataset[-trainIndex,]
  
  paramsExperiment$dataset = dataFile
  
  for (m in 1:length(models)) {
    for (d in 1:length(driftMethods)) {
      for (i in 1:length(imbalanceMethods)) {
        for (f in 1:length(fSelectionMethods))  {
          paramsExperiment$model     = models[[m]]
          paramsExperiment$imbalance = imbalanceMethods[[i]]
          paramsExperiment$drift     = driftMethods[[d]]
          paramsExperiment$fsel      = fSelectionMethods[[f]]
          
          print(paste0("[",Sys.time(),"] Executing experiment ",getIdExperiment(paramsExperiment), " - ",paramsExperiment$dataset))
          gc()
          startTime <- Sys.time()
          if (nCores!=1) {
            results <- executeExperiment.bootstrap.parallel(nBootstraps, trainData = dtrain, testData = dtest, dateColumn = dateColumn, classColumn = classColumn, paramsExperiment, nCores = nCores, verbose = FALSE)
          } else {
            results <- executeExperiment.bootstrap(nBootstraps, trainData = dtrain, testData = dtest, dateColumn = dateColumn, classColumn = classColumn, paramsExperiment, verbose = FALSE)  
          }
          gc()
          
          if (!anyNA(results, recursive = TRUE)) {
            print(paste0("[",Sys.time(),"] ",getIdExperiment(paramsExperiment), "-",paramsExperiment$dataset," Ok! - Elapsed time: ",format(Sys.time()-startTime,format="%H:%M:%S")))
            doLog(paste0("---- Experiment ",getIdExperiment(paramsExperiment)," finished. Elapsed time: ",format(Sys.time()-startTime,format="%H:%M:%S")))
          } else {
            warning(paste0("[",Sys.time(),"] ",getIdExperiment(paramsExperiment), "-",paramsExperiment$dataset," ERRORS DETECTED - SEE LOG FOR DETAILS!!!  - Elapsed time: ",format(Sys.time()-startTime,format="%H:%M:%S")),immediate. = TRUE)
            doLog(paste0("---- Experiment ",getIdExperiment(paramsExperiment)," FAILED. Elapsed time: ",format(Sys.time()-startTime,format="%H:%M:%S")))
          }
        }
      }
    }
  }
}

