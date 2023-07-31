##
# Functions to generate 
#
#
# Requires
# sourcing of "functions/utils.r"


analyseDataset <- function(dataset, classColumn) {
  ###
  #
  # Generates a data frame with the analysis of the features of a dataset (number of observations per values in factors, mean, sd in numeric features, etc.)
  # for each class level
  #
  # dataset       : Data from the dataset to be analised
  # classColumn   : Name of the column that contains the class
  # 
  #
  att      <- c()
  attValue <- c()
  cl1Mean  <- c()
  cl1Sd    <- c()
  cl2Mean  <- c()
  cl2Sd    <- c()

  l <- levels(dataset[[classColumn]]) 
  idx<-1
  for (colname in colnames(dataset)) {
    col<-dataset[[colname]]
    if (is.logical(col)) { col <- as.factor(col) }
    if (is.factor(col)) {
      t<-table(dataset[[classColumn]],col)
      for (f in levels(col)) {
        num1<-t[l[1],f]
        prc1<-num1/sum(t[l[1],])
        num2<-t[l[2],f]
        prc2<-num2/sum(t[l[2],])
        att[idx]     <- colname
        attValue[idx]<- f
        cl1Mean[idx] <- sprintf("%d",num1)
        cl1Sd[idx]   <- sprintf("%1.2f%%",prc1*100)
        cl2Mean[idx] <- sprintf("%d",num2)
        cl2Sd[idx]   <- sprintf("%1.2f%%",prc2*100)
        idx <- idx + 1
      }
    } else if (is.numeric(col)) {
      m1=mean(col[dataset[[classColumn]]==l[1]])
      s1=sd(col[dataset[[classColumn]]==l[1]])
      m2=mean(col[dataset[[classColumn]]==l[2]])
      s2=sd(col[dataset[[classColumn]]==l[2]])
      att[idx]     <- colname
      attValue[idx]<- "numeric"
      cl1Mean[idx] <- sprintf("%1.2f",m1)
      cl1Sd[idx]   <- sprintf("%1.2f",s1)
      cl2Mean[idx] <- sprintf("%1.2f",m2)
      cl2Sd[idx]   <- sprintf("%1.2f",s2)
      idx <- idx + 1
    } else {
      att[idx]     <- colname
      attValue[idx]<- "IGNORED"
      cl1Mean[idx] <- 0
      cl1Sd[idx]   <- 0
      cl2Mean[idx] <- 0
      cl2Sd[idx]   <- 0
    }
  }
  return(data.frame(Feaure=att,Value=attValue,MeanCl1=cl1Mean,SdCl1=cl1Sd,MeanCl2=cl2Mean,SdCl2=cl2Sd))
}


characteriseDataset<-function(dataModel,classColumn) {
  ###
  #
  # Generates a string with the analysis of the features of a dataset (number of observations per values in factors, mean, sd in numeric features, etc.)
  # for each class level. Columns are separated by \t
  #
  # dataset       : Data from the dataset to be analised
  # classColumn   : Name of the column that contains the class
  # 
  #  
  l <- levels(dataModel[[classColumn]]) 
  output<-paste("\t",l,collapse = "\t")
  output<-paste0(output,"\r\n")
  output<-paste0(output,"\tCount/Mean\t%/Sd\tCount/Mean\t%/Sd\r\n")
  
  for (colname in colnames(dataModel)) {
    col<-dataModel[[colname]]
    if (is.logical(col)) { col <- as.factor(col) }
    if (is.factor(col)) {
      t<-table(dataModel[[classColumn]],col)
      output<-paste0(output,colname,"\t\t\t\t\r\n")
      for (f in levels(col)) {
        num1<-t[l[1],f]
        prc1<-num1/sum(t[l[1],])
        num2<-t[l[2],f]
        prc2<-num2/sum(t[l[2],])
        output<-paste0(output,sprintf(" - %s \t %d \t %1.2f %% \t %d \t %1.2f %% \r\n",f,num1,prc1,num2,prc2))
      }
    } else if (is.numeric(col)) {
      m1=mean(col[dataModel[[classColumn]]==l[1]])
      s1=sd(col[dataModel[[classColumn]]==l[1]])
      m2=mean(col[dataModel[[classColumn]]==l[2]])
      s2=sd(col[dataModel[[classColumn]]==l[2]])
      output<-paste0(output,sprintf("%s \t %1.2f \t %1.2f \t %1.2f \t %1.2f \r\n",colname,m1,s1,m2,s2))
    } else {
      output<-paste0(output,sprintf("IGNORED COLUMN: %s  \r\n",colname))
    }
  }
  return(output)
}  


characteriseDatasetLatex<-function(dataModel, classColumn,datasetName = "NO NAME") {
  ###
  #
  # Generates a string in Latex format with the analysis of the features of a dataset (number of observations per values in factors, mean, sd in numeric features, etc.)
  # for each class level
  #
  # dataset       : Data from the dataset to be analised
  # classColumn   : Name of the column that contains the class
  # 
  #    
  l <- levels(dataModel[[classColumn]])
  output<-sprintf(" & & \\multicolumn{4}{c}{%s} \\\\\r\n",datasetName)
  output<-paste0(output,sprintf(" & & \\multicolumn{2}{c}{%s} & \\multicolumn{2}{c}{%s} \\\\\r\n",l[1],l[2],l[1],l[2]))
  output<-paste0(output," & & Count/Mean & \\%/Sd & Count/Mean & \\%/Sd  \\\\\r\n")
  
  for (colname in colnames(dataModel)) {
    col<-dataModel[[colname]]
    if (is.logical(col)) { col <- as.factor(col) }
    if (is.factor(col)) {
      t<-table(dataModel[[classColumn]],col)
      output<-paste0(output,sprintf("\\multicolumn{2}{l}{%s} & & & & \\\\\r\n",colname))
      for (f in levels(col)) {
        num1<-t[l[1],f]
        prc1<-(num1/sum(t[l[1],]))*100
        num2<-t[l[2],f]
        prc2<-(num2/sum(t[l[2],]))*100
        output<-paste0(output,sprintf(" - & %s & %d & %1.2f \\%% & %d & %1.2f \\%% \\\\\r\n",f,num1,prc1,num2,prc2))
      }
    } else if (is.numeric(col)) {
      m1=mean(col[dataModel[[classColumn]]==l[1]])
      s1=sd(col[dataModel[[classColumn]]==l[1]])
      m2=mean(col[dataModel[[classColumn]]==l[2]])
      s2=sd(col[dataModel[[classColumn]]==l[2]])
      output<-paste0(output,sprintf("\\multicolumn{2}{l}{%s} & %1.2f & %1.2f & %1.2f & %1.2f \\\\\r\n",colname,m1,s1,m2,s2))
    } else {
      output<-paste0(output,sprintf("IGNORED COLUMN: %s  \r\n",colname))
    }
  }
  return(gsub("_","\\\\_",output))
}

ppName<-function(name) {
  ###
  #
  # Generates a "human readable" description for different string "codes" used
  # in the experiments
  # 
  #  
  ifelse(name == "none"             ,   "None"       ,
  ifelse(name == "seasonAsFeature"  ,   "Season as feature"  ,
  ifelse(name == "modelPerSeason"   ,   "Model per season"   ,
  ifelse(name == "seasonalWindow"   ,   "Seasonal window"    ,
  ifelse(name == "monthlyWindow03"  ,   "3-month window"    ,
  ifelse(name == "monthlyWindow05"  ,   "5-month window"    ,
  ifelse(name == "monthlyWindow07"  ,   "7-month window"    ,
  ifelse(name == "monthlyWindow09"  ,   "9-month window"    ,
  ifelse(name == "monthlyEnsemble"  ,   "Monthtly ensemble"  ,
  ifelse(name == "seasonalEnsemble" ,   "Seasonal ensemble"  ,
  ifelse(name == "under1"           ,   "Undersample 1:1"    ,
  ifelse(name == "under2"           ,   "Undersample 2:1"    ,
  ifelse(name == "over1"            ,   "Oversample 1:1"     ,
  ifelse(name == "over2"            ,   "Oversample 2:1"     ,
  ifelse(name == "pvalue05"         ,   "p-Value"  ,
  ifelse(name == "fcbf"             ,   "FCBF"  ,
  ifelse(name == "synth_condensed"     ,   "Synthetic - Condensed"  ,
  ifelse(name == "synth_sinusoidal"    ,   "Synthetic - Sinusoidal"  ,
  ifelse(name == "mimic_acinetobacter" ,   "MIMIC III - *Acinetobacter spp.*"  ,
  ifelse(name == "mimic_spneumoniae"   ,   "MIMIC III - *S. pneumoniae*"  ,
  ifelse(name == "FSel"             ,   "Filter"  ,
  ifelse(name == "Balancing"        ,   "Balancing"  ,
  ifelse(name == "Drift"            ,   "Seasonal drift approach"  ,
  ifelse(name == "CombinationAUC"   , "Mean AUC (95\\% Conf.Int.)",
  ifelse(name == "CombinationNCoef" , "Mean features per model (95\\% Conf.Int.)",
  ifelse(name == "glmnet" , "LR+LASSO",
  ifelse(name == "C50" , "C5.0",
         name
  )))))))))))))))))))))))))))
}

ppNumber <- function(x){
  # Format number for nice displaying 
  return(format(round(x,3),scientific=FALSE,digits=3,nsmall=3))
}

genKnitrFullTable <- function(tableToPrint, caption) {
  require(kableExtra)
  ncolExtra <- ncol(tableToPrint)-2
  knitr::kable(tableToPrint,"latex",caption = caption, booktabs=T, 
      linesep=c("","","","","\\addlinespace"),
      row.names = FALSE,digits = 2) %>% 
      add_header_above(c(" " = 2,"Seasonal drift approach" = ncolExtra)) %>% 
      kable_styling(latex_options = "striped", font_size = 6)
}

genKnitrRankingTable <- function(tableToPrint, caption,nTops =10) {
  require(kableExtra)
  knitr::kable(tableToPrint,"latex",caption = caption, booktabs=T, 
               linesep=c(rep("",nTops-1),"\\addlinespace","\\addlinespace"),
               row.names = FALSE,digits = 2) %>% 
    kable_styling(latex_options = "striped", font_size = 6)
}


genKnitrDatasetTable <- function(tableToPrint, caption) {
  require(kableExtra)
  finalData <- tableToPrint
  finalData[[1]]<-NULL
  headerClass <- c(1,2,2)
  names(headerClass) <- c(" ",rev(tableToPrint[[2]])[2],rev(tableToPrint[[2]])[1]) # The class levels are at the end of the second column
  knitr::kable(finalData,"latex",caption = caption, booktabs=T, 
               row.names = FALSE,digits = 2) %>% 
       kable_styling(font_size = 6) %>%
       pack_rows(index = table(tableToPrint[[1]])[unique(tableToPrint[[1]])]) %>% # With the 'unique' we order the table results in the same way that the dataset
       add_header_above(c(" ","Mean/Count","Sd/Prc","Mean/Count","Sd/Prc")) %>%
       add_header_above(headerClass)
}

genKnitrDoubleDatasetTable <- function(tableToPrint1, tableToPrint2, datasetName1, datasetName2,caption) {
  require(kableExtra)
  names(tableToPrint2) <- c("Feature","Value","MeanCl12","SdCl12","MeanCl22","SdCl22")
  tableToPrint1$key <- paste0(tableToPrint1[[1]],"-",tableToPrint1[[2]])
  tableToPrint2$key <- paste0(tableToPrint2[[1]],"-",tableToPrint2[[2]])
  tableToPrint<-full_join(tableToPrint1,tableToPrint2,by="key")
  finalData <- tableToPrint
  finalData$key <- NULL
  finalData[[1]]<-NULL 
  finalData[[6]]<-NULL # Notice that we removed a column previously, so the index of columns changed
  finalData[[6]]<-NULL  # Notice that we removed a column previously, so the index of columns changed
  headerClass <- c(1,2,2,2,2)
  names(headerClass) <- c(" ",rev(tableToPrint[[2]])[2],rev(tableToPrint[[2]])[1],rev(tableToPrint[[2]])[2],rev(tableToPrint[[2]])[1]) # The class levels are at the end of the second column
  headerDataset <- c(1,4,4)
  names(headerDataset) <- c(" ",datasetName1,datasetName2) # The class levels are at the end of the second column
  knitr::kable(finalData,"latex",caption = caption, booktabs=T, 
               row.names = FALSE,digits = 2) %>% 
    kable_styling(font_size = 6) %>%
    pack_rows(index = table(tableToPrint[[1]])[unique(tableToPrint[[1]])]) %>% # With the 'unique' we order the table results in the same way that the dataset
    add_header_above(c(" ","Mean/Count","Sd/Prc","Mean/Count","Sd/Prc","Mean/Count","Sd/Prc","Mean/Count","Sd/Prc")) %>%
    add_header_above(headerClass) %>%
    add_header_above(headerDataset)
  
}

loadCompleteResults <- function(nBootstraps, modelsPath, modelMethods, driftMethods, imbalanceMethods, fSelectionMethods, datasetNames, aucType = "standard") {
# This function load all the results of the indicated experiments and
# generates a data frame with all of them 
  
  numElements = nBootstraps*length(datasetNames)*length(modelMethods)*length(driftMethods)*length(imbalanceMethods)*length(fSelectionMethods)
  resModels   <- rep(NA,numElements)
  resFeat     <- rep(NA,numElements)
  resBalance  <- rep(NA,numElements)
  resDrift    <- rep(NA,numElements)
  resDataset  <- rep(NA,numElements)
  resResample <- rep(NA,numElements)
  resAuc      <- rep(NA,numElements)
  resNCoef    <- rep(NA,numElements)
  
  m <- 1
  df<-1
  d<-1
  i<-1
  f<-3
  
  idx<-1
  for (df in 1:length(datasetNames)) {
    for (m in 1:length(modelMethods)) {
      for (d in 1:length(driftMethods)) {
        for (i in 1:length(imbalanceMethods)) {
          for (f in 1:length(fSelectionMethods))  {
            paramsExperiment <- list(
              modelsPath  = modelsPath,
              imbalance   = imbalanceMethods[[i]],
              drift       = driftMethods[[d]],
              fsel        = fSelectionMethods[[f]],
              model       = modelMethods[[m]],
              dataset     = datasetNames[[df]],
              aucType     = aucType)
            
            #print(paste0("f:", f," i:",i, " d:",d," df:",df))
            gc()
            results = retrieveResults.bootstrap(nBootstraps, paramsExperiment)
            #results = retrieveResults.bootstrap.parallel(nBootstraps, paramsExperiment,nCores = nCores)
            gc()
            
            for (it in 1:nBootstraps) {
              resModels[[idx]]   <- modelMethods[[m]]
              resFeat[[idx]]     <- fSelectionMethods[[f]]
              resBalance[[idx]]  <- imbalanceMethods[[i]]
              resDrift[[idx]]    <- driftMethods[[d]]
              resDataset[[idx]]  <- datasetNames[[df]]
              resResample[[idx]] <- it
              
              if ((!is.na.oneCheck(results)) && (!is.na(results[[it]])) && (!is.na.oneCheck(results[[it]]$aucTest))) {
                resAuc[[idx]]   <- results[[it]]$aucTest
                if (!is.na.oneCheck(results[[it]]$coefs)) {
                  totalModels <- length(results[[it]]$coefs)
                  if (totalModels==0) {
                    resNCoef[[idx]] <- NA
                  } else {
                    if (paramsExperiment$model=="glmnet") {
                      resNCoef[[idx]] <- sum(sapply(results[[it]]$coefs,function(x) {length(x$coef)-1}))/totalModels # We substract 1 to the number of coefs to discard the intercept as feature
                    } else {
                      resNCoef[[idx]] <- sum(sapply(results[[it]]$coefs,function(x) {length(x$coef)}))/totalModels
                    }
                  }
                }
              } else {
                resAuc[[idx]] <- NA
                resNCoef[[idx]] <- NA
              }
              idx <- idx +1
            }
          }
        }
      }
    }
  }
  completeResults           <- data.frame(Model = resModels, FSel = resFeat,Balancing = resBalance,Drift = resDrift,dataset = resDataset,resample = resResample,auc = resAuc, ncoef = resNCoef)
  completeResults$Model     <- factor(completeResults$Model, levels = modelMethods)
  completeResults$FSel      <- factor(completeResults$FSel, levels = fSelectionMethods)
  completeResults$Balancing <- factor(completeResults$Balancing, levels = imbalanceMethods)
  completeResults$Drift     <- factor(completeResults$Drift, levels = driftMethods)
  completeResults$dataset   <- factor(completeResults$dataset, levels = datasetNames)
  return(completeResults)
}

