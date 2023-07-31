cat("!!!!! IMPORTANT NOTE - DISCLAIMER !!!!!!\r\n")
cat("THE EXECUTION OF THIS SCRIPT WILL CREATE LARGE .rds FILES IN\r\n")
cat("THE DIRECTORY INDICATED IN THE datasetPath VARIABLE (defined in code)\r\n")
cat("USE AT YOUR OWN RISK!!!!\r\n")
cat("\r\n") 
cat("See more details about the methods employed in the experiments in the paper:\r\n")
cat("\r\n")
cat("Cánovas-Segura B, Morales A, Juarez JM, Campos M.\r\n")
cat("Approaches for Dealing with Seasonality in Clinical Prediction Models for Infections.\r\n")
cat("Applied Sciences. 2023; 13(14):8317. https://doi.org/10.3390/app13148317")
cat("\r\n")

#//////////////////////////////////////////////////////////////////////////
# CONFIGURATION -----------------------------------------------------------
#//////////////////////////////////////////////////////////////////////////

source("./src/functions/utils.r")

# Path to the directory where the synthetic datasets are going to be created
datasetPath <- "./data/"

# Number of elements generated for each dataset
# In the original paper, we generated 10000 data points. 
numObservations <- 10000

#//////////////////////////////////////////////////////////////////////////
# BASE DATASET ------------------------------------------------------------
#//////////////////////////////////////////////////////////////////////////

# Generation of a dataset (withou class) with the following features:
# - test_date: Timestamp from 2100-01-01 to 2199-12-31
# - x1,x2: Features (random from -10 to 10) that will affect the output of the underlying model
# - r1,...,r10: Random features from -10 to 10 that will NOT affect the output of the underlying model
# - c1_1,...,c1_10: Random features from -15 to 15, highly correlated with x1, that will NOT affect the output of the underlying model
# - c2_1,...,c2_10: Random features from -15 to 15, highly correlated with x2, that will NOT affect the output of the underlying model

startDate       <- as.Date("2100-01-01")
endDate         <- as.Date("2199-12-31")
test_date       <- sample(seq(startDate,endDate,by="day"),numObservations, replace = TRUE)
set.seed(1)

# Columns affecting the class
x1 <- runif(n = numObservations,min = -10.0, max = 10.0)
x2 <- runif(n = numObservations,min = -10.0, max = 10.0)

# Columns with random values not related with the class
r1  <- runif(n = numObservations,min = -10.0, max = 10.0)
r2  <- runif(n = numObservations,min = -10.0, max = 10.0)
r3  <- runif(n = numObservations,min = -10.0, max = 10.0)
r4  <- runif(n = numObservations,min = -10.0, max = 10.0)
r5  <- runif(n = numObservations,min = -10.0, max = 10.0)
r6  <- runif(n = numObservations,min = -10.0, max = 10.0)
r7  <- runif(n = numObservations,min = -10.0, max = 10.0)
r8  <- runif(n = numObservations,min = -10.0, max = 10.0)
r9  <- runif(n = numObservations,min = -10.0, max = 10.0)
r10 <- runif(n = numObservations,min = -10.0, max = 10.0)

# Delta that will added to x1,x2 to generate correlated features
noiseValue <- 5

# Columns values correlated with x1
c1_1  <- x1 + runif(n = numObservations,min = -1*noiseValue, max = noiseValue)
c1_2  <- x1 + runif(n = numObservations,min = -1*noiseValue, max = noiseValue)
c1_3  <- x1 + runif(n = numObservations,min = -1*noiseValue, max = noiseValue)
c1_4  <- x1 + runif(n = numObservations,min = -1*noiseValue, max = noiseValue)
c1_5  <- x1 + runif(n = numObservations,min = -1*noiseValue, max = noiseValue)
c1_6  <- x1 + runif(n = numObservations,min = -1*noiseValue, max = noiseValue)
c1_7  <- x1 + runif(n = numObservations,min = -1*noiseValue, max = noiseValue)
c1_8  <- x1 + runif(n = numObservations,min = -1*noiseValue, max = noiseValue)
c1_9  <- x1 + runif(n = numObservations,min = -1*noiseValue, max = noiseValue)
c1_10 <- x1 + runif(n = numObservations,min = -1*noiseValue, max = noiseValue)
# Columns values correlated with x2
c2_1  <- x2 + runif(n = numObservations,min = -1*noiseValue, max = noiseValue)
c2_2  <- x2 + runif(n = numObservations,min = -1*noiseValue, max = noiseValue)
c2_3  <- x2 + runif(n = numObservations,min = -1*noiseValue, max = noiseValue)
c2_4  <- x2 + runif(n = numObservations,min = -1*noiseValue, max = noiseValue)
c2_5  <- x2 + runif(n = numObservations,min = -1*noiseValue, max = noiseValue)
c2_6  <- x2 + runif(n = numObservations,min = -1*noiseValue, max = noiseValue)
c2_7  <- x2 + runif(n = numObservations,min = -1*noiseValue, max = noiseValue)
c2_8  <- x2 + runif(n = numObservations,min = -1*noiseValue, max = noiseValue)
c2_9  <- x2 + runif(n = numObservations,min = -1*noiseValue, max = noiseValue)
c2_10 <- x2 + runif(n = numObservations,min = -1*noiseValue, max = noiseValue)

datasetWithoutClass <- data.frame(test_date,x1,x2,
                                  r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,
                                  c1_1,c1_2,c1_3,c1_4,c1_5,c1_6,c1_7,c1_8,c1_9,c1_10,
                                  c2_1,c2_2,c2_3,c2_4,c2_5,c2_6,c2_7,c2_8,c2_9,c2_10)

#//////////////////////////////////////////////////////////////////////////
# CONDENSED DATASET -------------------------------------------------------
#//////////////////////////////////////////////////////////////////////////
#  In the condensed dataset, we assume that the seasonal effect happens 
#  always (and only) in winter. We follow a Normal distribution to simulate 
#  the effect.

generateFactorCondensed <- function(date) {
  # This function generates the values of the factor k1 assuming a ``condensed'' seasonal effect in winter
  #
  #
  require(lubridate)
  
  d             <- date
  year(d)       <- 2000
  minDateEffect <- winterSolstice1999
  maxDateEffect <- springEquinox2000-1
  year(minDateEffect) <- 1999
  year(maxDateEffect) <- 2000
  # Any date posterior to the spring solstice is 'moved' to 1999. By this way, 
  # our dates will range from 21-Mar-1999 to 20-Mar-2000, saving leap year and inter-year dates problems.
  if (d>maxDateEffect) { 
    year(d)<-1999
  }
  
  if (d<minDateEffect || d>maxDateEffect) return(0.0)
  
  totalDays    <- as.numeric(maxDateEffect - minDateEffect)
  currentDays  <- as.numeric(maxDateEffect - d)
  normalValue  <- dnorm(currentDays, mean = totalDays/2.0,     sd = ((totalDays/2.0)/3.0) )
  normalMax    <- dnorm(totalDays/2.0  , mean = totalDays/2.0, sd = ((totalDays/2.0)/3.0) )
  
  return( (normalValue)/normalMax)
}

k1 <- unlist(mapply(generateFactorCondensed, test_date))
k2 <- max(k1)-k1
class <- as.factor(ifelse((k1*x1+k2*x2)>=0,"NON-NEGATIVE","NEGATIVE"))

datasetWithClass <- cbind(datasetWithoutClass,class)

# And finally, we sample the dataset to create a class imbalance
set.seed(1)
idxUnbalance <- unbalancedSampling(class,"NON-NEGATIVE",10)
datasetCondensed <- datasetWithClass[idxUnbalance,]


outfile <- "synth_condensed.rds"
saveRDS(datasetCondensed,file = paste0(datasetPath,outfile))
print(paste0("Condensed dataset saved to ",normalizePath(paste0(datasetPath,outfile))))

#//////////////////////////////////////////////////////////////////////////
# SINUSOIDAL DATASET ------------------------------------------------------
#//////////////////////////////////////////////////////////////////////////
#  A common approach is to estimate the seasonal effects as a sinusoidal wave. 
#  In this dataset we assume that the factor k_1 follows a sinusoidal waveform 
#  whose maximum is reached in the middel of winter and its minimum 
#  in the middle of summer.

generateFactorSinusoidal <- function(date) {
  # This function generates the values of the factor k1 assuming a ``sinusoidal'' seasonal effect throughout the year
  #
  #
  require(lubridate)
  
  d             <- date
  year(d)       <- 2000
  maxDateEffect <- springEquinox2000-1
  year(maxDateEffect) <- 2000
  # Any date posterior to the spring equinox is 'moved' to 1999. By this way, 
  # our dates will range from 21-Mar-1999 to 20-Mar-2000, saving leap year and inter-year dates problems.
  if (d>maxDateEffect) { 
    year(d)<-1999
  }
  
  firstDay <- springEquinox1999 # Our data is flattened to the 21-Mar-1999 to 20-Mar-2000 period
  peakDay  <- winterSolstice1999+45 #- The peak of our wave will be in the middle of winter season (45 days beyond the winter solstice)
  numDaysToPeak <- as.double(peakDay - firstDay, units="days")
  numDaysDate   <- as.double(d - firstDay, units = "days")
  convertFactor <- (2*pi)/366.0
  
  return ((cos((numDaysDate-numDaysToPeak)*convertFactor)+1.0)/2.0)
}

k1 <- unlist(mapply(generateFactorSinusoidal, test_date))
k2 <- max(k1)-k1
class <- as.factor(ifelse((k1*x1+k2*x2)>=0,"NON-NEGATIVE","NEGATIVE"))

datasetWithClass <- cbind(datasetWithoutClass,class)

# And finally, we sample the dataset to create a class imbalance
set.seed(1)
idxUnbalance <- unbalancedSampling(class,"NON-NEGATIVE",10)
datasetSinusoidal <- datasetWithClass[idxUnbalance,]

outfile <- "synth_sinusoidal.rds"
saveRDS(datasetSinusoidal,file = paste0(datasetPath,outfile))
print(paste0("Sinusoidal dataset saved to ",normalizePath(paste0(datasetPath,outfile))))
