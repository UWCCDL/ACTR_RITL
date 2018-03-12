library(data.table)

## MAKE SAMPLE FROM EXPERIMENT
fileList <- list.files("C:/Users/ccdl/Downloads/Charlotte",pattern=".txt")

# Read all the files and create a FileName column to store filenames
DT <- rbindlist( sapply(paste("~/Downloads/Charlotte/",fileList, sep=""), fread, simplify = FALSE),
                 use.names = TRUE, idcol = "FileName" )
DT <- DT[DT$Execution.RT != 0,] # Remove execution times of 0 ms

randomSample <- DT[sample(1:nrow(DT),40)]

randomStimuliExperiment <- data.frame(
  operatorTask1 = randomSample$Operator2, operatorTask2 = randomSample$Operator3, operatorTask3 = randomSample$Operator1,
  x = randomSample$X, y = randomSample$Y, probe = randomSample$Probe)

# Example format: ((double third add) (5 9) 13))
exportStimuli <- paste("((", randomStimuliExperiment$operatorTask1, randomStimuliExperiment$operatorTask2, randomStimuliExperiment$operatorTask3, ")","(",randomStimuliExperiment$x,randomStimuliExperiment$y,")",randomStimuliExperiment$probe,")")

# write(exportStimuli,"~/Documents/GitHub/ACTR_RITL",ncolumns=1) # Permission denied
write(exportStimuli,"trials",ncolumns=1)



## MAKE DATA FROM SCRATCH
# Define simuli (remove decrement and subtract for now)
unary <- c("THIRD", "INCREMENT", "TRIPLE", "HALF","DOUBLE")
binary <- c("TIMES","DIVIDE","ADD")

#Define functions
DECREMENT <- function(x) {x-1}
THIRD <- function(x) {x/3}
INCREMENT <- function(x) {x+1}
TRIPLE <- function(x) {x*3}
HALF <- function(x) {x/2}
DOUBLE <- function(x) {x*2}

SUBTRACT <- function(x, y) {x-y}
TIMES <- function(x,y) {x*y}
DIVIDE <- function(x,y) {x/y}
ADD <- function(x,y) {x+y}

generatePracticeTrials <- function(numPracticedCombinations = 2) {
  practiceInst <- data.frame(
    operatorTask1 = rep(sample(unary,1),10), operatorTask2 = rep(sample(unary,1),10), operatorTask3 = rep(sample(binary,1),10),
    x = sample(1:9,10,T), y = sample(1:9,10,T), probe = 0,  stringsAsFactors = F)
  
  while (nrow(practiceInst)/10 != numPracticedCombinations) {
    practiceInst <- rbind(practiceInst,data.frame(
      operatorTask1 = rep(sample(unary,1),10), operatorTask2 = rep(sample(unary,1),10), operatorTask3 = rep(sample(binary,1),10),
      x = sample(1:9,10,T), y = sample(1:9,10,T), probe = 0,  stringsAsFactors = F))
  }
  
  # Solve operations
  for (i in 1:length(practiceInst$probe)) {
    tempX <- match.fun(practiceInst$operatorTask1[i])(practiceInst$x[i])
    tempY <- match.fun(practiceInst$operatorTask2[i])(practiceInst$y[i])
    practiceInst$probe[i] <- floor(match.fun(practiceInst$operatorTask3[i])(tempX,tempY))
    
    # If the result is not between 1 en 49, resample x & y
    while((1>practiceInst$probe[i]) | (practiceInst$probe[i]>49)) {
      practiceInst$x[i] <- sample(1:9,1)
      practiceInst$y[i] <- sample(1:9,1)
      
      tempX <- match.fun(practiceInst$operatorTask1[i])(practiceInst$x[i])
      tempY <- match.fun(practiceInst$operatorTask2[i])(practiceInst$y[i])
      practiceInst$probe[i] <- floor(match.fun(practiceInst$operatorTask3[i])(tempX,tempY))
    }
  }
  
  # Add mistakes (-1 or +1) and randomize
  mistakeRows <- sample(nrow(practiceInst),nrow(practiceInst)/2)
  practiceInst$probe[mistakeRows] <- practiceInst$probe[mistakeRows]-1
  practiceInst[practiceInst$probe == 0,"probe"] <- practiceInst[practiceInst$probe == 0,"probe"]+2 # If the probe becomes 0, make it 2
  
  practiceInst <- practiceInst[sample(nrow(practiceInst),nrow(practiceInst)),]
  
  return(practiceInst)
}

session1Trials <- generatePracticeTrials()

generateSession2Trials <- function(numPracticedCombinations = 2, practiceInst = session1Trials) {
  
  # WARNING: Chances are that filler items are identical to practiced trials
  practiceInst <- rbind(practiceInst,data.frame(
    operatorTask1 = sample(unary,numPracticedCombinations*10,T), operatorTask2 = sample(unary,numPracticedCombinations*10,T), operatorTask3 = sample(binary,numPracticedCombinations*10,T),
    x = sample(1:9,numPracticedCombinations*10,T), y = sample(1:9,numPracticedCombinations*10,T), probe = 0,  stringsAsFactors = F))
  
  # Solve operations
  for (i in 1:length(practiceInst$probe)) {
    tempX <- match.fun(practiceInst$operatorTask1[i])(practiceInst$x[i])
    tempY <- match.fun(practiceInst$operatorTask2[i])(practiceInst$y[i])
    practiceInst$probe[i] <- floor(match.fun(practiceInst$operatorTask3[i])(tempX,tempY))
    
  # If the result is not between 1 en 49, resample x & y
    while((1>practiceInst$probe[i]) | (practiceInst$probe[i]>49)) {
      practiceInst$x[i] <- sample(1:9,1)
      practiceInst$y[i] <- sample(1:9,1)
      
      tempX <- match.fun(practiceInst$operatorTask1[i])(practiceInst$x[i])
      tempY <- match.fun(practiceInst$operatorTask2[i])(practiceInst$y[i])
      practiceInst$probe[i] <- floor(match.fun(practiceInst$operatorTask3[i])(tempX,tempY))
    }
  }
  
  # Add mistakes (-1 or +1) and randomize
  mistakeRows <- sample(nrow(practiceInst),nrow(practiceInst)/2)
  practiceInst$probe[mistakeRows] <- practiceInst$probe[mistakeRows]-1
  practiceInst[practiceInst$probe == 0,"probe"] <- practiceInst[practiceInst$probe == 0,"probe"]+2 # If the probe becomes 0, make it 2
  
  practiceInst <- practiceInst[sample(nrow(practiceInst),nrow(practiceInst)),]
  
  return(practiceInst)
}

session2Trials <- generateSession2Trials()

modelTrials <- rbind(session1Trials,session2Trials)

exportStimuli <- paste("((", modelTrials$operatorTask1, modelTrials$operatorTask2, modelTrials$operatorTask3, ")","(",modelTrials$x,modelTrials$y,")",modelTrials$probe,")")
write(exportStimuli,"trials",ncolumns=1)
