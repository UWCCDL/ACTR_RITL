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
exportStimuli <- paste("((", randomStimuliExperiment$operatorTask1, randomStimuliExperiment$operatorTask2, randomStimuliExperiment$operatorTask3, ")","(",randomStimuliExperiment$x,randomStimuliExperiment$y,")",randomStimuliExperiment$probe,"))")

# write(exportStimuli,"~/Documents/GitHub/ACTR_RITL",ncolumns=1) # Permission denied
write(exportStimuli,ncolumns=1)

## MAKE DATA FROM SCRATCH
unary <- c("DECREMENT", "THIRD", "INCREMENT", "TRIPLE", "HALF","DOUBLE")
binary <- c("SUBTRACT","TIMES","DIVIDE","ADD")

randomStimuliGenerated <- data.frame(
  operatorTask1 = sample(unary,40,T), operatorTask2 = sample(unary,40,T), operatorTask3 = sample(binary,40,T),
  x = sample(1:10,40,T), y = sample(1:10,40,T), probe = "")

exportStimuli <- paste("((", randomStimuliGenerated$operatorTask1, randomStimuliGenerated$operatorTask2, randomStimuliGenerated$operatorTask3, ")","(",randomStimuliGenerated$x,randomStimuliGenerated$y,")",randomStimuliGenerated$probe,"))")
