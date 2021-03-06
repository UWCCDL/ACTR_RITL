library(data.table)
library(ggplot2)
library(plyr)
library(gridExtra)

## Get bilingual simulation data
fileList <- list.files("/projects/actr/models/ACTR_RITL/simulations_02/bilingual",pattern=".txt")

# Select files with specific parameters (optional)
fileList <- fileList[!grepl("ans_0.050",fileList)]

## One Gigantic Data Table
DTbi <- rbindlist( sapply(paste("/projects/actr/models/ACTR_RITL/simulations_02/bilingual/", fileList, sep=""), fread, simplify = FALSE),
                 use.names = TRUE, idcol = "idx" )

# Remove practice trials
add19 <- function(x) {x+(0:19)}
practice <- as.vector(sapply(seq(1,nrow(DTbi),by = 60), add19)) # The first 20 rows of every 60 rows are practice trials
DTbi <- DTbi[-practice,]

DTbi$language <- "bilingual"
DTbi$practiced <- DTbi$Rule == "(INCREMENT DOUBLE DIVIDE)" | DTbi$Rule == "(TRIPLE INCREMENT ADD)"
DTbi$trial <- rep(1:40,length.out = nrow(DTbi))

vars <- c("practiced", "alpha", "ans", "imaginal-delay", "le", "nu", "RT")

biEncoding <- aggregate(DTbi$EncodingRT,list(DTbi$practiced, DTbi$alpha, DTbi$ans, DTbi$`imaginal-delay`,DTbi$le,DTbi$nu),mean)
colnames(biEncoding) <- vars
biExecution <- aggregate(DTbi$ExecutionRT,list(DTbi$practiced, DTbi$alpha, DTbi$ans, DTbi$`imaginal-delay`,DTbi$le,DTbi$nu),mean)
colnames(biExecution) <- vars


## One Gigantic Data Table of monolingual simulation data
fileList <- list.files("/projects/actr/models/ACTR_RITL/simulations_02/monolingual",pattern=".txt")
# Select files with specific parameters (optional)
fileList <- fileList[!grepl("ans_0.050",fileList)]

DTmono <- rbindlist( sapply(paste("/projects/actr/models/ACTR_RITL/simulations_02/monolingual/", fileList, sep=""), fread, simplify = FALSE),
                 use.names = TRUE, idcol = "idx" )


practice <- as.vector(sapply(seq(1,nrow(DTmono),by = 60), add19))
DTmono <- DTmono[-practice,]

DTmono$language <- "monolingual"
DTmono$practiced <- DTmono$Rule == "(INCREMENT DOUBLE DIVIDE)" | DTmono$Rule == "(TRIPLE INCREMENT ADD)"
DTmono$trial <- rep(1:40,length.out = nrow(DTmono))

monoEncoding <- aggregate(DTmono$EncodingRT,list(DTmono$practiced, DTmono$alpha, DTmono$ans, DTmono$`imaginal-delay`,DTmono$le,DTmono$nu),mean)
colnames(monoEncoding) <- vars
monoExecution <- aggregate(DTmono$ExecutionRT,list(DTmono$practiced, DTmono$alpha, DTmono$ans, DTmono$`imaginal-delay`,DTmono$le,DTmono$nu),mean)
colnames(monoExecution) <- vars

DTComplete <- rbind(DTmono,DTbi)

## Read experiment data
fileList <- list.files("/projects/actr/models/ACTR_RITL/RITLExperimentData/", pattern=".txt")
DTExperiment <- rbindlist( sapply(paste("/projects/actr/models/ACTR_RITL/RITLExperimentData/", fileList, sep=""), fread, simplify = FALSE),
                   use.names = TRUE, idcol = "FileName")
DTExperiment <- DTExperiment[DTExperiment$Execution.RT != 0 | DTExperiment$Encoding.RT != 0,] # Remove rts of 0 ms
DTExperiment <- DTExperiment[,-c("Procedure","Running")]
subjects <- read.table("/projects/actr/models/ACTR_RITL/groups_version7.txt")  #Has all subject names (but V9 also exists??)
DTExperiment <- merge(DTExperiment,subjects, by.x="Subject",by.y = "V1", all.y =T)

experimentAcc <- aggregate(DTExperiment$Probe.ACC, by = list(DTExperiment$Practiced, DTExperiment$V), mean)

### GET INDIVIDUAL PARAMETER SETS FOR PHASE, PRACTICE, AND LANGUAGE
# Aggregate by trial number, practiced, and language
experimentEnc <- aggregate(DTExperiment$Encoding.RT, by = list(DTExperiment$Practiced, DTExperiment$V), mean)

## Attempt to get error
error <- function(simulations, experiment) {
  simulations$error <- sqrt((simulations$RT*1000 - (experiment))**2) # Get error per observation, square and sqrt
  byParams <- aggregate(simulations$error, by = list(simulations$alpha, simulations$ans, simulations$`imaginal-delay`,simulations$le,simulations$nu), mean) #Get mean error per set of parameters
  t(unlist(subset(byParams, byParams$x == min(byParams$x)))) # get parameter set with least error
}

paramsBiEnc <- error(biEncoding[biEncoding$practiced == F,], experimentEnc$x[(experimentEnc$Group.1 == "No") & (experimentEnc$Group.2 == "Bilingual")])
paramsBiEnc <- rbind(paramsBiEnc, error(biEncoding[biEncoding$practiced == T,], experimentEnc$x[(experimentEnc$Group.1 == "Yes") & (experimentEnc$Group.2 == "Bilingual")]))

paramsMonoEnc <- error(monoEncoding[monoEncoding$practiced == F,], experimentEnc$x[(experimentEnc$Group.1 == "No") & (experimentEnc$Group.2 == "Monolingual")])
paramsMonoEnc <- rbind(paramsMonoEnc, error(monoEncoding[monoEncoding$practiced == T,], experimentEnc$x[(experimentEnc$Group.1 == "Yes") & (experimentEnc$Group.2 == "Monolingual")]))

# Make images

plotEncoding <- function(paramsBi, paramsMono, DTbi, DTmono) {
  novelBi <- subset(DTbi, (practiced == F) & (alpha == paramsBi[1,1]) & (ans == paramsBi[1,2]) & (`imaginal-delay` == paramsBi[1,3]) & (le == paramsBi[1,4]) & (nu == paramsBi[1,5])) #Make sure novel is on row 1
  print(mean(novelBi$EncodingRT*1000))
  practicedBi <- subset(DTbi, (practiced == T) & (alpha == paramsBi[2,1]) & (ans == paramsBi[2,2]) & (`imaginal-delay` == paramsBi[2,3]) & (le == paramsBi[2,4]) & (nu == paramsBi[2,5]))
  print(mean(practicedBi$EncodingRT*1000))
  
  novelMono <- subset(DTmono, (practiced == F) & (alpha == paramsMono[1,1]) & (ans == paramsMono[1,2]) & (`imaginal-delay` == paramsMono[1,3]) & (le == paramsMono[1,4]) & (nu == paramsMono[1,5]))
  print(mean(novelMono$EncodingRT*1000))
  
  practicedMono <- subset(DTmono, (practiced == T) & (alpha == paramsMono[2,1]) & (ans == paramsMono[2,2]) & (`imaginal-delay` == paramsMono[2,3]) & (le == paramsMono[2,4]) & (nu == paramsMono[2,5]))
  print(mean(practicedMono$EncodingRT*1000))
  
  merged <- rbind(novelBi,practicedBi,novelMono,practicedMono)
  ggplot(merged, aes(language,EncodingRT*1000,fill=practiced)) + 
    geom_bar(stat = "summary", fun.y=mean,position = "dodge") + 
    scale_fill_grey(start= 0.8, end = 0.2, breaks=c(FALSE,TRUE), labels=c("Novel", "Practiced"), name="") +
    theme_bw() + 
    labs(title = "Encoding Times", y = "Response Time (ms)", x ="") + ylim(0,5000)
}
# plotEncoding(paramsBiEnc,paramsMonoEnc,DTbi,DTmono)


experimentEx <- aggregate(DTExperiment$Execution.RT, by = list(DTExperiment$Practiced, DTExperiment$V2), mean)

paramsBiEx <- error(biExecution[biExecution$practiced == F,], experimentEx$x[(experimentEx$Group.1 == "No") & (experimentEx$Group.2 == "Bilingual")])
paramsBiEx <- rbind(paramsBiEx, error(biExecution[biExecution$practiced == T,], experimentEx$x[(experimentEx$Group.1 == "Yes") & (experimentEx$Group.2 == "Bilingual")]))
paramsMonoEx <- error(monoExecution[monoExecution$practiced == F,], experimentEx$x[(experimentEx$Group.1 == "No") & (experimentEx$Group.2 == "Monolingual")])
paramsMonoEx <- rbind(paramsMonoEx, error(monoExecution[monoExecution$practiced == T,], experimentEx$x[(experimentEx$Group.1 == "Yes") & (experimentEx$Group.2 == "Monolingual")]))

# Make images

plotExecution <- function(paramsBi, paramsMono, DTbi, DTmono) {
  novelBi <- subset(DTbi, (practiced == F) & (alpha == paramsBi[1,1]) & (ans == paramsBi[1,2]) & (`imaginal-delay` == paramsBi[1,3]) & (le == paramsBi[1,4]) & (nu == paramsBi[1,5])) #Make sure novel is on row 1
  print(mean(novelBi$ExecutionRT*1000))
  practicedBi <- subset(DTbi, (practiced == T) & (alpha == paramsBi[2,1]) & (ans == paramsBi[2,2]) & (`imaginal-delay` == paramsBi[2,3]) & (le == paramsBi[2,4]) & (nu == paramsBi[2,5]))
  print(mean(practicedBi$ExecutionRT*1000))
  
  novelMono <- subset(DTmono, (practiced == F) & (alpha == paramsMono[1,1]) & (ans == paramsMono[1,2]) & (`imaginal-delay` == paramsMono[1,3]) & (le == paramsMono[1,4]) & (nu == paramsMono[1,5]))
  print(mean(novelMono$ExecutionRT*1000))
  
  practicedMono <- subset(DTmono, (practiced == T) & (alpha == paramsMono[2,1]) & (ans == paramsMono[2,2]) & (`imaginal-delay` == paramsMono[2,3]) & (le == paramsMono[2,4]) & (nu == paramsMono[2,5]))
  print(mean(practicedMono$ExecutionRT*1000))
  
  
  merged <- rbind(novelBi,practicedBi,novelMono,practicedMono)
  ggplot(merged, aes(language,ExecutionRT*1000,fill=practiced)) + 
    geom_bar(stat = "summary", fun.y=mean,position = "dodge") + 
    scale_fill_grey(start= 0.8, end = 0.2, breaks=c(FALSE,TRUE), labels=c("Novel", "Practiced"), name="") +
    theme_bw() + 
    labs(title = "Execution Times", y = "Response Time (ms)", x ="") + ylim(0,5000)
}
# plotExecution(paramsBiEx,paramsMonoEx,DTbi,DTmono)


### GET SINGLE PARAMETER SET WITH SMALLEST MEAN SQUARED ERROR OVER ALL DATA

# Aggregate all data by all parameters
aggregatedComplete <- aggregate(DTComplete$EncodingRT, by = list(DTComplete$practiced, DTComplete$language, DTComplete$alpha, DTComplete$ans, DTComplete$`imaginal-delay`,DTComplete$le,DTComplete$nu), mean)
aggregatedComplete <- cbind(aggregatedComplete, aggregate(DTComplete$ExecutionRT, by = list(DTComplete$practiced, DTComplete$language, DTComplete$alpha, DTComplete$ans, DTComplete$`imaginal-delay`,DTComplete$le,DTComplete$nu), mean)$x)
colnames(aggregatedComplete) <- c("practiced", "language", "alpha", "ans", "imaginal-delay", "le", "nu", "EncRT","ExRT")

#Determine error: errorExecution + errorEncoding
aggregatedComplete$error <- (sqrt((aggregatedComplete$EncRT*1000 - (experimentEnc$x))**2) + sqrt((aggregatedComplete$ExRT*1000 - (experimentEx$x))**2))
# Aggregate error by parameters, then take smallest error
byParams <- aggregate(aggregatedComplete$error, by = list(aggregatedComplete$alpha, aggregatedComplete$ans, aggregatedComplete$`imaginal-delay`,aggregatedComplete$le,aggregatedComplete$nu), mean)
params <- t(unlist(subset(byParams, byParams$x == min(byParams$x))))

# Plot and print RTs
plotEncoding <- function(params, DTbi, DTmono) {
  novelBi <- subset(DTbi, (practiced == F) & (alpha == params[1]) & (ans == params[2]) & (`imaginal-delay` == params[3]) & (le == params[4]) & (nu == params[5])) #Make sure novel is on row 1
  print(mean(novelBi$EncodingRT*1000))
  practicedBi <- subset(DTbi, (practiced == T) & (alpha == params[1]) & (ans == params[2]) & (`imaginal-delay` == params[3]) & (le == params[4]) & (nu == params[5]))
  print(mean(practicedBi$EncodingRT*1000))
  
  novelMono <- subset(DTmono, (practiced == F) & (alpha == params[1]) & (ans == params[2]) & (`imaginal-delay` == params[3]) & (le == params[4]) & (nu == params[5]))
  print(mean(novelMono$EncodingRT*1000))
  
  practicedMono <- subset(DTmono, (practiced == T) & (alpha == params[1]) & (ans == params[2]) & (`imaginal-delay` == params[3]) & (le == params[4]) & (nu == params[5]))
  print(mean(practicedMono$EncodingRT*1000))
  
  merged <- rbind(novelBi,practicedBi,novelMono,practicedMono)
  colnames(merged)[1] <- "fileName"
  ggplot(merged, aes(language,EncodingRT*1000,fill=practiced)) + 
    geom_bar(stat = "summary", fun.y=mean,position = "dodge") + 
    scale_fill_grey(start= 0.8, end = 0.2, breaks=c(FALSE,TRUE), labels=c("Novel", "Practiced"), name="") +
    theme_bw() + 
    labs(title = "Encoding Times", y = "Response Time (ms)", x ="") + ylim(0,5000)
}

plotExecution <- function(params, DTbi, DTmono) {
  novelBi <- subset(DTbi, (practiced == F) & (alpha == params[1]) & (ans == params[2]) & (`imaginal-delay` == params[3]) & (le == params[4]) & (nu == params[5])) #Make sure novel is on row 1
  print(mean(novelBi$ExecutionRT*1000))
  practicedBi <- subset(DTbi, (practiced == T) & (alpha == params[1]) & (ans == params[2]) & (`imaginal-delay` == params[3]) & (le == params[4]) & (nu == params[5]))
  print(mean(practicedBi$ExecutionRT*1000))
  
  novelMono <- subset(DTmono, (practiced == F) & (alpha == params[1]) & (ans == params[2]) & (`imaginal-delay` == params[3]) & (le == params[4]) & (nu == params[5]))
  print(mean(novelMono$ExecutionRT*1000))
  
  practicedMono <- subset(DTmono, (practiced == T) & (alpha == params[1]) & (ans == params[2]) & (`imaginal-delay` == params[3]) & (le == params[4]) & (nu == params[5]))
  print(mean(practicedMono$ExecutionRT*1000))
  
  merged <- rbind(novelBi,practicedBi,novelMono,practicedMono)
  colnames(merged)[1] <- "fileName"
  ggplot(merged, aes(language,ExecutionRT*1000,fill=practiced)) + 
    geom_bar(stat = "summary", fun.y=mean,position = "dodge") + 
    scale_fill_grey(start= 0.8, end = 0.2, breaks=c(FALSE,TRUE), labels=c("Novel", "Practiced"), name="") +
    theme_bw() + 
    labs(title = "Execution Times", y = "Response Time (ms)", x ="") + ylim(0,5000)
}

plotEncoding(params,DTbi,DTmono)
plotExecution(params,DTbi,DTmono)
params


## Find params that correlate the most with these datapoints (so, not by trial, that's optional for later)

meanRTs <- cbind(experimentEnc, experimentEx$x)
colnames(meanRTs) <- c("practiced","language","EncRT","ExRT")

allParams <- unique(aggregatedComplete[,c('alpha','ans','imaginal-delay','le','nu')])

for (i in 1:nrow(allParams)) {
  dat <- DTComplete[(alpha == allParams[i,1]) & (ans == allParams[i,2]) & (`imaginal-delay` == allParams[i,3]) & (le == allParams[i,4]) & (nu == allParams[i,5]),]
  
  aggregated <- aggregate(dat$EncodingRT, by = list(dat$practiced, dat$language), mean)
  aggregated <- cbind(aggregated, aggregate(dat$ExecutionRT, by = list(dat$practiced, dat$language), mean)$x)
  colnames(aggregated) <- c("practiced","language","EncRT","ExRT")
  
  allParams$EncCor[i] <- cor(meanRTs$EncRT,aggregated$EncRT) # encoding correlation between experiment and param set
  allParams$ExCor[i] <- cor(meanRTs$ExRT,aggregated$ExRT) # execution correlation between experiment and param set
  allParams$cor[i] <- allParams$EncCor[i] + allParams$ExCor[i]
  
}

corParams <- t(unlist(subset(allParams, allParams$cor == max(allParams$cor)))) # get parameter set with least error

plotEncoding(corParams,DTbi,DTmono)
plotExecution(corParams,DTbi,DTmono)
corParams

paramsMerge <- merge(byParams,allParams,by.x = colnames(byParams[1:5]), by.y = colnames(allParams[1:5]))
paramsRMSECor <- t(unlist(subset(paramsMerge, paramsMerge$cor == max(paramsMerge$cor) | paramsMerge$x == min(paramsMerge$x)))) # Consider correlation and RMSE
paramsRMSECor

### Parameter space partitioning (NEED FULL DATASET FOR THIS!!)

# Percentage NovelEncoding > PracticedEncoding for bilinguals
sum(aggregatedComplete[aggregatedComplete$practiced == FALSE & aggregatedComplete$language == "bilingual",]$EncRT > aggregatedComplete[aggregatedComplete$practiced == TRUE & aggregatedComplete$language == "bilingual",]$EncRT) / length(aggregatedComplete[aggregatedComplete$practiced == FALSE & aggregatedComplete$language == "bilingual",]$EncRT > aggregatedComplete[aggregatedComplete$practiced == TRUE & aggregatedComplete$language == "bilingual",]$EncRT)
# Percentage NovelEncoding > PracticedEncoding for monolinguals
sum(aggregatedComplete[aggregatedComplete$practiced == FALSE & aggregatedComplete$language == "monolingual",]$EncRT > aggregatedComplete[aggregatedComplete$practiced == TRUE & aggregatedComplete$language == "monolingual",]$EncRT) / length(aggregatedComplete[aggregatedComplete$practiced == FALSE & aggregatedComplete$language == "monolingual",]$EncRT > aggregatedComplete[aggregatedComplete$practiced == TRUE & aggregatedComplete$language == "monolingual",]$EncRT) 

# Percentage NovelExecution > PracticedExecution for monolinguals
sum(aggregatedComplete[aggregatedComplete$practiced == FALSE & aggregatedComplete$language == "bilingual",]$ExRT > aggregatedComplete[aggregatedComplete$practiced == TRUE & aggregatedComplete$language == "bilingual",]$ExRT) / length(aggregatedComplete[aggregatedComplete$practiced == FALSE & aggregatedComplete$language == "bilingual",]$ExRT > aggregatedComplete[aggregatedComplete$practiced == TRUE & aggregatedComplete$language == "bilingual",]$ExRT)
# Percentage NovelExecution > PracticedExecution for monolinguals
sum(aggregatedComplete[aggregatedComplete$practiced == FALSE & aggregatedComplete$language == "monolingual",]$ExRT > aggregatedComplete[aggregatedComplete$practiced == TRUE & aggregatedComplete$language == "monolingual",]$ExRT) / length(aggregatedComplete[aggregatedComplete$practiced == FALSE & aggregatedComplete$language == "monolingual",]$ExRT > aggregatedComplete[aggregatedComplete$practiced == TRUE & aggregatedComplete$language == "monolingual",]$ExRT)

# Boxplot of execution RTs over all parameters
ggplot(data = aggregatedComplete, aes(x=language, y =ExRT,fill = practiced)) +
  geom_boxplot() +
  theme_bw() +
  labs(title = "Execution Times over Parameter Space", y = "Response Time (ms)", x = "") +
  scale_fill_grey(start = 0.8, end = 0.3, name = "", breaks = c(FALSE,TRUE), label = c("Novel", "Practiced"))

# Mean execution times by parameter
ExRTAlpha <- aggregate(DTComplete$ExecutionRT,list(DTComplete$practiced,DTComplete$language,DTComplete$alpha), mean)
ExRTAns <- aggregate(DTComplete$ExecutionRT,list(DTComplete$practiced,DTComplete$language,DTComplete$ans), mean)
ExRTImDelay <- aggregate(DTComplete$ExecutionRT,list(DTComplete$practiced,DTComplete$language,DTComplete$`imaginal-delay`), mean)
ExRTLe <- aggregate(DTComplete$ExecutionRT,list(DTComplete$practiced,DTComplete$language,DTComplete$le), mean)
ExRTNu <- aggregate(DTComplete$ExecutionRT,list(DTComplete$practiced,DTComplete$language,DTComplete$nu), mean)

plotAlphaEx <- ggplot(ExRTAlpha, aes(x=interaction(Group.1,Group.2), y=x, col = as.factor(Group.3))) +
  geom_point() +
  theme_bw() + scale_color_grey() +
  labs(y = "Response Time (ms)", x = "", color = "alpha") +
  ylim(2.7,4.3) +
  scale_x_discrete(breaks = waiver(), labels = c("BN"," BP", "MN", "MP"))

plotAnsEx <- ggplot(ExRTAns, aes(x=interaction(Group.1,Group.2), y=x, col = as.factor(Group.3))) +
  geom_point() +
  theme_bw() + scale_color_grey() +
  labs(y = "Response Time (ms)", x = "", color = "ans") +
  ylim(2.7,4.3) +
  scale_x_discrete(breaks = waiver(), labels = c("BN"," BP", "MN", "MP"))

plotImDelay <- ggplot(ExRTImDelay, aes(x=interaction(Group.1,Group.2), y=x, col = as.factor(Group.3))) +
  geom_point() +
  theme_bw() + scale_color_grey() +
  labs(y = "Response Time (ms)", x = "", color = "imaginal-delay") +
  ylim(2.7,4.3) +
  scale_x_discrete(breaks = waiver(), labels = c("BN"," BP", "MN", "MP"))

plotLe <- ggplot(ExRTLe, aes(x=interaction(Group.1,Group.2), y=x, col = as.factor(Group.3))) +
  geom_point() +
  theme_bw() + scale_color_grey() +
  labs(y = "Response Time (ms)", x = "", color = "le") +
  ylim(2.7,4.3) +
  scale_x_discrete(breaks = waiver(), labels = c("BN"," BP", "MN", "MP"))

plotNu <- ggplot(ExRTNu, aes(x=interaction(Group.1,Group.2), y=x, col = as.factor(Group.3))) +
  geom_point() +
  theme_bw() + scale_color_grey() +
  ylim(2.7,4.3) +
  labs(y = "Response Time (ms)", x = "", color = "nu") +
  scale_x_discrete(breaks = waiver(), labels = c("BN"," BP", "MN", "MP"))

grid.arrange(plotAlphaEx, plotAnsEx, plotImDelay,plotLe,plotNu, ncol = 5)

EncRTAlpha <- aggregate(DTComplete$EncodingRT,list(DTComplete$practiced,DTComplete$language,DTComplete$alpha), mean)
EncRTAns <- aggregate(DTComplete$EncodingRT,list(DTComplete$practiced,DTComplete$language,DTComplete$ans), mean)
EncRTImDelay <- aggregate(DTComplete$EncodingRT,list(DTComplete$practiced,DTComplete$language,DTComplete$`imaginal-delay`), mean)
EncRTLe <- aggregate(DTComplete$EncodingRT,list(DTComplete$practiced,DTComplete$language,DTComplete$le), mean)
EncRTNu <- aggregate(DTComplete$EncodingRT,list(DTComplete$practiced,DTComplete$language,DTComplete$nu), mean)

plotAlphaEnc <- ggplot(EncRTAlpha, aes(x=interaction(Group.1,Group.2), y=x, col = as.factor(Group.3))) +
  geom_point() +
  theme_bw() + scale_color_grey() +
  labs(y = "Response Time (ms)", x = "", color = "alpha") +
  ylim(1.2,3.2) +
  scale_x_discrete(breaks = waiver(), labels = c("BN"," BP", "MN", "MP"))

plotAnsEnc <- ggplot(EncRTAns, aes(x=interaction(Group.1,Group.2), y=x, col = as.factor(Group.3))) +
  geom_point() +
  theme_bw() + scale_color_grey() +
  labs(y = "Response Time (ms)", x = "", color = "ans") +
  ylim(1.2,3.2) +
  scale_x_discrete(breaks = waiver(), labels = c("BN"," BP", "MN", "MP"))


plotImDelayEnc <- ggplot(EncRTImDelay, aes(x=interaction(Group.1,Group.2), y=x, col = as.factor(Group.3))) +
  geom_point() +
  theme_bw() + scale_color_grey() +
  labs(y = "Response Time (ms)", x = "", color = "imaginal-delay") +
  ylim(1.2,3.2) +
  scale_x_discrete(breaks = waiver(), labels = c("BN"," BP", "MN", "MP"))

plotLeEnc <- ggplot(EncRTLe, aes(x=interaction(Group.1,Group.2), y=x, col = as.factor(Group.3))) +
  geom_point() +
  theme_bw() + scale_color_grey() +
  labs(y = "Response Time (ms)", x = "", color = "le") +
  ylim(1.2,3.2) +
  scale_x_discrete(breaks = waiver(), labels = c("BN"," BP", "MN", "MP"))

plotNuEnc <- ggplot(EncRTNu, aes(x=interaction(Group.1,Group.2), y=x, col = as.factor(Group.3))) +
  geom_point() +
  theme_bw() + scale_color_grey() +
  labs(y = "Response Time (ms)", x = "", color = "nu") +
  ylim(1.2,3.2) +
  scale_x_discrete(breaks = waiver(), labels = c("BN"," BP", "MN", "MP"))

grid.arrange(plotAlphaEnc, plotAnsEnc, plotImDelayEnc,plotLeEnc, plotNuEnc, ncol = 5)
