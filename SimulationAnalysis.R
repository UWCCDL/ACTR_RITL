library(data.table)
library(ggplot2)

## Read separate files (disabled)
fileList <- list.files("~/GitHub/ACTR_RITL/simulations_02/bilingual",pattern=".txt")

# Select files with only default :le and :nu
fileList <- fileList[grepl("alpha_0.100_ans_0.040_imaginal-delay_0.200_le_1.000_nu_0.000",fileList)]

# # Read each file separately
# for (i in 1:length(fileList)) {
#   dat <- read.csv(paste("~/GitHub/ACTR_RITL/simulations_02/bilingual/", fileList[i], sep = ""),
#            header=T,
#            stringsAsFactors = F)
#   assign(paste("sim",i,sep=""), dat)
# }

## One Gigantic Data Table
DTbi <- rbindlist( sapply(paste("~/GitHub/ACTR_RITL/simulations_02/bilingual/", fileList, sep=""), fread, simplify = FALSE),
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


## One Gigantic Data Table
fileList <- list.files("~/GitHub/ACTR_RITL/simulations_02/monolingual",pattern=".txt")
# Select files with only default :le and :nu
fileList <- fileList[grepl("alpha_0.100_ans_0.040_imaginal-delay_0.200_le_1.000_nu_0.000",fileList)]

DTmono <- rbindlist( sapply(paste("~/GitHub/ACTR_RITL/simulations_02/monolingual/", fileList, sep=""), fread, simplify = FALSE),
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
fileList <- list.files("~/GitHub/ACTR_RITL/RITLExperimentData/", pattern=".txt")
DTExperiment <- rbindlist( sapply(paste("~/GitHub/ACTR_RITL/RITLExperimentData/", fileList, sep=""), fread, simplify = FALSE),
                   use.names = TRUE, idcol = "FileName")
DTExperiment <- DTExperiment[DTExperiment$Execution.RT != 0 | DTExperiment$Encoding.RT != 0,] # Remove rts of 0 ms
DTExperiment <- DTExperiment[,-c("Procedure","Running")]
subjects <- read.table("~/GitHub/ACTR_RITL/groups_version7.txt")  #Has all subject names (but V9 also exists??)
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
  ggplot(merged, aes(language,ExecutionRT*1000,fill=practiced)) + 
    geom_bar(stat = "summary", fun.y=mean,position = "dodge") + 
    scale_fill_grey(start= 0.8, end = 0.2, breaks=c(FALSE,TRUE), labels=c("Novel", "Practiced"), name="") +
    theme_bw() + 
    labs(title = "Execution Times", y = "Response Time (ms)", x ="") + ylim(0,5000)
}

plotEncoding(params,DTbi,DTmono)
plotExecution(params,DTbi,DTmono)
params


### Correlations based on each trial


# Find params that correlate the most with these datapoints (so, not by trial, that's optional for later)

meanRTs <- cbind(experimentEnc, experimentEx$x)
colnames(meanRTs) <- c("practiced","language","EncRT","ExRT")

allParams <- unique(aggregatedComplete[,c('alpha','ans','imaginal-delay','le','nu')])

for (i in 1:nrow(allParams)) {
  dat <- DTComplete[(alpha == allParams[i,1]) & (ans == allParams[i,2]) & (`imaginal-delay` == allParams[i,3]) & (le == allParams[i,4]) & (nu == allParams[i,5]),]
  
  aggregated <- aggregate(dat$EncodingRT, by = list(dat$practiced, dat$language), mean)
  aggregated <- cbind(aggregated, aggregate(dat$ExecutionRT, by = list(dat$practiced, dat$language), mean)$x)
  colnames(aggregated) <- c("practiced","language","EncRT","ExRT")
  
  allParams$EncCor[i] <- cor(meanRTs$EncRT,aggregated$EncRT)
  allParams$ExCor[i] <- cor(meanRTs$ExRT,aggregated$ExRT)
  allParams$cor[i] <- allParams$EncCor[i] + allParams$ExCor[i]
  
}

params <- t(unlist(subset(allParams, allParams$cor == max(allParams$cor)))) # get parameter set with least error

plotEncoding(params,DTbi,DTmono)
plotExecution(params,DTbi,DTmono)
params


### Parameter space partitioning (NEED FULL DATASET FOR THIS!!)

# Percentage NovelEncoding > PracticedEncoding for bilinguals
prop.table(aggregatedComplete[aggregatedComplete$practiced == FALSE & aggregatedComplete$language == "bilingual",]$EncRT > aggregatedComplete[aggregatedComplete$practiced == TRUE & aggregatedComplete$language == "bilingual",]$EncRT)
# Percentage NovelEncoding > PracticedEncoding for monolinguals
prop.table(aggregatedComplete[aggregatedComplete$practiced == FALSE & aggregatedComplete$language == "monolingual",]$EncRT > aggregatedComplete[aggregatedComplete$practiced == TRUE & aggregatedComplete$language == "monolingual",]$EncRT)

# Percentage NovelExecution > PracticedExecution for monolinguals
prop.table(aggregatedComplete[aggregatedComplete$practiced == FALSE & aggregatedComplete$language == "monolingual",]$ExRT > aggregatedComplete[aggregatedComplete$practiced == TRUE & aggregatedComplete$language == "monolingual",]$ExRT)



