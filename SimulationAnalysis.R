library(data.table)
library(ggplot2)

## Read separate files (disabled)
fileList <- list.files("/projects/actr/models/ACTR_RITL/simulations_02/bilingual",pattern=".txt")
# 
# for (i in 1:length(fileList)) {
#   dat <- read.csv(paste("~/Documents/GitHub/ACTR_RITL/simulations_02/bilingual/", fileList[i], sep = ""),
#            header=T,
#            stringsAsFactors = F)
#   assign(paste("sim",i,sep=""), dat)
# }

# Index practice trials
add19 <- function(x) {x+(0:19)}
practice <- as.vector(sapply(seq(1,6000,by = 60), add19))

# dat <- dat[-practice,]
# dat$language <- "bilingual"
# dat$practiced <- dat$Rule == "(INCREMENT DOUBLE DIVIDE)" | dat$Rule == "(TRIPLE INCREMENT ADD)"
# 
# aggregate(dat$EncodingRT,list(dat$Practiced),mean)
# aggregate(dat$ExecutionRT,list(dat$Practiced),mean)

## One Gigantic Data Table
DTbi <- rbindlist( sapply(paste("/projects/actr/models/ACTR_RITL/simulations_02/bilingual/", fileList, sep=""), fread, simplify = FALSE),
                 use.names = TRUE, idcol = "idx" )

practice <- as.vector(sapply(seq(1,nrow(DTbi),by = 60), add19))
DTbi <- DTbi[-practice,]

DTbi$language <- "bilingual"
DTbi$practiced <- DTbi$Rule == "(INCREMENT DOUBLE DIVIDE)" | DTbi$Rule == "(TRIPLE INCREMENT ADD)"
DTbi$trial <- rep(1:40,length.out = nrow(DTbi))

vars <- c("practiced", "alpha", "ans", "imaginal-delay", "le", "nu", "RT")

biEncoding <- aggregate(DTbi$EncodingRT,list(DTbi$practiced, DTbi$alpha, DTbi$ans, DTbi$`imaginal-delay`,DTbi$le,DTbi$nu),mean)
colnames(biEncoding) <- vars
biExecution <- aggregate(DTbi$ExecutionRT,list(DTbi$practiced, DTbi$alpha, DTbi$ans, DTbi$`imaginal-delay`,DTbi$le,DTbi$nu),mean)
colnames(biExecution) <- vars

# Separate Novel and practiced trials
summary(biEncoding[biEncoding$practiced == F,]$RT)
summary(biEncoding[biEncoding$practiced == T,]$RT)

# Separate Novel and practiced trials execution
summary(biExecution[biExecution$practiced == F,]$RT)
summary(biExecution[biExecution$practiced == T,]$RT)

deltaBiEncoding <- (biEncoding[biEncoding$practiced == F,]$RT - biEncoding[biEncoding$practiced == T,]$RT)
summary(deltaBiEncoding)
hist(deltaBiEncoding)
deltaBiExecution <- (biExecution[biExecution$practiced == F,]$RT - biExecution[biExecution$practiced == T,]$RT)
summary(deltaBiExecution)
hist(deltaBiExecution)

# write.csv(biEncoding, "bilingualEncoding.csv")
# write.csv(biExecution, "bilingualExecution.csv")

## One Gigantic Data Table
fileList <- list.files("/projects/actr/models/ACTR_RITL/simulations_02/monolingual",pattern=".txt")
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

# Separate Novel and practiced trials encoding
summary(monoEncoding[monoEncoding$practiced == F,]$RT)
summary(monoEncoding[monoEncoding$practiced == T,]$RT)

# Separate Novel and practiced trials execution
summary(monoExecution[monoExecution$practiced == F,]$RT)
summary(monoExecution[monoExecution$practiced == T,]$RT)

deltaMonoEncoding <- (monoEncoding[monoEncoding$practiced == F,]$RT - monoEncoding[monoEncoding$practiced == T,]$RT)
summary(deltaMonoEncoding)
hist(deltaMonoEncoding)
deltaMonoExecution <- (monoExecution[monoExecution$practiced == F,]$RT - monoExecution[monoExecution$practiced == T,]$RT)
summary(deltaMonoExecution)
hist(deltaMonoExecution)

# write.csv(monoEncoding, "monolingualEncoding.csv")
# write.csv(monoExecution, "monolingualExecution.csv")


## Read experiment data
fileList <- list.files("/projects/actr/models/ACTR_RITL/RITLExperimentData/", pattern=".txt")
DTExperiment <- rbindlist( sapply(paste("/projects/actr/models/ACTR_RITL/RITLExperimentData/", fileList, sep=""), fread, simplify = FALSE),
                   use.names = TRUE, idcol = "FileName")
DTExperiment <- DTExperiment[DTExperiment$Execution.RT != 0 | DTExperiment$Encoding.RT != 0,] # Remove rts of 0 ms
DTExperiment <- DTExperiment[,-c("Procedure","Running")]
subjects <- read.table("/projects/BBT/RITL/groups_version8.txt")  #Has all subject names (but V9 also exists??)
DTExperiment <- merge(DTExperiment,subjects, by.x="Subject",by.y = "V1", all =T)

# Aggregate by trial number, practiced, and language
experimentEnc <- aggregate(DTExperiment$Encoding.RT, by = list(DTExperiment$Practiced, DTExperiment$V2), mean)
experimentEx <- aggregate(DTExperiment$Execution.RT, by = list(DTExperiment$Trials, DTExperiment$Practiced, DTExperiment$V2), mean)

## Attempt to get error
error <- function(simulations, experiment) {
  simulations$error <- sqrt((simulations$RT - (experiment/1000))**2) # Get error per observation, square and sqrt
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
  practicedBi <- subset(DTbi, (practiced == T) & (alpha == paramsBi[2,1]) & (ans == paramsBi[2,2]) & (`imaginal-delay` == paramsBi[2,3]) & (le == paramsBi[2,4]) & (nu == paramsBi[2,5]))
  
  novelMono <- subset(DTmono, (practiced == F) & (alpha == paramsMono[1,1]) & (ans == paramsMono[1,2]) & (`imaginal-delay` == paramsMono[1,3]) & (le == paramsMono[1,4]) & (nu == paramsMono[1,5]))
  practicedMono <- subset(DTmono, (practiced == T) & (alpha == paramsMono[2,1]) & (ans == paramsMono[2,2]) & (`imaginal-delay` == paramsMono[2,3]) & (le == paramsMono[2,4]) & (nu == paramsMono[2,5]))
  
  merged <- rbind(novelBi,practicedBi,novelMono,practicedMono)
  ggplot(merged, aes(language,EncodingRT*1000,fill=practiced)) + 
    geom_bar(stat = "summary", fun.y=mean,position = "dodge") + 
    scale_fill_grey(start= 0.8, end = 0.2, breaks=c(FALSE,TRUE), labels=c("Novel", "Practiced"), name="") +
    theme_bw() + 
    labs(title = "Encoding Times", y = "Response Time (ms)", x ="")
}
plotEncoding(paramsBiEnc,paramsMonoEnc,DTbi,DTmono)


experimentEx <- aggregate(DTExperiment$Execution.RT, by = list(DTExperiment$Practiced, DTExperiment$V2), mean)

paramsBiEx <- error(biExecution[biExecution$practiced == F,], experimentEx$x[(experimentEx$Group.1 == "No") & (experimentEx$Group.2 == "Bilingual")])
paramsBiEx <- rbind(paramsBiEx, error(biExecution[biExecution$practiced == T,], experimentEx$x[(experimentEx$Group.1 == "Yes") & (experimentEx$Group.2 == "Bilingual")]))
paramsMonoEx <- error(monoExecution[monoExecution$practiced == F,], experimentEx$x[(experimentEx$Group.1 == "No") & (experimentEx$Group.2 == "Monolingual")])
paramsMonoEx <- rbind(paramsMonoEx, error(monoExecution[monoExecution$practiced == T,], experimentEx$x[(experimentEx$Group.1 == "Yes") & (experimentEx$Group.2 == "Monolingual")]))
 
# experimentEx2 <- aggregate(DTExperiment$Execution.RT, by = list(DTExperiment$Trials, DTExperiment$Practiced, DTExperiment$V2), mean)
# 
# paramsBiEx2 <- error(biExecution[biExecution$practiced == F,], experimentEx2$x[(experimentEx2$Group.2 == "No") & (experimentEx2$Group.3 == "Bilingual")])
# paramsBiEx2 <- rbind(paramsBiEx2, error(biExecution[biExecution$practiced == T,], experimentEx2$x[(experimentEx2$Group.2 == "Yes") & (experimentEx2$Group.3 == "Bilingual")]))
# paramsMonoEx2 <- error(monoExecution[monoExecution$practiced == F,], experimentEx2$x[(experimentEx2$Group.2 == "No") & (experimentEx2$Group.3 == "Monolingual")])
# paramsMonoEx2 <- rbind(paramsMonoEx2, error(monoExecution[monoExecution$practiced == T,], experimentEx2$x[(experimentEx2$Group.2 == "Yes") & (experimentEx2$Group.3 == "Monolingual")]))


# Make images

plotExecution <- function(paramsBi, paramsMono, DTbi, DTmono) {
  novelBi <- subset(DTbi, (practiced == F) & (alpha == paramsBi[1,1]) & (ans == paramsBi[1,2]) & (`imaginal-delay` == paramsBi[1,3]) & (le == paramsBi[1,4]) & (nu == paramsBi[1,5])) #Make sure novel is on row 1
  practicedBi <- subset(DTbi, (practiced == T) & (alpha == paramsBi[2,1]) & (ans == paramsBi[2,2]) & (`imaginal-delay` == paramsBi[2,3]) & (le == paramsBi[2,4]) & (nu == paramsBi[2,5]))
  
  novelMono <- subset(DTmono, (practiced == F) & (alpha == paramsMono[1,1]) & (ans == paramsMono[1,2]) & (`imaginal-delay` == paramsMono[1,3]) & (le == paramsMono[1,4]) & (nu == paramsMono[1,5]))
  practicedMono <- subset(DTmono, (practiced == T) & (alpha == paramsMono[2,1]) & (ans == paramsMono[2,2]) & (`imaginal-delay` == paramsMono[2,3]) & (le == paramsMono[2,4]) & (nu == paramsMono[2,5]))
  
  merged <- rbind(novelBi,practicedBi,novelMono,practicedMono)
  ggplot(merged, aes(language,ExecutionRT*1000,fill=practiced)) + 
    geom_bar(stat = "summary", fun.y=mean,position = "dodge") + 
    scale_fill_grey(start= 0.8, end = 0.2, breaks=c(FALSE,TRUE), labels=c("Novel", "Practiced"), name="") +
    theme_bw() + 
    labs(title = "Execution Times", y = "Response Time (ms)", x ="")
}
plotExecution(paramsBiEx2,paramsMonoEx2,DTbi,DTmono)
