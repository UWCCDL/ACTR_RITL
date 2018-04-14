library(data.table)

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
biNovelEnc <- biEncoding[biEncoding$practiced == F,]
summary(biNovelEnc$RT)
biPracticedEnc <- biEncoding[biEncoding$practiced == T,]
summary(biPracticedEnc$RT)

# Separate Novel and practiced trials execution
biNovelEx <- biExecution[biExecution$practiced == F,]
summary(biNovelEx$RT)
biPracticedEx <- biExecution[biExecution$practiced == T,]
summary(biPracticedEx$RT)

deltaBiEncoding <- (biNovelEnc$RT - biPracticedEnc$RT)
summary(deltaBiEncoding)
hist(deltaBiEncoding)
deltaBiExecution <- (biNovelEx$RT - biPracticedEx$RT)
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
monoNovelEnc <- monoEncoding[monoEncoding$practiced == F,]
summary(monoNovelEnc$RT)
monoPracticedEnc <- monoEncoding[monoEncoding$practiced == T,]
summary(monoPracticedEnc$RT)

# Separate Novel and practiced trials execution
monoNovelEx <- monoExecution[monoExecution$practiced == F,]
summary(monoNovelEx$RT)
monoPracticedEx <- monoExecution[monoExecution$practiced == T,]
summary(monoPracticedEx$RT)

deltaMonoEncoding <- (monoNovelEnc$RT - monoPracticedEnc$RT)
summary(deltaMonoEncoding)
hist(deltaMonoEncoding)
deltaMonoExecution <- (monoNovelEx$RT - monoPracticedEx$RT)
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
experimentEnc <- aggregate(DTExperiment$Encoding.RT, by = list(DTExperiment$Trials, DTExperiment$Practiced, DTExperiment$V2), mean)
experimentEx <- aggregate(DTExperiment$Execution.RT, by = list(DTExperiment$Trials, DTExperiment$Practiced, DTExperiment$V2), mean)

## Attempt to get error
error <- function(simulations, experiment) {
  simulations$error <- sqrt((simulations$RT - (experiment/1000))**2) # Get error per observation, square and sqrt
  byParams <- aggregate(simulations$error, by = list(simulations$alpha, simulations$ans, simulations$`imaginal-delay`,simulations$le,simulations$nu), mean) #Get mean error per set of parameters
  subset(byParams, byParams$x == min(byParams$x)) # get parameter set with least error
}

error(biNovelEnc, experimentEnc$x[(experimentEnc$Group.2 == "No") & (experimentEnc$Group.3 == "Bilingual")])
error(biPracticedEnc, experimentEnc$x[(experimentEnc$Group.2 == "Yes") & (experimentEnc$Group.3 == "Bilingual")])

error(biNovelEx,experimentEx$x[(experimentEx$Group.2 == "No") & (experimentEx$Group.3 == "Bilingual")])
error(biPracticedEx,experimentEx$x[(experimentEx$Group.2 == "Yes") & (experimentEx$Group.3 == "Bilingual")])

error(monoNovelEnc,experimentEnc$x[(experimentEnc$Group.2 == "No") & (experimentEnc$Group.3 == "Monolingual")])
error(monoPracticedEnc,experimentEnc$x[(experimentEnc$Group.2 == "Yes") & (experimentEnc$Group.3 == "Monolingual")])

error(monoNovelEx,experimentEx$x[(experimentEx$Group.2 == "No") & (experimentEx$Group.3 == "Monolingual")])
error(monoPracticedEx,experimentEx$x[(experimentEx$Group.2 == "Yes") & (experimentEx$Group.3 == "Monolingual")])

