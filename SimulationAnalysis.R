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
subjects <- read.table("/projects/BBT/RITL/groups_version9.txt")
DTExperiment <- merge(DTExperiment,subjects, by.x="Subject",by.y = "V1")

# Aggregate by trial number, practiced, and language OOPS DONT NEED THIS
experimentEnc <- aggregate(DTExperiment$Encoding.RT, by = list(DTExperiment$Trials, DTExperiment$Practiced, DTExperiment$V2), mean)
experimentEx <- aggregate(DTExperiment$Execution.RT, by = list(DTExperiment$Trials, DTExperiment$Practiced, DTExperiment$V2), mean)

## Attempt to get error

error <- function(simulations, experiment) {
  simulations$error <- sqrt((simulations$RT - (experiment/1000))**2)
  subset(simulations, simulations$error == min(simulations$error))
}

simulations <- experimentEnc[(experimentEnc$Group.2 == "No") & (experimentEnc$Group.3 == "Bilingual"),]

error(biNovelEnc,experimentEnc$x[(experimentEnc$Group.2 == "No") & (experimentEnc$Group.3 == "Bilingual")])
error(biPracticedEnc,experimentEnc$x[(experimentEnc$Group.2 == "Yes") & (experimentEnc$Group.3 == "Bilingual")])

error(biNovelEx,experimentEx$x[(experimentEx$Group.1 == "No") & (experimentEx$Group.2 == "Bilingual")])
error(biPracticedEx,experimentEx$x[(experimentEx$Group.1 == "Yes") & (experimentEx$Group.2 == "Bilingual")])

error(monoNovelEnc,experimentEnc$x[(experimentEnc$Group.1 == "No") & (experimentEnc$Group.2 == "Monolingual")])
error(monoPracticedEnc,experimentEnc$x[(experimentEnc$Group.1 == "Yes") & (experimentEnc$Group.2 == "Monolingual")])

error(monoNovelEx,experimentEx$x[(experimentEx$Group.1 == "No") & (experimentEx$Group.2 == "Monolingual")])
error(monoPracticedEx,experimentEx$x[(experimentEx$Group.1 == "Yes") & (experimentEx$Group.2 == "Monolingual")])

## Error function 1: Minimize distance from the four base values
error <- function(a, b, c, d) {
  ta <- target[1]
  tb <- target[2]
  tc <- target[3]
  td <- target[4]
  
  vec <- 100 * c(mean(c(a,b)), mean(c(c,d)), a - b, c - d)
  tvec <- 100 * c(mean(c(ta,tb)), mean(c(tc,td)), ta - tb, tc - td)
  
  sum((vec - tvec) ** 2)
}

verror <- Vectorize(error)

equal$Error <- verror(equal$Con_ACC, equal$Incon_ACC,
                      equal$Con_RT, equal$Incon_RT)



## Error function 2: Minimize distance from congruent conditions
## and effects.
error2 <- function(a, b, c, d) {
  ta <- target[1]
  tb <- target[2]
  tc <- target[3]
  td <- target[4]
  
  vec <- 100 * c(a, c, a - b, c - d)
  tvec <- 100 * c(ta, tc, ta - tb, tc - td)
  
  sum((vec - tvec) ** 2)
}


verror2 <- Vectorize(error2)

equal$Error2 <- verror2(equal$Con_ACC, equal$Incon_ACC,
                        equal$Con_RT, equal$Incon_RT)


# Both criteria converge on the same set of parameters
model <- subset(equal, equal$Error == min(equal$Error))
model <- subset(equal, equal$Error2 == min(equal$Error2))



# Generates a random factor
model$Run <- paste("Run", 1:dim(model)[1])


