library(data.table)

## Read separate files (disabled)
fileList <- list.files("~/Documents/ACTR_RITL/simulations_02/bilingual",pattern=".txt")
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
DTbi <- rbindlist( sapply(paste("~/Documents/ACTR_RITL/simulations_02/bilingual/", fileList, sep=""), fread, simplify = FALSE),
                 use.names = TRUE, idcol = "idx" )

practice <- as.vector(sapply(seq(1,nrow(DTbi),by = 60), add19))
DTbi <- DTbi[-practice,]

DTbi$language <- "bilingual"
DTbi$practiced <- DTbi$Rule == "(INCREMENT DOUBLE DIVIDE)" | DTbi$Rule == "(TRIPLE INCREMENT ADD)"

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

summary(biNovelEnc$RT - biPracticedEnc$RT)
hist(biNovelEnc$RT - biPracticedEnc$RT)
summary(biNovelEx$RT - biPracticedEx$RT)
hist(biNovelEx$RT - biPracticedEx$RT)

# write.csv(biEncoding, "bilingualEncoding.csv")
# write.csv(biExecution, "bilingualExecution.csv")

## One Gigantic Data Table
fileList <- list.files("~/Documents/ACTR_RITL/simulations_02/monolingual",pattern=".txt")
DTmono <- rbindlist( sapply(paste("~/Documents/ACTR_RITL/simulations_02/monolingual/", fileList, sep=""), fread, simplify = FALSE),
                 use.names = TRUE, idcol = "idx" )

practice <- as.vector(sapply(seq(1,nrow(DTmono),by = 60), add19))
DTmono <- DTmono[-practice,]

DTmono$language <- "bilingual"
DTmono$practiced <- DTmono$Rule == "(INCREMENT DOUBLE DIVIDE)" | DTmono$Rule == "(TRIPLE INCREMENT ADD)"

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

summary(monoNovelEnc$RT - monoPracticedEnc$RT)
hist(monoNovelEnc$RT - monoPracticedEnc$RT)
summary(monoNovelEx$RT - monoPracticedEx$RT)
hist(monoNovelEx$RT - monoPracticedEx$RT)

# write.csv(monoEncoding, "monolingualEncoding.csv")
# write.csv(monoExecution, "monolingualExecution.csv")
