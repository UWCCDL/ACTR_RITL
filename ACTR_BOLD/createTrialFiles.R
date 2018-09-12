# Read in ACT-R file
model_act <- read.table("~/GitHub/ACTR_RITL/ACTR_BOLD/model_act.txt", quote="\"", comment.char="")

# Use Manual module to get trial times
# Manual module is used 3x in one trial -> after encoding, execution and probe
manual <- model_act[model_act$V1 == "MANUAL",]
manual$phase <- rep(c("encoding","execution","probe"), 60)
TrialTime <- data.frame(endTime = manual$V3[manual$phase == "probe"], trial = 1:60)

# Get trial numbers in ACT-R var
model_act$trial <- NA

model_act$trial[model_act$V3 <= TrialTime[1,1]] <- TrialTime[1,2] # first trial

for (i in 2:nrow(TrialTime)) {
  model_act$trial[(model_act$V3 <= TrialTime[i,1]) & (model_act$V3 > TrialTime[i-1,1])] <- TrialTime[i,2]
}

model_act$trial[is.na(model_act$trial)] <- tail(TrialTime,1)[1,2] # Remaining NAs belong to last trial

# Currently, new trials do no start at t=0; t just continues
# Subtract end time of previous trial from current trial times
datModelAct <- model_act
endTime <- aggregate(datModelAct$V3,list(datModelAct$trial),max)

for (i in 2:length(unique(datModelAct$trial))) {
  datModelAct$V2[datModelAct$trial == i] <- (datModelAct$V2[datModelAct$trial == i]) - endTime$x[endTime$Group.1 == i-1]
  datModelAct$V3[datModelAct$trial == i] <- (datModelAct$V3[datModelAct$trial == i]) - endTime$x[endTime$Group.1 == i-1]
  
}

# Get model behavior
model <- read.csv("~/GitHub/ACTR_RITL/ACTR_BOLD/model.txt")
model$trial <- 1:60
model$type <- NA

# Determine rules, and then trial types
rules <- unique(model$Rule[1:20])
model$type[model$Rule %in% unique(model$Rule[1:20])] <- 2 #Practiced instructions
model$type[1:20] <- 1 # Training
model$type[is.na(model$type)] <- 3 # novel instructions

dat <- merge(datModelAct,model[,c("trial","type")], sort = F)

# Write files for MATLAB
for (i in 1:max(dat$trial)) {
  temp <- dat[dat$trial == i,c("V1","V2","V3")]

  write.table(temp,paste("model_act_1_1_", i, ".txt",sep = ""), col.names = F, row.names = F, quote = F) #model_act_subject_block(?)_trial.txt
}

# condition information. Columns: subject, block, trial, condition. (not sure what to do with block...)
actrinfo4mat <- data.frame(1,1,dat$trial,dat$type)
write.table(actrinfo4mat,"actrinfo4mat.txt", col.names = F, row.names = F)

