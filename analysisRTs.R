modelMonolingual <- read.csv("~/Documents/GitHub/ACTR_RITL/modelMonolingual.txt")
session2Mono <- modelMonolingual[21:60,]
session2Mono$Practiced <- session2Mono$Rule == "(INCREMENT DOUBLE DIVIDE)" | session2Mono$Rule == "(TRIPLE INCREMENT ADD)"

modelBilingual <- read.csv("~/Documents/GitHub/ACTR_RITL/modelBilingual.txt")
session2Bi <- modelBilingual[21:60,]
session2Bi$Practiced <- session2Bi$Rule == "(INCREMENT DOUBLE DIVIDE)" | session2Bi$Rule == "(TRIPLE INCREMENT ADD)"

aggregate(session2Mono$EncodingRT,list(session2Mono$Practiced),mean)
aggregate(session2Mono$ExecutionRT,list(session2Mono$Practiced),mean)

aggregate(session2Bi$EncodingRT,list(session2Bi$Practiced),mean)
aggregate(session2Bi$ExecutionRT,list(session2Bi$Practiced),mean)

session2Mono$lang <- "mono"
session2Bi$lang <- "bi"
merged <- rbind(session2Mono,session2Bi)
ggplot(merged, aes(lang,EncodingRT,fill=Practiced)) + geom_bar(stat = "summary", fun.y=mean,position = "dodge")
ggplot(merged, aes(lang,ExecutionRT,fill=Practiced)) + geom_bar(stat = "summary", fun.y=mean,position = "dodge")