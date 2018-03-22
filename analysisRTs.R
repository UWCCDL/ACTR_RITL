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


library(ggplot2)
session2Mono$lang <- "Monolingual"
session2Bi$lang <- "Bilingual"
merged <- rbind(session2Mono,session2Bi)
ggplot(merged, aes(lang,EncodingRT*1000,fill=Practiced)) + 
  geom_bar(stat = "summary", fun.y=mean,position = "dodge") + 
  scale_fill_grey(breaks=c(FALSE,TRUE), labels=c("Novel", "Practiced"), name="") +
  theme_bw() + 
  labs(title = "Encoding Times", y = "Response Time (ms)", x ="")
ggplot(merged, aes(lang,ExecutionRT*1000,fill=Practiced)) +
  geom_bar(stat = "summary", fun.y=mean,position = "dodge") + 
  scale_fill_grey(breaks=c(FALSE,TRUE), labels=c("Novel", "Practiced"), name="") +
  theme_bw() + 
  labs(title = "Execution Times", y = "Response Time (ms)", x ="")