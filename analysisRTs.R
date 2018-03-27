modelMonolingual <- read.csv("~/hopeneverdies8.txt", header=T, stringsAsFactors = T)
session2Mono <- modelMonolingual[21:60,]
session2Mono$Practiced <- session2Mono$Rule == "(INCREMENT DOUBLE DIVIDE)" | session2Mono$Rule == "(TRIPLE INCREMENT ADD)"

#modelBilingual <- read.csv("~/Documents/GitHub/ACTR_RITL/modelBilingual.txt")
#session2Bi <- modelBilingual[21:60,]
#session2Bi$Practiced <- session2Bi$Rule == "(INCREMENT DOUBLE DIVIDE)" | session2Bi$Rule == "(TRIPLE INCREMENT ADD)"

session2Mono$EncodingRT <- as.numeric(as.character(session2Mono$EncodingRT))
session2Mono$ExecutionRT <- as.numeric(as.character(session2Mono$ExecutionRT))
aggregate(session2Mono$EncodingRT,list(session2Mono$Practiced),mean)
aggregate(session2Mono$ExecutionRT,list(session2Mono$Practiced),mean)

aggregate(session2Bi$EncodingRT,list(session2Bi$Practiced),mean)
aggregate(session2Bi$ExecutionRT,list(session2Bi$Practiced),mean)$x*1000


library(ggplot2)
session2Mono$lang <- "Monolingual"
session2Bi$lang <- "Bilingual"
merged <- rbind(session2Mono,session2Bi)
ggplot(merged, aes(lang,EncodingRT*1000,fill=Practiced)) + 
  geom_bar(stat = "summary", fun.y=mean,position = "dodge") + 
  scale_fill_grey(start= 0.8, end = 0.2, breaks=c(FALSE,TRUE), labels=c("Novel", "Practiced"), name="") +
  theme_bw() + 
  labs(title = "Encoding Times", y = "Response Time (ms)", x ="") + ylim(0,5000)
ggplot(merged, aes(lang,ExecutionRT*1000,fill=Practiced)) +
  geom_bar(stat = "summary", fun.y=mean,position = "dodge") + 
  scale_fill_grey(start= 0.8, end = 0.2, breaks=c(FALSE,TRUE), labels=c("Novel", "Practiced"), name="") +
  theme_bw() + 
  labs(title = "Execution Times", y = "Response Time (ms)", x ="") + ylim(0,5000)