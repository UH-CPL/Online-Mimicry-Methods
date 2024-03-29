ss = Sys.time()

#Libraries Needed
library(plyr)
library(ggplot2)
library(dplyr)
library(miceadds)
library(gtools)
library(ggpubr)
library(gvlma)
library(MASS)
library(reshape2)
library(ggcorrplot)
library(GGally)
library(BBmisc)
library(scales)
library(viridis)
#library(Scales)
library(ggnewscale)
library(patchwork)
library(gridExtra)
library(rstatix)
library(data.table)
library(R.utils)


#Reading the Data
d = read.csv("Final Reports/Final Reports/Gaze_data_presentation_for_R_40_subjects.csv")
d = d[-24,]

d$Group[which(d$Group %in% c("CH","BH"))] = "I"
d$Group[which(d$Group %in% c("CL","BL"))] = "NI"

d = d[,c(1,2,7:10)]
d1 = melt(d)

d2 = filter(d1, variable == "Right")
o = d2$Participant_ID[order(d2$value)]

d3 = data.frame()
for (i in 1:40) {
  sub = o[i]
  print(sub)
  d4 = filter(d1, Participant_ID == sub)
  d4$Participant_ID = i
  d3 = rbind(d3, d4)
}

d5 = data.frame()
for (j in unique(d3$Participant_ID)) {
  d6 = filter(d3, Participant_ID == j)
  d6 = rbind(d6[1:2,], d6[4,], d6[3,])
  d5 = rbind(d5, d6)
}

lbl = d$Group
lbl[which(lbl == "I")] = ""
lbl[which(lbl == "NI")] = "^"
q = seq(4, 160, by = 4)


ggplot(d5, aes(fill = variable, y = value, x = Participant_ID)) + 
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual('variable', values = c("grey","blue","red","green")) +
  scale_x_discrete(labels = lbl)

write.csv(d5,"bar.csv", row.names = F)
d7 = read.csv("bar.csv")
d7$Participant_ID = as.character(d7$Participant_ID)

ggplot(d7, aes(fill = variable, y = value, x = Participant_ID)) + 
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual('variable', values = c("grey","blue","green","red"))+
  scale_x_discrete(labels = lbl)



barplot(t(d1[,c(3:6)]))

gazcol = c("red","green","blue","grey")

a1<-as.matrix(d[,c(9, 10, 8, 7)])

  a1<-a1[order(a1[,1]),]
  par(mar=c(3, 4, 3, 3), xpd=TRUE)
  barplot(t(a1),beside = FALSE,col=gazcol,ylab="Observations [%]",xlab="",
          names.arg = lbl)
  #legend("top",,col=c("grey","blue","green","red"),
  #       horiz=T,bty="n",title="Gaze")
  legend("top", inset=c(0, -0.15), legend=c("Closed","Left","Center","Right"), 
         fill=gazcol,horiz=T,bty="n",title="Gaze")



a1<-as.matrix(d[,2:5])
a1<-a1[order(a1[,5]),]
par(mar=c(5, 5, 5, 0), xpd=TRUE)
barplot(t(a1),beside = FALSE,col=c("grey","blue","green","red"),ylab="Percent[%]",xlab="Subjects")
  #legend("top",,col=c("grey","blue","green","red"),
  #       horiz=T,bty="n",title="Gaze")
legend("top", inset=c(0, -0.15), legend=c("Closed","Left","Center","Right"), 
fill=GazeCol,horiz=T,bty="n",title="Gaze")






