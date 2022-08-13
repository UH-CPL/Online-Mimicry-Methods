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
d = read.csv("Final Reports/Final Reports/Judges-Presenter_1Hz_final.csv")
sub = read.csv("Final Reports/Final Reports/Gaze_data_presentation_for_R_40_subjects.csv")$Participant_ID
sub = sub[-24]
d = filter(d, Participant_ID %in% sub)
d = filter(d, Treatment == "PR")

y = data.frame()
allEm = c("Afraid","Angry","Disgusted","Happy","Neutral","Sad","Surprised")
t = data.frame(c1 = allEm,
               c2 = NA)

for (sub in unique(d$Participant_ID)) {
  print(sub)
  dx = filter(d, Participant_ID == sub)
  
  p = dx$F_1
  l = dx$L_1
  l = capitalize(l)
  c = dx$C_1
  c = capitalize(c)
  r = dx$R_1
  r = capitalize(r)
  
#Participant
  p1 = as.data.frame((table(p)/sum(table(p)))*100)
  colnames(p1)[1] = "em"
  missingEM = allEm[which(!allEm %in% p1$em)]
  for (i in 1:length(missingEM)) {
    print(missingEM[i])
    p1 = rbind(p1, data.frame(em = missingEM[i],
                     Freq = 0))
  }
  p1["label"] = "Participant"
  p1$em = as.character(p1$em)
  p1[order(p1$em),]
  
  
#Left J
  l1 = as.data.frame((table(l)/sum(table(l)))*100)
  colnames(l1)[1] = "em"
  missingEM = allEm[which(!allEm %in% l1$em)]
  for (i in 1:length(missingEM)) {
    print(missingEM[i])
    l1 = rbind(l1, data.frame(em = missingEM[i],
                              Freq = 0))
  }
  l1["label"] = "Left Judge"
  l1$em = as.character(l1$em)
  l1[order(l1$em),]
  
  
#Center J
  c1 = as.data.frame((table(c)/sum(table(c)))*100)
  colnames(c1)[1] = "em"
  missingEM = allEm[which(!allEm %in% c1$em)]
  for (i in 1:length(missingEM)) {
    print(missingEM[i])
    c1 = rbind(c1, data.frame(em = missingEM[i],
                              Freq = 0))
  }
  c1["label"] = "Center Judge"
  c1$em = as.character(c1$em)
  c1[order(c1$em),]
  
  
#Right J
  r1 = as.data.frame((table(r)/sum(table(r)))*100)
  colnames(r1)[1] = "em"
  missingEM = allEm[which(!allEm %in% r1$em)]
  for (i in 1:length(missingEM)) {
    print(missingEM[i])
    r1 = rbind(r1, data.frame(em = missingEM[i],
                              Freq = 0))
  }
  r1["label"] = "Right Judge"
  r1$em = as.character(r1$em)
  r1[order(r1$em),]
  
  
  x = rbind(p1, l1, c1, r1)
  x["Participant"] = sub
  
  
  y = rbind(y,x)
}

#levels(y$em) = allEm

dp = filter(y, label == "Participant")
dl = filter(y, label == "Left Judge")
dc = filter(y, label == "Center Judge")
dr = filter(y, label == "Right Judge")

clr = c("orange","red","brown","green","grey", "deepskyblue2","yellow")

#P Boxplot
pp = ggplot(dp, aes(y = Freq, x = em)) + geom_boxplot(fill = clr) + 
  xlab("") + ylab("Observations [%]") +
  ggtitle(bquote("Participants  |  "~italic("n = ")~40)) +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5))


#BoxPlot LJ
pl = ggplot(dl, aes(y = Freq, x = em)) + geom_boxplot(fill = clr) + 
  xlab("") + ylab("") +
  ggtitle(bquote("Left Judge  |  "~italic("n = ")~40)) +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5))


#CJ Boxplot
pc = ggplot(dc, aes(y = Freq, x = em)) + geom_boxplot(fill = clr) + 
  xlab("") + ylab("Observations [%]") +
  ggtitle(bquote("Center Judge  |  "~italic("n = ")~40)) +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5))


#RJ Boxplot
pr = ggplot(dr, aes(y = Freq, x = em)) + geom_boxplot(fill = clr) + 
  xlab("") + ylab("") + 
  ggtitle(bquote("Right Judge  |  "~italic("n = ")~40)) +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5))



px = ggarrange(pp, pl, pc, pr, nrow = 2, ncol = 2)
pdf("Plots/emotion_boxplot_PR_40_v3.pdf", width = 12, height = 9)
plot(px)
dev.off()


