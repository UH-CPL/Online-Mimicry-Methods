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
library(Scale)
library(ggnewscale)


#Reading the Data
d = read.csv("Data/GazeSummary_ParticipantLevel.csv")

for (grp in unique(d$Group)) {
  print(grp)
  d1 = filter(d, Group == grp)
  d1na = d1[,c(1,3,4)]
  d1g = d1[,-c(2,3,4,5,10)]
  d1na = melt(d1na)
  d1g = melt(d1g)
  
  bn = ggplot(d1na, aes(x = Participant_ID, y = value, fill = variable))+geom_bar(stat = "identity", color = "black") + xlab("") + ylab("%") + scale_fill_manual(values = c("azure2","black")) + ggtitle(grp) + labs(fill = "Validity")
  
  bg = ggplot(d1g, aes(x = Participant_ID, y = value, fill = variable))+geom_bar(stat = "identity", color = "black") + xlab("Participant") + ylab("%") + scale_fill_manual(values = c("black","chartreuse2","brown1","deepskyblue")) + labs(fill = "Eye Position")
  
  b = ggarrange(bn, bg, nrow = 2)
  assign(paste0("b",grp),b)
  pdf(paste0("Plots/Gaze_Summary/",grp,"_Gaze.pdf"))
  ggarrange(bn, bg, nrow = 2)
  dev.off()
}

ggarrange(bBL, bBH, bCL, bCH, nrow = 2, ncol = 2, common.legend = T)


