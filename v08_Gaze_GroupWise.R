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
library(patchwork)


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
  # pdf(paste0("Plots/Gaze_Summary/",grp,"_Gaze.pdf"))
  # plot(b)
  # dev.off()
}

for (grp in unique(d$Group)) {
  print(grp)
  d1 = filter(d, Group == grp)
  d1na = d1[,c(1,3,4)]
  d1g = d1[,-c(2,3,4,5,10)]
  d1na = melt(d1na)
  d1g = melt(d1g)
  
  bn = ggplot(d1na, aes(x = Participant_ID, y = value, fill = variable))+geom_bar(stat = "identity", color = "black") + xlab("") + ylab("%") + scale_fill_manual(values = c("azure2","black")) + ggtitle(grp) + labs(fill = "Validity")
  
  bg = ggplot(d1g, aes(x = Participant_ID, y = value, fill = variable))+geom_bar(stat = "identity", color = "black") + xlab("Participant") + ylab("%") + scale_fill_manual(values = c("black","chartreuse2","brown1","deepskyblue")) + labs(fill = "Eye Position")
  
  if (grp %in% c("BL","BH")) {
    bn = bn + xlab("")
    bg = bg + xlab("")
  }
  if (grp %in% c("CH","BH")) {
    bn = bn + ylab("")
    bg = bg + ylab("")
  }
  
  
  b = ggarrange(bn, bg, nrow = 2)
  assign(paste0("b",grp),b)
}


g = ggarrange(bBL , bBH, bCL, bCH, nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom")

# pdf("Plots/Gaze_Summary/Groupwise_Gaze.pdf", width = 14, height = 9.5)
# plot(g)
# dev.off()

g1 = bBL + bBH + bCL + bCH & theme(legend.position = "bottom")
