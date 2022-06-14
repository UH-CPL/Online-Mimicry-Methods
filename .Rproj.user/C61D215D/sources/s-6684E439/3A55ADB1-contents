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
  d1 = d1[-c(1,5,10)]
  d2 = melt(d1)
  d2["Category"] = NA
  d2$Category[which(d2$variable %in% c("NAData","ValidData"))] = "Validity"
  d2$Category[which(d2$variable %in% c("Closed","Left","Center","Right"))] = "Eye Position"
  d2$Category = factor(d2$Category, levels = c("Validity","Eye Position"))
  
  b = ggplot(d2, aes(x = Category, y = value, fill = variable))+geom_bar(stat = "identity", position = "fill") + xlab("") + ylab("") + scale_fill_manual(values = c("azure2","black","black","chartreuse2","brown1","deepskyblue")) + ggtitle(grp) + labs(fill = "") + theme_bw()
  
  assign(paste0("b",grp),b)
  pdf(paste0("Plots/Gaze_GroupWise/",grp,"_GazeCumulative.pdf"))
  plot(b)
  dev.off()
}

for (grp in unique(d$Group)) {
  print(grp)
  d1 = filter(d, Group == grp)
  d1 = d1[-c(1,5,10)]
  d2 = melt(d1)
  d2["Category"] = NA
  d2$Category[which(d2$variable %in% c("NAData","ValidData"))] = "Validity"
  d2$Category[which(d2$variable %in% c("Closed","Left","Center","Right"))] = "Eye Position"
  d2$Category = factor(d2$Category, levels = c("Validity","Eye Position"))
  
  b = ggplot(d2, aes(x = Category, y = value, fill = variable))+geom_bar(stat = "identity", position = "fill") + xlab("") + ylab("") + scale_fill_manual(values = c("azure2","black","black","chartreuse2","brown1","deepskyblue")) + ggtitle(grp) + labs(fill = "") + theme_bw()
  
  if (grp %in% c("CH","BH")) {
    b = b + ylab("")
  }
  assign(paste0("b",grp),b)
}


g = ggarrange(bBL , bBH, bCL, bCH, nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom")

pdf("Plots/Gaze_GroupWise/Gaze_GroupWise_Cumulative.pdf", width = 14, height = 9.5)
plot(g)
dev.off()

# g1 = bBL + bBH + bCL + bCH & theme(legend.position = "bottom")
