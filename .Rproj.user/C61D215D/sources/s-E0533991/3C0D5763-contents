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
library(gridExtra)


#Reading the Data
d = read.csv("Data/GazeSummary_TreatmentLevel.csv")
d$Treatment[is.na(d$Treatment)] = "NA"

for (grp in unique(d$Group)) {
  print(grp)
  d1 = filter(d, Group == grp)
  i = 0
  for (sub in unique(d1$Participant_ID)) {
    i = i+1
    print(sub)
    d2 = filter(d1, Participant_ID == sub)
    d2 = d2[,-c(2,6,11)]
    d3 = melt(d2)
    d3["Category"] = NA
    d3$Category[which(d3$variable %in% c("NAData","ValidData"))] = "Validity"
    d3$Category[which(d3$variable %in% c("Closed","Left","Center","Right"))] = "Eye Position"
    for (tr in unique(d3$Treatment)) {
      print(tr)
      d4 = filter(d3, Treatment == tr)
      d5 = d4
      d5$Category = factor(d5$Category, levels = c("Validity","Eye Position"))
      
      b = ggplot(d5, aes(x = Category, y = value, fill = variable))+geom_bar(stat = "identity", color = "black") + xlab("") + ylab(sub) + scale_fill_manual(values = c("azure2","black","black","chartreuse2","brown1","deepskyblue")) + ggtitle(tr) + labs(fill = "")
      if (tr != "NA") {
        b = b + ylab("")
      }
      assign(paste0("stg",tr), b)
    }
    g = plot(ggarrange(stgNA, stgRB, stgPM, stgST, stgDT, stgPR, nrow = 1, common.legend = T, legend = "bottom"))
    pdf(paste0("Plots/Gaze_Subject_TreatmentWise_Portrait/", sub,"Gaze.pdf"), width = 18, height = 6)
    plot(g)
    dev.off()
    assign(paste0("g",i), g)
  }
  
  # if (grp %in% c("CH","CL")) {
  #   t1 = ggarrange(g1, g2, g3, g4, g5, g6, g7, g8,  common.legend = T, nrow = 2, ncol = 4, legend = "bottom")
  #   t2 = ggarrange(g8, g9, g10, g11, g12, g13, g14, common.legend = T, nrow = 3, ncol = 5, legend = "bottom")
  # }
  # 
  # if (grp %in% c("BH","BL")) {
  #   t = ggarrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, common.legend = T, nrow = 3, ncol = 5, legend = "bottom")
  # }
  
}


# pdf(paste0("Plots/test_", sub,"Gaze.pdf"), width = 36, height = 12)
# plot()
# dev.off()
# 
# x = grid.arrange(g1, g2, g3, g4, g5, g6, ncol = 2)




