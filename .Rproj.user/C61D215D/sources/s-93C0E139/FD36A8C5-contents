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
dx = read.csv("Data/GazeSummary_TreatmentLevel.csv")
dx$Treatment[is.na(dx$Treatment)] = "NA"

for (grp in unique(dx$Group)) {
  print(grp)
  dx1 = filter(dx, Group == grp)
  for (tr in unique(dx1$Treatment)) {
    print(tr)
    dx2 = filter(dx1, Treatment == tr)
    dx2 = dx2[,-c(1,2,6,11)]
    dx3 = melt(dx2)
    dx3["Category"] = NA
    dx3$Category[which(dx3$variable %in% c("NAData","ValidData"))] = "Validity"
    dx3$Category[which(dx3$variable %in% c("Closed","Left","Center","Right"))] = "Eye Position"
  dx3$Category = factor(dx3$Category, levels = c("Validity","Eye Position"))
      
      b = ggplot(dx3, aes(x = Category, y = value, fill = variable))+geom_bar(stat = "identity", position = "fill") + xlab("") + ylab("") + scale_fill_manual(values = c("azure2","black","black","chartreuse2","brown1","deepskyblue")) + ggtitle(tr) + labs(fill = "")
      if (tr == "NA") {
        b = b + ylab(grp) + theme(axis.title.y = element_text(angle = 0, size = 16, face = "bold"))
      }
      assign(paste0("stg",tr), b)

  }
    g = plot(ggarrange(stgNA, stgRB, stgPM, stgST, stgDT, stgPR, nrow = 1, common.legend = T, legend = "bottom"))
    pdf(paste0("Plots/Gaze_TreatmentWise/Gaze_",grp,"_Cumulative.pdf"), width = 18, height = 6)
    plot(g)
    dev.off()
    assign(paste0("g",grp), g)
}
  
    gx = plot(ggarrange(gCH, gCL, gBH, gBL, nrow = 4, common.legend = T, legend = "bottom"))
    pdf(paste0("Plots/Gaze_TreatmentWise/Gaze_TreatmentWise_Cumulative.pdf"), width = 8, height = 16)
    plot(gx)
    dev.off()




