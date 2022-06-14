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
d = read.csv("Data/Blink+Gaze_Summary_TreatmentLevel.csv")
d$Treatment[is.na(d$Treatment)] = "NA"

for (sub in unique(d$Participant_ID)) {
  d1 = filter(d, Participant_ID == sub)
  for (tr in unique(d1$Treatment)) {
    d2 = filter(d1, Treatment == sub)
  }
}

for (grp in unique(d$Group)) {
  print(grp)
  d1 = filter(d, Group == grp)
  for (sub in unique(d1$Participant_ID)) {
    print(sub)
    d2 = filter(d1, Participant_ID == sub)
    for (tr in unique(d3$Treatment)) {
      print(tr)
      d4 = filter(d3, Treatment == tr)
    }
    g = plot(ggarrange(stgNA, stgRB, stgPM, stgST, stgDT, stgPR, nrow = 1, common.legend = T, legend = "bottom"))
    pdf(paste0("Plots/Gaze_Subject_TreatmentWise_Portrait/", sub,"Gaze.pdf"), width = 18, height = 6)
    plot(g)
    dev.off()
    assign(paste0("g",i), g)
  }
  
  
}






