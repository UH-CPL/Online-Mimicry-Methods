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
d = read.csv("Data/Blink+Gaze_Summary_GroupLevel.csv")
d["Group2"] = NA
d$Group2[which(d$Group %in% c("CH","BH"))] = "I"
d$Group2[which(d$Group %in% c("CL","BL"))] = "NI"


for (grp in unique(d$Group)) {
  cat(paste0(grp,"\n\n"))
  d1 = filter(d, Group == grp)
  assign(grp, d1$BlinkRate)
}
  boxplot(CH, BH, BL, CL)


for (grp in unique(d$Group2)) {
  cat(paste0(grp,"\n\n"))
  d2 = filter(d, Group2 == grp)
  assign(grp, d2$BlinkRate)
}
  boxplot(I, NI)


