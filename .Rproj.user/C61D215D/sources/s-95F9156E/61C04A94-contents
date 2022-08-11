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
d = read.csv("")
d = d[-24,]

d$Group[which(d$Group %in% c("CH","BH"))] = "I"
d$Group[which(d$Group %in% c("CL","BL"))] = "NI"

d = d[,c(1,2,7:10)]
d1 = melt(d)
d1 = d1[order(d$Right),]

ggplot(d1, aes(fill = variable, y = value, x = Participant_ID)) + 
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual('variable', values = c("grey","blue","green","red")) +
  scale_x_discrete(labels = lbl)



barplot(t(d1[,c(3:6)]))
