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
d = read.csv("Data/Presenter-Judges_Summary_AllT_1HzMean_Clean_v2.csv")

ch = filter(d, Group == "CH")
cl = filter(d, Group == "CL")
bh = filter(d, Group == "BH")
bl = filter(d, Group == "BL")

barplot(mean(ch$P_DomEmoB_N_percentage, na.rm = T), mean(ch$P_DomEmoB_N_percentage, na.rm = T))
