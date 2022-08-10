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


#Reading the Data
d = read.csv("Final Reports/Final Reports/Judges-Presenter_1Hz_final.csv")

p = d$F_1
l = d$L_1
c = d$C_1
r = d$R_1

p1 = (table(p)/sum(table(p)))*100
barplot(p1)
