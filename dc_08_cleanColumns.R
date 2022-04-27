# Libraries Required and Loading
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(plotly)

# Retrieving current directory
dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)

# Read 1 Hz Data File
d = read.csv("Data/Presenter-Judges_FGSPB+CJ+ALLStats_AllT_1HzMean.csv")

d1 = d[,-c(1,2,3,5,14:35,38:42,50:52,60:62,70:72,109:113,118,120)]

d2 = d1[,c(1,2,11,10,33,34,69,3:9,49:52,97:106,12:18,53:56,19:25,57:60,26:32,61:64,42:48,65:68,107:116,96,35:41,70:95,117:176)]

for (i in 8:28) {
  s = strsplit(colnames(d2)[i], split = "F_")[[1]][2]
  colnames(d2)[i] = paste0("P_",s)
}

write.csv(d2, "Data/Presenter-Judges_FGSPB+CJ+ALLStats_AllT_1HzMean_Clean.csv", row.names = F)
