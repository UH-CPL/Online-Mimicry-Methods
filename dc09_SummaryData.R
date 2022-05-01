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
d = read.csv("Data/Presenter-Judges_FGSPB+CJ+ALLStats_AllT_1HzMean_Clean.csv")
d$Treatment[which(is.na(d$Treatment))] = "NA"

subs = 

d1 = d[,-c(2,5,6,15:18,36:39,47:50,58:61,69:72,83,117,120,123,126,129,132,135,138,141,144,147,150,153,156,159,162,165,168,171,174)]

d9 = data.frame()

sink("Logs/Summary.txt")

for (sub in unique(d$Participant_ID)) {
  print("================")
  print(sub)
  print("================")
  cat("\n")
  d2 = filter(d1, Participant_ID == sub)
  for (tr in unique(d2$Treatment)) {
    d3 = filter(d2, Treatment == tr)
    print("----------------")
    print(tr)
    print(dim(d3))
    d4 = d3[,c(3,5:11,22:49,60:64)]
    d5 = d3[,-c(5:11,22:49,60:64)]
    
    d6 = aggregate(d4, by = list(d4$Treatment), FUN = mean, na.rm = T)
    d6 = d6[,-c(1,2)]
    
    d7 = d5[1,]
    
    d8 = data.frame()
    d8 = cbind(d6, d7)
    
    # d3$F_Angry = mean(d2$F_Angry, na.rm = T)
    # d3$F_Disgusted = mean(d2$F_Disgusted, na.rm = T)
    # d3$F_Afraid = mean(d2$F_Angry, na.rm = T)
    # d3$F_Happy = mean(d2$F_Angry, na.rm = T)
    # d3$F_Sad = mean(d2$F_Angry, na.rm = T)
    # d3$F_Surprised = mean(d2$F_Angry, na.rm = T)
    # d3$F_Neutral = mean(d2$F_Angry, na.rm = T)
  d9 = rbind(d9, d8)
  }
  cat("\n\n\n\n")
}

sink()

d10 = d9[,c(41:44,1:7,45:54,8:35,55:64,93:132,36:40,65:92)]

write.csv(d10, "Data/Presenter-Judges_Summary_AllT_1HzMean_Clean.csv", row.names = F)
