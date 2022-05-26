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
d = as.data.frame(read.csv("Data/Presenter-Judges_FGSP_AllT_2022-04-06.csv"))
f = as.data.frame(read.csv("Data/Presenter-Judges_FGSPB+CJ+ALLStats_AllT_1HzMean_Clean_v2.csv"))

df1 = data.frame()
df2 = data.frame()



for (sub in unique(f$Participant_ID)) {
  f1 = filter(f, Participant_ID == sub)
  d1 = filter(d, Participant_ID == sub)
  #print(paste0(unique(f1$Participant_ID),"===="))
  cat(paste0("Working on  ",unique(f1$Participant_ID), " = ", unique(d1$Participant_ID),"\n"))
  if (unique(f1$Participant_ID) == unique(d1$Participant_ID)) {
    cat(" CheckTest1 Passed!\n")
  }
  d1 = d1[order(d1$Seconds),]
  d1["Seconds_1Hz"] = as.integer(d1$Seconds)
  for (i in unique(f1$Seconds)) {
    cat(i)
    f2 = filter(f1, Seconds == i)
    d2 = filter(d1, Seconds_1Hz == i)
    if (unique(f2$Seconds) == unique(d2$Seconds_1Hz)) {
      cat("      - Test Passed\n")
    }
    f3 = f2[rep(1, nrow(d2)),]
    d3 = d2[,c(18:27,77)]
    df = cbind(d3, f3)
    df = df[,c(12:18,1:11,19:187)]
    df1 = rbind(df1, df)
  }
  df2 = rbind(df2, df1)
  cat("-------------\n")
  cat(paste0(sub," Successfully Bonded to the DF!"))
  cat("=============\n")
  cat("\n\n\n\n")
}


