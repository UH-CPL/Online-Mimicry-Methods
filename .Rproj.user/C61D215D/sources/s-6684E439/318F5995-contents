ss = Sys.time()

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
d = as.data.frame(read.csv("Data/Presenter-Judges_Summary_AllT_1HzMean_Clean_v2.csv"))

df = data.frame()

sink("Logs/Remake_Timeseries_Gaze+Blink+AllStats+AllT_v3.txt")

for (sub in unique(d$Participant_ID)) {
  cat(paste0("\n",sub))
  d1 = read.csv(paste0("Data/Presenter-Judges_FSPB+CJ+Gaze+Blink(10Hz)+ALLStats_AllT_1HzMean_v2/",sub,".csv"))
  d1 = d1[,-1]
  cat("  - Read File Successful")
  df = rbind(df, d1)
  cat("  - Bind Successful\n\n")
  
}

  cat("\n\n\n====  Writing Data ====")
  write.csv(df,paste0("Data/Presenter-Judges_FSPB+CJ+Gaze(10Hz)+Blink_TimeSeries_ALLStats_AllT_1HzMean_v3.csv"), row.names = F)
  cat("\nWrite File Successful\n\n\n")

ee = Sys.time()

ee-ss

sink()







