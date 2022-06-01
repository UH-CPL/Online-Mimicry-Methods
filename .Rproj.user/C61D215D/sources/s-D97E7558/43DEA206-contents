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
d = as.data.frame(read.csv("Data/Presenter-Judges_FSPB+CJ+Gaze+ALLStats_AllT_1HzMean_Clean.csv"))

sink("Logs/Split_FSPB+CJ+Gaze+AllStats_AllT.txt")

for (sub in unique(d$Participant_ID)) {
  cat(paste0("\n",sub))
  d1 = filter(d, Participant_ID == sub)
  #cat("\n====  Writing Data ====")
  write.csv(d1,paste0("Data/Presenter-Judges_FSPB+CJ+Gaze+ALLStats_AllT_1HzMean_Clean/",sub,".csv"), row.names = F)
  cat("\nWrite File Successful\n\n\n")
}


ee = Sys.time()

ee-ss

sink()







