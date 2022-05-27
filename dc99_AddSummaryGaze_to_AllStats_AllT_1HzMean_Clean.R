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
d = as.data.frame(read.csv("Data/Presenter-Judges_FGSP_AllT_2022-04-06.csv"))


sink("Logs/AddSummaryGaze_to_AllT_AllStats_Clean_v2.txt")

for (sub in unique(d$Participant_ID)) {
  cat(paste0("Working on ", sub,"\n\n"))
  d1 = filter(d, Participant_ID == sub)
  for (tr in unique(d1)) {
    cat(paste0(tr,"\n"))
    d2 = filter(d1, Treatment == tr)
    
  }
}


sink()







