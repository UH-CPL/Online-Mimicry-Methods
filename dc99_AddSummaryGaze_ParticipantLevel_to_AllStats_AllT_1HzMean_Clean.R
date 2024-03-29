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
d$G_Direction[which(d$G_Direction == "")] = NA

numrow = length(unique(d$Participant_ID))
df = as.data.frame(matrix(nrow = numrow, ncol = 10))
colnames(df) = c("Participant_ID","Group","NAData","ValidData","CheckPercentage1","Closed","Left","Right","Center","CheckPercentage2")

sink("Logs/AddSummaryGaze_ParticipantLevel_to_AllT_AllStats_Clean_v2.txt")

i = 0
for (sub in unique(d$Participant_ID)) {
  
    cat(paste0("\n\nWorking on ", sub,"\n\n"))
    d1 = filter(d, Participant_ID == sub)

    i = i+1
    
    df$Participant_ID[i] = sub
    df$Group[i] = unique(d1$Group)
    
    na = colSums(is.na(d1[c(18:20)]))[2][[1]]/nrow(d1)*100
    nona = sum(table(d1$G_Direction))/nrow(d1)*100
    df$NAData[i] = na
    df$ValidData[i] = nona
    df$CheckPercentage1[i] = na + nona
    
    x = prop.table(table(d1$G_Direction))*100
    if (dim(x) > 1) {
      df$CheckPercentage2[i] = sum(x)
    }
    if (dim(x) == 1) {
      df$CheckPercentage2[i] = 100
    }
    x = data.frame(t(data.frame(x)))
    colnames(x) = x[1,]
    if (ncol(x) > 1) {
      x = x[-1,]
      x[1,] = as.numeric(x[1,])
    }
    
    if (ncol(x) == 1) {
      y = as.data.frame(as.numeric(x[2,1]))
      colnames(y) = colnames(x)
      x = y
    }
    
    if ("CLOSED" %in% colnames(x)) {
      df$Closed[i] = x$CLOSED
    }
    
    if ("CENTER" %in% colnames(x)) {
      df$Center[i] = x$CENTER
    }
    
    if ("LEFT" %in% colnames(x)) {
      df$Left[i] = x$LEFT
    }
    
    if ("RIGHT" %in% colnames(x)) {
      df$Right[i] = x$RIGHT
    }
    
    cat("\n==========\n")
}

cat("\n\n====  Writing Data ====\n")
write.csv(df,"Data/GazeSummary_ParticipantLevel.csv", row.names = F)
cat("Write File Successful\n\n")

ee = Sys.time()

ee-ss

sink()







