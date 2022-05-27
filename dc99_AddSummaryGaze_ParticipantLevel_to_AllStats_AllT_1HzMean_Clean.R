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
d$Treatment[which(is.na(d$Treatment))] = "NA"

numrow = length(unique(d$Participant_ID))*length(unique(d$Treatment))
df = as.data.frame(matrix(nrow = numrow, ncol = 11))
colnames(df) = c("Participant_ID","Group","Treatment","NAData","ValidData","CheckPercentage1","Closed","Left","Right","Center","CheckPercentage2")

sink("Logs/AddSummaryGaze_to_AllT_AllStats_Clean_v2.txt")

i = 0
for (sub in unique(d$Participant_ID)) {
  
  cat(paste0("\n\nWorking on ", sub,"\n\n"))
  d1 = filter(d, Participant_ID == sub)
  
  for (tr in unique(d1$Treatment)) {
    i = i+1
    cat(paste0(i, " ", tr,"\n"))
    
    d2 = filter(d1, Treatment == tr)
    
    df$Participant_ID[i] = sub
    df$Group[i] = unique(d2$Group)
    df$Treatment[i] = tr
    
    na = colSums(is.na(d2[c(18:20)]))[2][[1]]/nrow(d2)*100
    nona = sum(table(d2$G_Direction))/nrow(d2)*100
    df$NAData[i] = na
    df$ValidData[i] = nona
    df$CheckPercentage1[i] = na + nona
    
    x = prop.table(table(d2$G_Direction))*100
    df$CheckPercentage2[i] = sum(x)
    x = data.frame(t(data.frame(x)))
    colnames(x) = x[1,]
    x = x[-1,]
    x[1,] = as.numeric(x[1,])
    
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
    
  }
  cat("\n==========\n")
}

cat("\n\n====  Writing Data ====\n")
write.csv(df,"Data/GazeSummary_TreatmentLevel.csv", row.names = F)
cat("Write File Successful\n\n")

ee = Sys.time()

ee-ss

sink()






