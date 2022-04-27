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
d = read.csv("Data/Presenter-Judges_FGSPB_AllT_1HzMean_SemiFinal_2022-04-06_withCompositeJBinary.csv")

# Participant Summary
#d[c("F_DomEmoB_N")] = NA
d[c("F_DomEmoB_N_count")] = NA
#d[c("F_DomEmoB_E")] = NA
d[c("F_DomEmoB_E_count")] = NA
d[c("F_DomEmoB_N_percentage")] = NA
d[c("F_DomEmoB_E_percentage")] = NA

#d[c("F_SumEmoB_N")] = NA
d[c("F_SumEmoB_N_count")] = NA
#d[c("F_SumEmoB_E")] = NA
d[c("F_SumEmoB_E_count")] = NA
d[c("F_SumEmoB_N_percentage")] = NA
d[c("F_SumEmoB_E_percentage")] = NA

d[c("F_observation_count")] = NA
d[c("F_observation_percentage")] = NA

# Summary Judges
#d[c("J_DomEmoB_N")] = NA
d[c("J_DomEmoB_N_count")] = NA
#d[c("J_DomEmoB_E")] = NA
d[c("J_DomEmoB_E_count")] = NA
d[c("J_DomEmoB_N_percentage")] = NA
d[c("J_DomEmoB_E_percentage")] = NA

#d[c("J_SumEmoB_N")] = NA
d[c("J_SumEmoB_N_count")] = NA
#d[c("J_SumEmoB_E")] = NA
d[c("J_SumEmoB_E_count")] = NA
d[c("J_SumEmoB_N_percentage")] = NA
d[c("J_SumEmoB_E_percentage")] = NA

d[c("J_observation_count")] = NA
d[c("J_observation_percentage")] = NA

# Participant-Judges Lock Summary
d[c("PJ_DEM_NN")] = NA
d[c("PJ_DEM_NN_count")] = NA
d[c("PJ_DEM_NN_percentage")] = NA
d[c("PJ_DEM_EE")] = NA
d[c("PJ_DEM_EE_count")] = NA
d[c("PJ_DEM_EE_percentage")] = NA
d[c("PJ_DEM_Lock")] = NA
d[c("PJ_DEM_Lock_count")] = NA
d[c("PJ_DEM_Lock_percentage")] = NA

d[c("PJ_SEM_NN")] = NA
d[c("PJ_SEM_NN_count")] = NA
d[c("PJ_SEM_NN_percentage")] = NA
d[c("PJ_SEM_EE")] = NA
d[c("PJ_SEM_EE_count")] = NA
d[c("PJ_SEM_EE_percentage")] = NA
d[c("PJ_SEM_Lock")] = NA
d[c("PJ_SEM_Lock_count")] = NA
d[c("PJ_SEM_Lock_percentage")] = NA

# Participant-Judges NoLock Summary
d[c("PJ_DEM_NE")] = NA
d[c("PJ_DEM_NE_count")] = NA
d[c("PJ_DEM_NE_percentage")] = NA
d[c("PJ_DEM_EN")] = NA
d[c("PJ_DEM_EN_count")] = NA
d[c("PJ_DEM_EN_percentage")] = NA
d[c("PJ_DEM_NoLock")] = NA
d[c("PJ_DEM_NoLock_count")] = NA
d[c("PJ_DEM_NoLock_percentage")] = NA

d[c("PJ_SEM_NE")] = NA
d[c("PJ_SEM_NE_count")] = NA
d[c("PJ_SEM_NE_percentage")] = NA
d[c("PJ_SEM_EN")] = NA
d[c("PJ_SEM_EN_count")] = NA
d[c("PJ_SEM_EN_percentage")] = NA
d[c("PJ_SEM_NoLock")] = NA
d[c("PJ_SEM_NoLock_count")] = NA
d[c("PJ_SEM_NoLock_percentage")] = NA

# Participant-Judges Lock Summary
d[c("PCJ_DEM_NE")] = NA
d[c("PCJ_DEM_NE_count")] = NA
d[c("PCJ_DEM_NE_percentage")] = NA
d[c("PCJ_DEM_EN")] = NA
d[c("PCJ_DEM_EN_count")] = NA
d[c("PCJ_DEM_EN_percentage")] = NA
d[c("PCJ_DEM_NoLock")] = NA
d[c("PCJ_DEM_NoLock_count")] = NA
d[c("PCJ_DEM_NoLock_percentage")] = NA

# Participant-Judges NoLock Summary
d[c("PCJ_SEM_NE")] = NA
d[c("PCJ_SEM_NE_count")] = NA
d[c("PCJ_SEM_NE_percentage")] = NA
d[c("PCJ_SEM_EN")] = NA
d[c("PCJ_SEM_EN_count")] = NA
d[c("PCJ_SEM_EN_percentage")] = NA
d[c("PCJ_SEM_NoLock")] = NA
d[c("PCJ_SEM_NoLock_count")] = NA
d[c("PCJ_SEM_NoLock_percentage")] = NA





for (sub in unique(d$Participant_ID)) {
  print("=============================")
  print(sub)
  print("=============================")
  print("")
  print("")
  d1 = filter(d, Participant_ID == sub)
  for (tr in unique(d1$Treatment)[2:6]) {
    print("--------------------------")
    print(tr)
    print("--------------------------")
    d2 = filter(d1, Treatment == tr)
    if (tr %in% c("RB","ST","DT","PM")) {
      d2$F_DomEmoB_N_count = as.numeric(table(d2$F_DomEmoBinary)[1])
      d2$F_DomEmoB_N_percentage = as.numeric((proportions(table(d2$F_DomEmoBinary))*100)[1])
      d2$F_DomEmoB_E_count = as.numeric(table(d2$F_DomEmoBinary)[2])
      d2$F_DomEmoB_N_percentage = as.numeric((proportions(table(d2$F_DomEmoBinary))*100)[2])

      d2$F_SumEmoB_N_count = as.numeric(table(d2$F_SumEmoBinary)[1])
      d2$F_SumEmoB_N_percentage = as.numeric((proportions(table(d2$F_SumEmoBinary))*100)[1])
      d2$F_SumEmoB_E_count = as.numeric(table(d2$F_SumEmoBinary)[2])
      d2$F_SumEmoB_N_percentage = as.numeric((proportions(table(d2$F_SumEmoBinary))*100)[2])
      
      d2$F_observation_count = sum(table(d2$F_DomEmoBinary))
      d2$F_observation_percentage = (sum(table(d2$F_DomEmoBinary))/nrow(d2))*100
    }
    if (tr == "PR") {
      d2$J_DomEmoB_N_count = as.numeric(table(d2$J_DomEmoBinary)[1])
      d2$J_DomEmoB_N_percentage = as.numeric((proportions(table(d2$J_DomEmoBinary))*100)[1])
      d2$J_DomEmoB_E_count = as.numeric(table(d2$J_DomEmoBinary)[2])
      d2$J_DomEmoB_N_percentage = as.numeric((proportions(table(d2$J_DomEmoBinary))*100)[2])
      
      d2$J_SumEmoB_N_count = as.numeric(table(d2$J_SumEmoBinary)[1])
      d2$J_SumEmoB_N_percentage = as.numeric((proportions(table(d2$J_SumEmoBinary))*100)[1])
      d2$J_SumEmoB_E_count = as.numeric(table(d2$J_SumEmoBinary)[2])
      d2$J_SumEmoB_N_percentage = as.numeric((proportions(table(d2$J_SumEmoBinary))*100)[2])
      
      d2$J_observation_count = sum(table(d2$J_DomEmoBinary))
      d2$J_observation_percentage = (sum(table(d2$J_DomEmoBinary))/nrow(d2))*100
      
      cat("\n")
      for (i in 1:nrow(d2)) {
        cat(paste0(i,"-"))

# PJ DOM Lock
        if (d2$F_DomEmoBinary[i] == 0 && d2$J_DomEmoBinary[i] == 0) {
          d2$PJ_DEM_NN[i] = 1
          d2$PJ_DEM_Lock[i] = 1
        }
        if (d2$F_DomEmoBinary[i] == 1 && d2$J_DomEmoBinary[i] == 1) {
          d2$PJ_DEM_EE[i] = 1
          d2$PJ_DEM_Lock[i] = 1
        }

# PJ DOM Lock
        if (d2$F_DomEmoBinary[i] == 0 && d2$J_DomEmoBinary[i] == 0) {
          d2$PJ_DEM_NN[i] = 1
          d2$PJ_DEM_Lock[i] = 1
        }
        if (d2$F_DomEmoBinary[i] == 1 && d2$J_DomEmoBinary[i] == 1) {
          d2$PJ_DEM_EE[i] = 1
          d2$PJ_DEM_Lock[i] = 1
        }

        
# PJ SUM Lock
        if (d2$F_SumEmoBinary[i] == 0 && d2$J_SumEmoBinary[i] == 0) {
          d2$PJ_SEM_NN[i] = 1
          d2$PJ_SEM_Lock[i] = 1
        }
        if (d2$F_SumEmoBinary[i] == 1 && d2$J_SumEmoBinary[i] == 1) {
          d2$PJ_SEM_EE[i] = 1
          d2$PJ_SEM_Lock[i] = 1
        }

# PJ SUM Lock
        if (d2$F_SumEmoBinary[i] == 0 && d2$J_SumEmoBinary[i] == 0) {
          d2$PJ_SEM_NN[i] = 1
          d2$PJ_SEM_Lock[i] = 1
        }
        if (d2$F_SumEmoBinary[i] == 1 && d2$J_SumEmoBinary[i] == 1) {
          d2$PJ_SEM_EE[i] = 1
          d2$PJ_SEM_Lock[i] = 1
        }

        
        
# PJ DOM NoLock
        if (d2$F_DomEmoBinary[i] == 0 && d2$J_DomEmoBinary[i] == 0) {
          d2$PJ_DEM_NN[i] = 1
          d2$PJ_DEM_Lock[i] = 1
        }
        if (d2$F_DomEmoBinary[i] == 1 && d2$J_DomEmoBinary[i] == 1) {
          d2$PJ_DEM_EE[i] = 1
          d2$PJ_DEM_Lock[i] = 1
        }

# PJ DOM NoLock
        if (d2$F_DomEmoBinary[i] == 0 && d2$J_DomEmoBinary[i] == 0) {
          d2$PJ_DEM_NN[i] = 1
          d2$PJ_DEM_Lock[i] = 1
        }
        if (d2$F_DomEmoBinary[i] == 1 && d2$J_DomEmoBinary[i] == 1) {
          d2$PJ_DEM_EE[i] = 1
          d2$PJ_DEM_Lock[i] = 1
        }

        
# PJ SUM NoLock
        if (d2$F_SumEmoBinary[i] == 0 && d2$J_SumEmoBinary[i] == 0) {
          d2$PJ_SEM_NN[i] = 1
          d2$PJ_SEM_Lock[i] = 1
        }
        if (d2$F_SumEmoBinary[i] == 1 && d2$J_SumEmoBinary[i] == 1) {
          d2$PJ_SEM_EE[i] = 1
          d2$PJ_SEM_Lock[i] = 1
        }

# PJ SUM NoLock
        if (d2$F_SumEmoBinary[i] == 0 && d2$J_SumEmoBinary[i] == 0) {
          d2$PJ_SEM_NN[i] = 1
          d2$PJ_SEM_Lock[i] = 1
        }
        if (d2$F_SumEmoBinary[i] == 1 && d2$J_SumEmoBinary[i] == 1) {
          d2$PJ_SEM_EE[i] = 1
          d2$PJ_SEM_Lock[i] = 1
        }
        
      }
    }
    print("")
  }
  print("")
  print("")
  print("")
  print("")
  print("")
}
