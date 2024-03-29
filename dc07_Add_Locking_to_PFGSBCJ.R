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



# Judges Summary
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



# P-J DEM Lock Summary
d[c("PJ_DEM_NN")] = NA
d[c("PJ_DEM_NN_count")] = NA
d[c("PJ_DEM_NN_percentage")] = NA
d[c("PJ_DEM_EE")] = NA
d[c("PJ_DEM_EE_count")] = NA
d[c("PJ_DEM_EE_percentage")] = NA
d[c("PJ_DEM_NE")] = NA
d[c("PJ_DEM_NE_count")] = NA
d[c("PJ_DEM_NE_percentage")] = NA
d[c("PJ_DEM_EN")] = NA
d[c("PJ_DEM_EN_count")] = NA
d[c("PJ_DEM_EN_percentage")] = NA
d[c("PJ_DEM_Lock")] = NA
d[c("PJ_DEM_Lock_count")] = NA
d[c("PJ_DEM_Lock_percentage")] = NA

# # P-J SEM Lock Summary
d[c("PJ_SEM_NN")] = NA
d[c("PJ_SEM_NN_count")] = NA
d[c("PJ_SEM_NN_percentage")] = NA
d[c("PJ_SEM_EE")] = NA
d[c("PJ_SEM_EE_count")] = NA
d[c("PJ_SEM_EE_percentage")] = NA
d[c("PJ_SEM_NE")] = NA
d[c("PJ_SEM_NE_count")] = NA
d[c("PJ_SEM_NE_percentage")] = NA
d[c("PJ_SEM_EN")] = NA
d[c("PJ_SEM_EN_count")] = NA
d[c("PJ_SEM_EN_percentage")] = NA
d[c("PJ_SEM_Lock")] = NA
d[c("PJ_SEM_Lock_count")] = NA
d[c("PJ_SEM_Lock_percentage")] = NA



# P-CJ DEM Lock Summary
d[c("PCJ_DEM_NN")] = NA
d[c("PCJ_DEM_NN_count")] = NA
d[c("PCJ_DEM_NN_percentage")] = NA
d[c("PCJ_DEM_EE")] = NA
d[c("PCJ_DEM_EE_count")] = NA
d[c("PCJ_DEM_EE_percentage")] = NA
d[c("PCJ_DEM_NE")] = NA
d[c("PCJ_DEM_NE_count")] = NA
d[c("PCJ_DEM_NE_percentage")] = NA
d[c("PCJ_DEM_EN")] = NA
d[c("PCJ_DEM_EN_count")] = NA
d[c("PCJ_DEM_EN_percentage")] = NA
d[c("PCJ_DEM_Lock")] = NA
d[c("PCJ_DEM_Lock_count")] = NA
d[c("PCJ_DEM_Lock_percentage")] = NA

# # P-CJ SEM Lock Summary
d[c("PCJ_SEM_NN")] = NA
d[c("PCJ_SEM_NN_count")] = NA
d[c("PCJ_SEM_NN_percentage")] = NA
d[c("PCJ_SEM_EE")] = NA
d[c("PCJ_SEM_EE_count")] = NA
d[c("PCJ_SEM_EE_percentage")] = NA
d[c("PCJ_SEM_NE")] = NA
d[c("PCJ_SEM_NE_count")] = NA
d[c("PCJ_SEM_NE_percentage")] = NA
d[c("PCJ_SEM_EN")] = NA
d[c("PCJ_SEM_EN_count")] = NA
d[c("PCJ_SEM_EN_percentage")] = NA
d[c("PCJ_SEM_Lock")] = NA
d[c("PCJ_SEM_Lock_count")] = NA
d[c("PCJ_SEM_Lock_percentage")] = NA

d3 = data.frame()

# Fix Treatment using newPR information
table(d$newPR)
table(d$Treatment)
d$Treatment[which(d$Treatment == "PR")] = NA
d$Treatment[which(d$newPR == 1)] = "PR"
d$Treatment[which(is.na(d$Treatment))] = "NA"



# sub = "T005"
# tr = "PR"
# d5 = filter(d4, Participant_ID == "T005")
# d6 = filter(d5, Treatment == "PR")
# write.csv(d5, "testd2_v2.csv", row.names = F)
# write.csv(d6, "testd2PR_v2.csv", row.names = F)

sink("Logs/Lock1_v2.txt")

#unique(d$Participant_ID)[!is.na(unique(d$Participant_ID))]

# for (sub in unique(d$Participant_ID)[!is.na(unique(d$Participant_ID))]) {
#   #print("=============================")
#   print(sub)
#   #print("=============================")
#   #print("")
#   #print("")
#   d1 = filter(d, Participant_ID == sub)
#   d2 = filter(d1, Treatment == tr)
#   print(unique(d1$Treatment))
#   print("=============================")
# }

for (sub in unique(d$Participant_ID)[!is.na(unique(d$Participant_ID))]) {
  print("=============================")
  print("=============================")
  print(sub)
  print("=============================")
  print("=============================")
  cat("\n")
  cat("\n")
  d1 = filter(d, Participant_ID == sub)
  for (tr in unique(d1$Treatment)[c(2:6,1)]) {
    print("--------------------------")
    print(tr)
    print("--------------------------")
    d2 = filter(d1, Treatment == tr)

    d2$F_DomEmoB_N_count = as.numeric(table(d2$F_DomEmoBinary)[1])
    d2$F_DomEmoB_N_percentage = as.numeric((proportions(table(d2$F_DomEmoBinary))*100)[1])
    d2$F_DomEmoB_E_count = as.numeric(table(d2$F_DomEmoBinary)[2])
    d2$F_DomEmoB_E_percentage = as.numeric((proportions(table(d2$F_DomEmoBinary))*100)[2])

    d2$F_SumEmoB_N_count = as.numeric(table(d2$F_SumEmoBinary)[1])
    d2$F_SumEmoB_N_percentage = as.numeric((proportions(table(d2$F_SumEmoBinary))*100)[1])
    d2$F_SumEmoB_E_count = as.numeric(table(d2$F_SumEmoBinary)[2])
    d2$F_SumEmoB_E_percentage = as.numeric((proportions(table(d2$F_SumEmoBinary))*100)[2])
      
    d2$F_observation_count = sum(table(d2$F_DomEmoBinary))
    d2$F_observation_percentage = (sum(table(d2$F_DomEmoBinary))/nrow(d2))*100
      
    if (tr == "PR") {
      
      d2$Treatment_Time = seq(0, nrow(d2)-1)
      d1$LengthPR = nrow(d2)
      
      d2$J_DomEmoB_N_count = as.numeric(table(d2$J_DomEmoBinary)[1])
      d2$J_DomEmoB_N_percentage = as.numeric((proportions(table(d2$J_DomEmoBinary))*100)[1])
      d2$J_DomEmoB_E_count = as.numeric(table(d2$J_DomEmoBinary)[2])
      d2$J_DomEmoB_E_percentage = as.numeric((proportions(table(d2$J_DomEmoBinary))*100)[2])
      
      d2$J_SumEmoB_N_count = as.numeric(table(d2$J_SumEmoBinary)[1])
      d2$J_SumEmoB_N_percentage = as.numeric((proportions(table(d2$J_SumEmoBinary))*100)[1])
      d2$J_SumEmoB_E_count = as.numeric(table(d2$J_SumEmoBinary)[2])
      d2$J_SumEmoB_E_percentage = as.numeric((proportions(table(d2$J_SumEmoBinary))*100)[2])
      
      d2$J_observation_count = sum(table(d2$J_DomEmoBinary))
      d2$J_observation_percentage = (sum(table(d2$J_DomEmoBinary))/nrow(d2))*100
      
      for (i in 1:nrow(d2)) {
        cat(paste0(i,"-"))
# PJ DEM Lock
        if (!is.na(d2$F_DomEmoBinary[i]) && !is.na(d2$J_DomEmoBinary[i])) {
          if (d2$F_DomEmoBinary[i] == 0 && d2$J_DomEmoBinary[i] == 0) {
            d2$PJ_DEM_NN[i] = 1
            d2$PJ_DEM_Lock[i] = 1
          }
          if (d2$F_DomEmoBinary[i] == 1 && d2$J_DomEmoBinary[i] == 1) {
            d2$PJ_DEM_EE[i] = 1
            d2$PJ_DEM_Lock[i] = 1
          }
        }

# PJ DEM NoLock
        if (!is.na(d2$F_DomEmoBinary[i]) && !is.na(d2$J_DomEmoBinary[i])){
          if (d2$F_DomEmoBinary[i] == 0 && d2$J_DomEmoBinary[i] == 1) {
            d2$PJ_DEM_NE[i] = 1
            d2$PJ_DEM_Lock[i] = 0
          }
          if (d2$F_DomEmoBinary[i] == 1 && d2$J_DomEmoBinary[i] == 0) {
            d2$PJ_DEM_EN[i] = 1
            d2$PJ_DEM_Lock[i] =0
          }
        }

        
# PJ SEM Lock
        if (!is.na(d2$F_SumEmoBinary[i]) && !is.na(d2$J_SumEmoBinary[i])){
          if (d2$F_SumEmoBinary[i] == 0 && d2$J_SumEmoBinary[i] == 0) {
            d2$PJ_SEM_NN[i] = 1
            d2$PJ_SEM_Lock[i] = 1
          }
          if (d2$F_SumEmoBinary[i] == 1 && d2$J_SumEmoBinary[i] == 1) {
            d2$PJ_SEM_EE[i] = 1
            d2$PJ_SEM_Lock[i] = 1
          }
        }

# PJ SEM NoLock
        if (!is.na(d2$F_SumEmoBinary[i]) && !is.na(d2$J_SumEmoBinary[i])){
          if (d2$F_SumEmoBinary[i] == 0 && d2$J_SumEmoBinary[i] == 1) {
            d2$PJ_SEM_NE[i] = 1
            d2$PJ_SEM_Lock[i] = 0
          }
          if (d2$F_SumEmoBinary[i] == 1 && d2$J_SumEmoBinary[i] == 0) {
            d2$PJ_SEM_EN[i] = 1
            d2$PJ_SEM_Lock[i] = 0
          }
        }

        
        
# PCJ DEM Lock
        if (!is.na(d2$F_DomEmoBinary[i]) && !is.na(d2$CJ_Binary[i])){
          if (d2$F_DomEmoBinary[i] == 0 && d2$CJ_Binary[i] == 0) {
            d2$PCJ_DEM_NN[i] = 1
            d2$PCJ_DEM_Lock[i] = 1
          }
          if (d2$F_DomEmoBinary[i] == 1 && d2$CJ_Binary[i] == 1) {
            d2$PCJ_DEM_EE[i] = 1
            d2$PCJ_DEM_Lock[i] = 1
          }
        }

# PCJ DEM NoLock
        if (!is.na(d2$F_DomEmoBinary[i]) && !is.na(d2$CJ_Binary[i])){
          if (d2$F_DomEmoBinary[i] == 0 && d2$CJ_Binary[i] == 1) {
            d2$PCJ_DEM_NE[i] = 1
            d2$PCJ_DEM_Lock[i] = 0
          }
          if (d2$F_DomEmoBinary[i] == 1 && d2$CJ_Binary[i] == 0) {
            d2$PCJ_DEM_EN[i] = 1
            d2$PCJ_DEM_Lock[i] = 0
          }
        }

        
        
# PCJ SEM Lock
        if (!is.na(d2$F_SumEmoBinary[i]) && !is.na(d2$CJ_Binary[i])){
          if (d2$F_SumEmoBinary[i] == 0 && d2$CJ_Binary[i] == 0) {
            d2$PCJ_SEM_NN[i] = 1
            d2$PCJ_SEM_Lock[i] = 1
          }
          if (d2$F_SumEmoBinary[i] == 1 && d2$CJ_Binary[i] == 1) {
            d2$PCJ_SEM_EE[i] = 1
            d2$PCJ_SEM_Lock[i] = 1
          }
        }

# PCJ SEM NoLock
        if (!is.na(d2$F_SumEmoBinary[i]) && !is.na(d2$CJ_Binary[i])){
          if (d2$F_SumEmoBinary[i] == 0 && d2$CJ_Binary[i] == 1) {
            d2$PCJ_SEM_NE[i] = 1
            d2$PCJ_SEM_Lock[i] = 0
          }
          if (d2$F_SumEmoBinary[i] == 1 && d2$CJ_Binary[i] == 0) {
            d2$PCJ_SEM_EN[i] = 1
            d2$PCJ_SEM_Lock[i] = 0
          }
        }
      
      }
    }
    d3 = rbind(d3, d2)
    cat("\n")
  }
  cat("\n")
  cat("\n")
  cat("\n")
  cat("\n")
  cat("\n")
}

write.csv(d3, "Data/Presenter-Judges_FGSPB+CJ+LS_AllT_1HzMean_v2.csv", row.names = F)

sink()







d = read.csv("Data/Presenter-Judges_FGSPB+CJ+LS_AllT_1HzMean.csv")
d$Treatment[which(is.na(d$Treatment))] = "NA"

d4 = data.frame()

sink("Logs/Lock2_v2.txt")

  
for (sub in unique(d$Participant_ID)) {
  print("============")
  print(sub)
  print("============")
  cat("\n")
  cat("\n")
  d1 = filter(d, Participant_ID == sub)
  for (tr in unique(d1$Treatment)) {
    d2 = filter(d1, Treatment == tr)
    print("-----------")
    print(tr)
    print(dim(d2))
    print("-----------")
    if (tr %in% c("RB","ST","PM","DT","NA")) {
      #print(tr)
    }
    if (tr == "PR") {
# PJ DEM      
      d2$PJ_DEM_NN_count = as.numeric(table(d2$PJ_DEM_NN)[1])
      d2$PJ_DEM_NN_percentage = (d2$PJ_DEM_NN_count[1]/nrow(d2))*100
      cat(paste0("DEM     PJ     NN - "))

      d2$PJ_DEM_EE_count = as.numeric(table(d2$PJ_DEM_EE)[1])
      d2$PJ_DEM_EE_percentage = (d2$PJ_DEM_EE_count[1]/nrow(d2))*100
      cat(paste0("EE - "))

      d2$PJ_DEM_NE_count = as.numeric(table(d2$PJ_DEM_NE)[1])
      d2$PJ_DEM_NE_percentage = (d2$PJ_DEM_NE_count[1]/nrow(d2))*100
      cat(paste0("NE - "))


      d2$PJ_DEM_EN_count = as.numeric(table(d2$PJ_DEM_EN)[1])
      d2$PJ_DEM_EN_percentage = (d2$PJ_DEM_EN_count[1]/nrow(d2))*100
      cat(paste0("EN - "))
      
      d2$PJ_DEM_Lock_count = as.numeric(table(d2$PJ_DEM_Lock)[2])
      d2$PJ_DEM_Lock_percentage = as.numeric(proportions(table(d2$PJ_DEM_Lock)*100)[2])
      cat(paste0("Lock"))
      
      
# PJ SEM      
      d2$PJ_SEM_NN_count = as.numeric(table(d2$PJ_SEM_NN)[1])
      d2$PJ_SEM_NN_percentage = (d2$PJ_SEM_NN_count[1]/nrow(d2))*100
      cat(paste0("\nSEM     PJ     NN - "))
      
      d2$PJ_SEM_EE_count = as.numeric(table(d2$PJ_SEM_EE)[1])
      d2$PJ_SEM_EE_percentage = (d2$PJ_SEM_EE_count[1]/nrow(d2))*100
      cat(paste0("EE - "))
      
      d2$PJ_SEM_NE_count = as.numeric(table(d2$PJ_SEM_NE)[1])
      d2$PJ_SEM_NE_percentage = (d2$PJ_SEM_NE_count[1]/nrow(d2))*100
      cat(paste0("NE - "))
      
      d2$PJ_SEM_EN_count = as.numeric(table(d2$PJ_SEM_EN)[1])
      d2$PJ_SEM_EN_percentage = (d2$PJ_SEM_EN_count[1]/nrow(d2))*100
      cat(paste0("EN - "))
      
      d2$PJ_SEM_Lock_count = as.numeric(table(d2$PJ_SEM_Lock)[2])
      d2$PJ_SEM_Lock_percentage = as.numeric(proportions(table(d2$PJ_SEM_Lock)*100)[2])
      cat(paste0("Lock"))

      
      
# PCJ DEM      
      d2$PCJ_DEM_NN_count = as.numeric(table(d2$PCJ_DEM_NN)[1])
      d2$PCJ_DEM_NN_percentage = (d2$PCJ_DEM_NN_count[1]/nrow(d2))*100
      cat(paste0("\nDEM     PCJ     NN - "))

      d2$PCJ_DEM_EE_count = as.numeric(table(d2$PCJ_DEM_EE)[1])
      d2$PCJ_DEM_EE_percentage = (d2$PCJ_DEM_EE_count[1]/nrow(d2))*100
      cat(paste0("EE - "))

      d2$PCJ_DEM_NE_count = as.numeric(table(d2$PCJ_DEM_NE)[1])
      d2$PCJ_DEM_NE_percentage = (d2$PCJ_DEM_NE_count[1]/nrow(d2))*100
      cat(paste0("NE - "))

      d2$PCJ_DEM_EN_count = as.numeric(table(d2$PCJ_DEM_EN)[1])
      d2$PCJ_DEM_EN_percentage = (d2$PCJ_DEM_EN_count[1]/nrow(d2))*100
      cat(paste0("EN - "))
      
      d2$PCJ_DEM_Lock_count = as.numeric(table(d2$PCJ_DEM_Lock)[2])
      d2$PCJ_DEM_Lock_percentage = as.numeric(proportions(table(d2$PCJ_DEM_Lock)*100)[2])
      cat(paste0("Lock"))
      
      
# PCJ SEM      
      d2$PCJ_SEM_NN_count = as.numeric(table(d2$PCJ_SEM_NN)[1])
      d2$PCJ_SEM_NN_percentage = (d2$PCJ_SEM_NN_count[1]/nrow(d2))*100
      cat(paste0("\nSEM     PCJ     NN - "))
      
      d2$PCJ_SEM_EE_count = as.numeric(table(d2$PCJ_SEM_EE)[1])
      d2$PCJ_SEM_EE_percentage = (d2$PCJ_SEM_EE_count[1]/nrow(d2))*100
      cat(paste0("EE - "))
      
      d2$PCJ_SEM_NE_count = as.numeric(table(d2$PCJ_SEM_NE)[1])
      d2$PCJ_SEM_NE_percentage = (d2$PCJ_SEM_NE_count[1]/nrow(d2))*100
      cat(paste0("NE - "))
      
      d2$PCJ_SEM_EN_count = as.numeric(table(d2$PCJ_SEM_EN)[1])
      d2$PCJ_SEM_EN_percentage = (d2$PCJ_SEM_EN_count[1]/nrow(d2))*100
      cat(paste0("EN - "))
      
      d2$PCJ_SEM_Lock_count = as.numeric(table(d2$PCJ_SEM_Lock)[2])
      d2$PCJ_SEM_Lock_percentage = as.numeric(proportions(table(d2$PCJ_SEM_Lock)*100)[2])
      cat(paste0("Lock"))
      cat("\n")
    }
    d4 = rbind(d4, d2)
    print("Merged")
  }
  cat("\n")
  cat("\n")
  cat("\n")
  cat("\n")
  cat("\n")
}

sink()

write.csv(d4, "Data/Presenter-Judges_FGSPB+CJ+ALLStats_AllT_1HzMean_v2.csv", row.names = F)















