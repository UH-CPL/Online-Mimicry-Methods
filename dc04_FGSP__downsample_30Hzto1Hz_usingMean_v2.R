# Downsample from 30 Hz to 1 Hz

ss = Sys.time()

library(tidyverse)
library(dplyr)
library(readxl)
library(zoo)

dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)

d = read.csv("Data/Presenter-Judges_FGSP_AllTreatment_2022-04-06.csv")
d7 = data.frame()

asd = Sys.Date()
dir.create(paste0("Data/Presenter-Judges_FGS+P_AllT_1HzMean_",asd))


sink(paste0("Logs/dc_4_Mean_",asd,".txt"))


for (sub in unique(d$Participant_ID)) {
  print(sub)
  d1 = filter(d, Participant_ID == sub)
  d1 = d1[-c(1:3),]
  d4 = filter(d1, Treatment == "RB")
  d2 = arrange(d1, Seconds)
  d2[c("BL_PP","RBL")] = NA
  bpp = as_tibble(d2)
  bpp$Seconds = as.integer(bpp$Seconds)
  #bppn = bpp[,-(c(1, 19, 33, 34, 38, 48, 58, 69, 71))]
  bppc = bpp[,(c(1, 19, 33, 34, 38, 48, 58, 69, 71))]
  d3 = aggregate(bpp, by = list(bpp$Seconds), FUN = mean, na.rm = T)
  
  for (i in unique(d3$Seconds)) {
    if (i%%1000 == 0) {
      print(i)
    }
    d3$Participant_ID[i] = unique(bpp$Participant_ID[(which(bpp$Seconds == i))])
    d3$G_Direction[i] = unique(bpp$G_Direction[(which(bpp$Seconds == i))])
    d3$Treatment[i] = unique(bpp$Treatment[(which(bpp$Seconds == i))])
    d3$Group[i] = unique(bpp$Group[(which(bpp$Seconds == i))])[1]
    d3$L_Time[i] = unique(bpp$L_Time[(which(bpp$Seconds == i))])[1]
    d3$C_Time[i] = unique(bpp$C_Time[(which(bpp$Seconds == i))])[1]
    d3$R_Time[i] = unique(bpp$R_Time[(which(bpp$Seconds == i))])[1]
    d3$newTime[i] = unique(bpp$newTime[(which(bpp$Seconds == i))])[1]
    d3$Task[i] = unique(bpp$Task[(which(bpp$Seconds == i))])
  }
  
  
  d5 = filter(d3, Treatment == "RB")
  d6 = as_tibble(d5$PP_QC)
  d6["T"] = seq(1:nrow(d6))
  d6$value[is.na(d6$value)] = mean(d6$value, na.rm = T)
  x1 = rollmean(d6$value, k = 151)
  #d3$BL_PP2 = min(x1)
  wmin = which(x1 == min(x1))
  d3$Treatment_Time = ceiling(d3$Treatment_Time)
  rbls = which(d3$Treatment_Time == as.numeric(wmin) & d3$Treatment == "RB")
  d3$RBL[(rbls):(rbls+150)] = 1
  
  d3$BL_PP = mean(d4$PP_QC, na.rm = T)
  
  # d3$BL_EDA = mean(d4$EDA_QC, na.rm = T)
  # d3$BL_BR = mean(d4$BR_QC, na.rm = T)
  # d3$BL_Chest_HR = mean(d4$Chest_HR_QC, na.rm = T)
  # d3$BL_Wrist_HR = mean(d4$Wrist_HR_QC, na.rm = T)
  
  #p1 = p1[rep(seq_len(nrow(p1)), each = 3), ]
  write.csv(d3, paste0("Data/Presenter-Judges_FGS+P_AllT_1HzMean_",asd,"/",sub,".csv"), row.names = F)
  d7 = rbind(d7,d3)
  print("--------Bind successful---------")
  print("")
  print("")
  print("")
}

write.csv(d7, paste0("Data/Presenter-Judges_FGSP_AllT_1HzMean_",asd,".csv"))

d8 = filter(d7, newPR == 1)
write.csv(d8, paste0("Data/Presenter-Judges_FGSP_PR_1HzMean_",asd,".csv"))

ee = Sys.time()


print("")
print("")
print("")
print("")
print("")
print("===================")
print(ee-ss)
print("===================")
