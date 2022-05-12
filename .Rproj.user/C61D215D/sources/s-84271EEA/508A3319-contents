# Downsample from 30 Hz to 1 Hz

ss = Sys.time()

library(tidyverse)
library(dplyr)
library(readxl)
library(zoo)

dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)

d = read.csv("Data/Presenter-Judges_FGS+P_AllTreatment_2022-04-06.csv")
d7 = data.frame()

asd = read.csv("Data/TimeDate.csv")$x
dir.create(paste0("Data/Presenter-Judges_FGS+P_AllT_10HzMean_",asd))


#sink(paste0("Logs/dc_4_",asd,".txt"))


for (sub in unique(d$Participant_ID)) {
  print(sub)
  d1 = filter(d, Participant_ID == sub)
  d4 = filter(d1, Treatment == "RB")
  d2 = arrange(d1, Seconds)
  d2[c("BL_PP","BL_PP2")] = NA
  bpp = as_tibble(d2)
  bpp$Frame = as.integer(bpp$Frame)
  #bppn = bpp[,-(c(1, 19, 33, 34, 38, 48, 58, 69, 71))]
  bppc = bpp[,(c(1, 19, 33, 34, 38, 48, 58, 69, 71))]
  d3 = aggregate(bpp, by = list(bpp$Frame), FUN = mean, na.rm = T)
  
  for (i in unique(d3$Frame)) {
    if (i%%1000 == 0) {
      print(i)
    }
    d3$Participant_ID[i] = unique(bpp$Participant_ID[(which(bpp$Frame == i))])
    d3$G_Direction[i] = unique(bpp$G_Direction[(which(bpp$Frame == i))])
    d3$Treatment[i] = unique(bpp$Treatment[(which(bpp$Frame == i))])
    d3$Group[i] = unique(bpp$Group[(which(bpp$Frame == i))])[1]
    d3$L_Time[i] = unique(bpp$L_Time[(which(bpp$Frame == i))])[1]
    d3$C_Time[i] = unique(bpp$C_Time[(which(bpp$Frame == i))])[1]
    d3$R_Time[i] = unique(bpp$R_Time[(which(bpp$Frame == i))])[1]
    d3$newTime[i] = unique(bpp$newTime[(which(bpp$Frame == i))])[1]
    d3$Task[i] = unique(bpp$Task[(which(bpp$Frame == i))])
  }
  
  d3$BL_PP = mean(d4$PP_QC, na.rm = T)
  
  d5 = filter(d3, Treatment == "RB")
  d6 = as_tibble(d5$PP_QC)
  d6["T"] = seq(1:nrow(d6))
  d6$value[is.na(d6$value)] = mean(d6$value, na.rm = T)
  x1 = rollmean(d6$value, k = 1501)
  d3$BL_PP2 = min(x1)
  
  #p1 = p1[rep(seq_len(nrow(p1)), each = 3), ]
  write.csv(d3, paste0("Data/Presenter-Judges_FGS+P_AllT_10HzMean_",asd,"/",sub,".csv"), row.names = F)
  d7 = rbind(d7,d3)
  print("--------Bind successful---------")
  print("")
  print("")
  print("")
}

write.csv(d7, paste0("Data/Presenter-Judges_F+P_AllT_10HzMean_",asd,".csv"))

ee = Sys.time()


print("")
print("")
print("")
print("")
print("")
print("===================")
print(ee-ss)
print("===================")
