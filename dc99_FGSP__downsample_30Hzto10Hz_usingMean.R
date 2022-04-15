# Downsample from 30 Hz to 1 Hz

ss = Sys.time()

library(tidyverse)
library(dplyr)
library(readxl)

dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)

d = read.csv("Data/Presenter-Judges_F+P_AllTreatment_2022-04-06.csv")

sink(paste0("Logs/dc_4_",asd,".txt"))

for (sub in unique(d$Participant_ID)) {
  d1 = filter(d, Participant_ID == sub)
  d2 = filter(d1, Treatment == "RB")
  d3 = arrange(d2, Seconds)
  
  bpp = as_tibble(d3)
  bpp$Frame = as.integer(bpp$Frame)
  bppn = bpp[,-(c(1, 19, 33, 34, 69, 71))]
  bppc = bpp[,(c(1, 19, 33, 34, 69, 71))]
  d4 = aggregate(bpp, by = list(bpp$Frame), FUN = mean, na.rm = T)
  #summarize()
  
  
  bpp["T"] = seq(1:nrow(bpp))
  rbpp2$value[is.na(rbpp2$value)] = mean(rbpp2$value, na.rm = T)
  x1 = rollmean(rbpp2$value, k = 151)
  
  #p1 = p1[rep(seq_len(nrow(p1)), each = 3), ]
  print(paste0(sub,"-",unique(p1$Participant_ID)))
  p1 = p1[,c(4:11)]
  d1 = filter(d, Participant_ID == sub)
  x = merge(d1, p1, by.x = "newTime", by.y = "Time", all.x = T)
  x = x[,c(2:69,1,70:76)]
  write.csv(x, paste0("Data/presenter+judges_F+P_",asd,"/",sub,".csv"), row.names = F)
  d3 = rbind(d3,x)
  print("--------Bind successful---------")
}

dim(d2)