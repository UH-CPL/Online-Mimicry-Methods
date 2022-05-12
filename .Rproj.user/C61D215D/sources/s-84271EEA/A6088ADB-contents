# Adds physiology of the subject to the State of the ART judges-presenter dataset

ss = Sys.time()

library(tidyverse)
library(dplyr)
library(readxl)

dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)

source("Scripts/t2s.R")
source("Scripts/s2t.R")

asd = read.csv("Data/TimeDate.csv")$x
dir.create(paste0("Data/presenter+judges_F+P_",asd))

d = read.csv(paste0("Data/Presenter-Judges_F_30Hz_",asd,".csv"))
p = read.csv("Data/Physiological Data.csv")

d2 = read.csv("Data/colnames.csv")

sink(paste0("Logs/dc_3_",asd,".txt"))

for (sub in unique(d$Participant_ID)) {
   p1 = filter(p, Participant_ID == sub)
   #p1 = p1[rep(seq_len(nrow(p1)), each = 3), ]
   print(paste0(sub,"-",unique(p1$Participant_ID)))
   p1 = p1[,c(4:11)]
   d1 = filter(d, Participant_ID == sub)
   x = merge(d1, p1, by.x = "newTime", by.y = "Time", all.x = T)
   x = x[,c(2:69,1,70:76)]
   write.csv(x, paste0("Data/presenter+judges_F+P_",asd,"/",sub,".csv"), row.names = F)
   d2 = rbind(d2,x)
   print("--------Bind successful---------")
}

dim(d2)

write.csv(d2, paste0("Data/Presenter-Judges_F+P_AllTreatment_",Sys.Date(),".csv"), row.names = F)

print("")
print("")
print("")
print("=================================")
print("====== File Write Complete ======")
print("=================================")

sink()

ee = Sys.time()

print(ee-ss)
