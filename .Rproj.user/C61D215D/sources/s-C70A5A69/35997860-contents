ss = Sys.time()

library(tidyverse)
library(dplyr)
library(readxl)

dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)

source("Scripts/t2s.R")
source("Scripts/s2t.R")

d = read.csv("Data/subject_n=57_F_10Hz.csv")
d1 = d[rep(seq_len(nrow(d)), each = 3), ]
d1["newPR"] = NA

jd = read.csv("Data/All_J_F_30Hz_NO_Duplicates.csv")

f = read_xlsx("Data/F_sync_info.xlsx")
f = as.data.frame(f)
f$FacePRStart = as.character(f$FacePRStart)
f$FacePREnd = as.character(f$FacePREnd)
f = f[!is.na(f$FacePREnd),]
f$Subject = paste0("T",f$Subject)

a = 3465

d2 = data.frame(matrix(ncol = (ncol(d1)+ncol(jd)), nrow = 0))
colnames(d2) = append(colnames(d1),colnames(jd))

asd = Sys.Date()

dir.create(paste0("Data/XXsubjectFiles_",asd))

sink(paste0("Logs/dc_2_",asd,".txt"))

for (sub in f$Subject) {
   print(sub)
   x = filter(d1, Participant_ID == sub)
   y = filter(f, Subject == sub)
   y = y[,c(1,6,7)]
   s = t2s(y$FacePRStart)
   e = t2s(y$FacePREnd)
   #print(s)
   print(paste0(y$FacePREnd,"-",y$FacePRStart))
   x[(which(x$Seconds >=s & x$Seconds <= e)),36] = 1
   m = dim(x[which(x$newPR == 1),])[1]
   m = a+m
   jd1 = filter(jd, MainFrame >= a & MainFrame < m)
   x[(which(x$Seconds == s)[1]:which(x$Seconds == e)[3]),37:69] = jd1
   write.csv(x, paste0("Data/XXsubjectFiles_",asd,"/",sub,".csv"), row.names = F)
   d2 = rbind(d2,x)
   print("--------Bind successful---------")
}

dim(d2)


write.csv(d2, paste0("Data/Judges-Presenter_",asd,"_A.csv"), row.names = F)
print("")
print("")
print("")
print("=================================")
print("====== File Write Complete ======")
print("=================================")

sink()

write.csv(asd,"Data/TimeDate.csv", row.names = F)

ee = Sys.time()

print(ee-ss)

