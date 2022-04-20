library(ggplot2)
library(hrbrthemes)
library(reshape2)
library(dplyr)
library(tidyverse)

#Retrieving current directory
dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)
datadir = paste0(dir,"/Data/convertedCSV")

source("Scripts/createMatrix_new_facsN.R")
source("Scripts/heatmap_improved.R")
source("Scripts/renameMatrix.R")

files = list.files(datadir)




#d1 = as.data.frame(matrix(nrow = 0, ncol = 31))
# i = 1
# for (file in files) {
#    print(paste0(i," - ",file))
#    file = paste0(dir,"/Data/convertedCSV/",file)
#    d = read.csv(file)
#    d1 = rbind(d1,d)
#    i = i+1
#    
# }
# 
# write.csv(d1, "labFACS_on_ravdess_withRownames.csv")

sink("Data/BinaryLog.txt")
s = Sys.time()
print(s)
cat("\n\n\n\n\n\n")

d = read.csv("labFACS_on_ravdess.csv")
d1 = d

d = d1

d[c("DomE1","DomE2","GTBinary","FACSBinary","FACSBinary2")] = NA

for (i in 1:nrow(d)) {
   if (!is.na(d[i,8])) {
      d2 = d[i,8:14]
      print(i)
      print(d2)
      o1 = order(d2, decreasing = T)[1]
      o2 = order(d2, decreasing = T)[2]
      cat("\n")
      print(paste0(o1, " , ",o2))
      cat("\n")
      c1 = strsplit(colnames(d2)[o1], split = "_")[[1]][2]
      c2 = strsplit(colnames(d2)[o2], split = "_")[[1]][2]
      d$DomE1[i] = c1
      d$DomE2[i] = c2
      if (d$GroundTruth[i] == "Neutral") {
         d$GTBinary[i] = 0
      }
      else{
         d$GTBinary[i] = 1
      }
      if (d$DomE1[i] == "Neutral") {
         d$FACSBinary[i] = 0
      }
      else{
         d$FACSBinary[i] = 1
      }
      if (d$DomE2[i] == "Neutral") {
         d$FACSBinary2[i] = 0
      }
      else{
         d$FACSBinary2[i] = 1
      }
      cat("\n")
      print(paste0(d$GroundTruth[i]," - ",d$GTBinary[i]))
      print(paste0(d$DomE1[i]," - ",d$FACSBinary[i]))
      print(paste0(d$DomE2[i]," - ",d$FACSBinary2[i]))
      cat(("\n\n\n\n\n\n"))
   }

}
e = Sys.time()
print(e-s)

sink()

write.csv(d, "ravdess-allsubjects-labFACS.csv", row.names = F)



dd = filter(d, GroundTruth %in% c("Neutral","Happy","Sad","Sad","Angry","Disgusted","Surprised"))
dd = dd[which(!is.na(dd$F_Afraid)),]
dd = filter(dd, Seconds > 0.5)
dd = filter(dd, Seconds < 2.5)

ddn = filter(dd, dd$GTBinary == 0)
dde = filter(dd, dd$GTBinary == 1)


table(ddn$FACSBinary)

table(dde$FACSBinary)