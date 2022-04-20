library(ggplot2)
library(hrbrthemes)
library(reshape2)
library(dplyr)
library(tidyverse)

#Retrieving current directory
dir = dirname(rstudioapi::getSourceEditorContext()$path)
datadir = paste0(dir,"/Data/convertedCSV")

setwd(datadir)
files = list.files()

file = files[1]

dd1 = as.data.frame(matrix(nrow = 0, ncol = 30))
dd2 = as.data.frame(matrix(nrow = 0, ncol = 30))
dd3 = as.data.frame(matrix(nrow = 0, ncol = 30))

count = 1

for (file in files) {
   print(count)
   d = read.csv(file)
   gt = unique(d$GroundTruth)
   intensity = unique(d$Intensity)
   
   # 10 Hz - all
   d1 = d[,c(1,2,5:22,24:31)]
   d1["Seconds_10Hz"] = round(d1$Seconds, digits = 1)
   d1 = aggregate(d1, by = list(d1$Seconds_10Hz), FUN = mean, na.rm = T)
   d1 = d1[,c(2:5,7:30)]
   d2 = d1
   d1["GroundTruth"] = gt
   d1["Intensity"] = intensity
   d1 = d1[,c(1:2,29:30,3:4,28,5:27)]
   
   # 1 Hz - all
   d2["Seconds_1Hz"] = floor(d2$Seconds_10Hz)
   if (gt != "Neutral") {
      d3 = filter(d2, Seconds_10Hz > 0.5 & Seconds_10Hz < 2.5)
   }
   if (gt == "Neutral") {
      d3 = d2
   }
   d2 = aggregate(d2, by = list(d2$Seconds_1Hz), FUN = mean, na.rm = T)
   d2["GroundTruth"] = gt
   d2["Intensity"] = intensity
   d2 = d2[,c(2:3,31:32,4:5,30,6:28)]
   
   #i Hz - filtered
   d3 = aggregate(d3, by = list(d3$Seconds_1Hz), FUN = mean, na.rm = T)
   d3["GroundTruth"] = gt
   d3["Intensity"] = intensity
   d3 = d3[,c(2:3,31:32,4:5,30,6:28)]
   
   dd1 = rbind(dd1,d1)
   dd2 = rbind(dd2,d2)
   dd3 = rbind(dd3,d3)
   
   d = ""
   d1 = ""
   d2 = ""
   d3 = ""
   count = count+1
   
}


setwd(dir)
dd1[c("DomE1","DomE2","GTBinary","FACSBinary","FACSBinary2")] = NA
s = Sys.time()
sink("Data/Binary_10Hz.txt")
for (i in 1:nrow(dd1)) {
   if (!is.na(dd1[i,8])) {
      d2 = dd1[i,8:14]
      print(i)
      print(d2)
      o1 = order(d2, decreasing = T)[1]
      o2 = order(d2, decreasing = T)[2]
      cat("\n")
      print(paste0(o1, " , ",o2))
      cat("\n")
      c1 = strsplit(colnames(d2)[o1], split = "_")[[1]][2]
      c2 = strsplit(colnames(d2)[o2], split = "_")[[1]][2]
      dd1$DomE1[i] = c1
      dd1$DomE2[i] = c2
      if (dd1$GroundTruth[i] == "Neutral") {
         dd1$GTBinary[i] = 0
      }
      else{
         dd1$GTBinary[i] = 1
      }
      if (dd1$DomE1[i] == "Neutral") {
         dd1$FACSBinary[i] = 0
      }
      else{
         dd1$FACSBinary[i] = 1
      }
      if (dd1$DomE2[i] == "Neutral") {
         dd1$FACSBinary2[i] = 0
      }
      else{
         dd1$FACSBinary2[i] = 1
      }
      cat("\n")
      print(paste0(dd1$GroundTruth[i]," - ",dd1$GTBinary[i]))
      print(paste0(dd1$DomE1[i]," - ",dd1$FACSBinary[i]))
      print(paste0(dd1$DomE2[i]," - ",dd1$FACSBinary2[i]))
      cat(("\n\n\n\n\n\n"))
   }
   
}
e = Sys.time()
print(e-s)
sink()




setwd(dir)
dd2[c("DomE1","DomE2","GTBinary","FACSBinary","FACSBinary2")] = NA
s = Sys.time()
sink("Data/Binary_1Hz.txt")
for (i in 1:nrow(dd2)) {
   if (!is.na(dd2[i,8])) {
      d2 = dd2[i,8:14]
      print(i)
      print(d2)
      o1 = order(d2, decreasing = T)[1]
      o2 = order(d2, decreasing = T)[2]
      cat("\n")
      print(paste0(o1, " , ",o2))
      cat("\n")
      c1 = strsplit(colnames(d2)[o1], split = "_")[[1]][2]
      c2 = strsplit(colnames(d2)[o2], split = "_")[[1]][2]
      dd2$DomE1[i] = c1
      dd2$DomE2[i] = c2
      if (dd2$GroundTruth[i] == "Neutral") {
         dd2$GTBinary[i] = 0
      }
      else{
         dd2$GTBinary[i] = 1
      }
      if (dd2$DomE1[i] == "Neutral") {
         dd2$FACSBinary[i] = 0
      }
      else{
         dd2$FACSBinary[i] = 1
      }
      if (dd2$DomE2[i] == "Neutral") {
         dd2$FACSBinary2[i] = 0
      }
      else{
         dd2$FACSBinary2[i] = 1
      }
      cat("\n")
      print(paste0(dd2$GroundTruth[i]," - ",dd2$GTBinary[i]))
      print(paste0(dd2$DomE1[i]," - ",dd2$FACSBinary[i]))
      print(paste0(dd2$DomE2[i]," - ",dd2$FACSBinary2[i]))
      cat(("\n\n\n\n\n\n"))
   }
   
}
e = Sys.time()
print(e-s)
sink()






setwd(dir)
dd3[c("DomE1","DomE2","GTBinary","FACSBinary","FACSBinary2")] = NA
s = Sys.time()
sink("Data/Binary_1Hz_filtered.txt")
for (i in 1:nrow(dd3)) {
   if (!is.na(dd3[i,8])) {
      d2 = dd3[i,8:14]
      print(i)
      print(d2)
      o1 = order(d2, decreasing = T)[1]
      o2 = order(d2, decreasing = T)[2]
      cat("\n")
      print(paste0(o1, " , ",o2))
      cat("\n")
      c1 = strsplit(colnames(d2)[o1], split = "_")[[1]][2]
      c2 = strsplit(colnames(d2)[o2], split = "_")[[1]][2]
      dd3$DomE1[i] = c1
      dd3$DomE2[i] = c2
      if (dd3$GroundTruth[i] == "Neutral") {
         dd3$GTBinary[i] = 0
      }
      else{
         dd3$GTBinary[i] = 1
      }
      if (dd3$DomE1[i] == "Neutral") {
         dd3$FACSBinary[i] = 0
      }
      else{
         dd3$FACSBinary[i] = 1
      }
      if (dd3$DomE2[i] == "Neutral") {
         dd3$FACSBinary2[i] = 0
      }
      else{
         dd3$FACSBinary2[i] = 1
      }
      cat("\n")
      print(paste0(dd3$GroundTruth[i]," - ",dd3$GTBinary[i]))
      print(paste0(dd3$DomE1[i]," - ",dd3$FACSBinary[i]))
      print(paste0(dd3$DomE2[i]," - ",dd3$FACSBinary2[i]))
      cat(("\n\n\n\n\n\n"))
   }
   
}
e = Sys.time()
print(e-s)
sink()







write.csv(dd1, "RAVDESS_labfacs_10Hz_v2.csv", row.names = F)
write.csv(dd2, "RAVDESS_labfacs_1Hz_v2.csv", row.names = F)
write.csv(dd3, "RAVDESS_labfacs_1Hz_filtered_v2.csv", row.names = F)



