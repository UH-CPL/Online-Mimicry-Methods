# Rearrange and Edit columns

ss = Sys.time()

library(tidyverse)
library(dplyr)
library(readxl)
library(zoo)

dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)


d = read.csv("Data/Presenter-Judges_FGSPB_PR_1HzMean_SemiFinal_2022-04-06.csv")

d5 = read.csv("Data/2021/Participant-Group-Gender.csv")



colnames(d)
#First Bad Columns
d = d[,-c(1:3,5)]

d = d[,-c(10:16,20:26)]

# LCR meta data
d = d[,-c(18,20:24,32:34,42:44)]

d = d[,-c(40:44)]

d1 = d[,c(1,18,2,79,3:9,74,54:57,10:17,19:25,75,58:61,26:32,76,62:65,33:39,77,66:69,47:53,78,70:73,40,45:46,41:44,80:107)]
d3 = data.frame()

for (sub in unique(d5$Participant)) {
  print(sub)
  d2 = filter(d1, Participant_ID == sub)
  if (unique(d2$Group) %in% c("BL","CL")) {
    d2$Group = "NI"
  }
  if (unique(d2$Group) %in% c("BH","CH")) {
    d2$Group = "I"
  }
  d2$Seconds = 1:nrow(d2)
  d2$LengthPR = nrow(d2)
  colnames(d2)[4] = "PRLength"
  d3 = rbind(d3,d2)
}


colnames(d3)
d3 = d3[,-c(18, 20:24, 76:79, 83, 85)]
write.csv(d3, "Data/Presenter-Judges_n=39_FGSPB_PR_1HzMean_COSC6323_2022-04-06.csv", row.names = F)
colnames(d3)

d4 = data.frame()
d4 = colnames(d3)

write.csv(d4,"Data/Data_Descriptor_COSC6323.csv", row.names = F)








