library(tidyverse)
library(caret)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
d = read.csv("Data/CM.csv")
# g = read.csv("Data/GroundTruth_SelectionPool.csv")
# h = read.csv("Data/GroundTruth_Gaze_DataFile.csv")
# f = read.csv("Data/Randompool_with_key_2000.csv")
# e = read.csv("Data/Presenter-Judges_FSPB+CJ+Gaze(10Hz)+Blink+ALLStats_AllT_1HzMean_v4.csv")
k = read.csv("Data/AllGazeTest.csv")

d = d[1:500,]

head(d)
d["AlgoOutput"] = NA
#d["UID2"] = NA

for (i in 1:nrow(d)) {
  print(i)
  s = d$Participant_ID[i]
  t = d$Seconds[i]
  x = which(k$Participant_ID == s & k$Seconds == t)
  d$AlgoOutput[i] = k$G_Direction[x]
}

d = d[c(1:4,6:10,5,11)]

d$AlgoOutput[which(d$AlgoOutput == "CLOSED")] = "X"
d$AlgoOutput[which(d$AlgoOutput == "LEFT")] = "L"
d$AlgoOutput[which(d$AlgoOutput == "CENTER")] = "C"
d$AlgoOutput[which(d$AlgoOutput == "RIGHT")] = "R"
d$AlgoOutput[which(d$AlgoOutput == "")] = NA
d1 = d[-which(is.na(d$AlgoOutput)),]

gt = factor(d$AlgoOutput2)
pred = factor(d$Aman)
cmm = confusionMatrix(data = pred, reference = gt)

l = filter(d, AlgoOutput == "L")
c = filter(d, AlgoOutput == "C")
r = filter(d, AlgoOutput == "R")
xx = filter(d, AlgoOutput == "X")

lcm = confusionMatrix(reference = factor(l$Aman), data = factor(l$AlgoOutput))
ccm = confusionMatrix(reference = factor(c$Aman), data = factor(c$AlgoOutput))
rcm = confusionMatrix(reference = factor(r$Aman), data = factor(r$AlgoOutput))
xcm = confusionMatrix(reference = factor(xx$Aman), data = factor(xx$AlgoOutput))

d2 = d
d2 = filter(d2, AlgoOutput2 == "L")
d2$AlgoOutput2[which(d2$AlgoOutput2 != "L")] = "M"
d2["T"] = 0
for (j in 1:nrow(d2)) {
  print(j)
  if (d2$Aman[j] == d2$AlgoOutput2[j]) {
    d2$T[j] = 1
  }
  
}



