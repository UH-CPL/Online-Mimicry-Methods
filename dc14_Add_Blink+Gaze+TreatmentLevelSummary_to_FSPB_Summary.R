ss = Sys.time()

library(tidyverse)
library(dplyr)
library(readxl)
library(zoo)
library(ggpubr)
library(scales)
library(rstatix)

d = read.csv("Data/Blink+Gaze_Summary_TreatmentLevel.csv")
d$Treatment[is.na(d$Treatment)] = "NA"

f = read.csv("Data/Presenter-Judges_Summary_AllT_1HzMean_Clean_v2.csv")
f$Treatment[is.na(f$Treatment)] = "NA"
f[c("Closed","Left","Right","Center","BlinkRate")] = NA


sink(paste0("Logs/Add_Blink_Gaze_to_FSPB_CJ_ALLStats_Clean.txt"))

for (sub in unique(d$Participant_ID)) {
  cat(paste0("\n",sub,"\n\n"))
  d1 = filter(d, Participant_ID == sub)
  
  for (tr in unique(d1$Treatment)) {
    cat(paste0(tr,"\n"))
    d2 = filter(d1, Treatment == tr)
    f[which(f$Participant_ID == sub & f$Treatment == tr),c(133:137)] = d2[c(7:10,12)]
  }
}


cat("\n============================\n")
cat("\n\n\n")

write.csv(f, "Data/Presenter-Judges_AllSummary_AllT_1HzMean_Clean.csv", row.names = F)

cat(paste0("+++ Write Complete +++ \n\n\n\n"))

ee = Sys.time()

ee-ss

sink()
