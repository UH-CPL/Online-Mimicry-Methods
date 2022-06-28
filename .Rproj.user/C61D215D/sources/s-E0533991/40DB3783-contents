ss = Sys.time()

library(tidyverse)
library(dplyr)
library(readxl)
library(zoo)
library(ggpubr)

d = read.csv("Data/Presenter-Judges_FSPB+CJ+Gaze(10Hz)+Blink+ALLStats_AllT_1HzMean.csv")
d$Treatment[is.na(d$Treatment)] = "NA"
d = d[,c(1,2,3,4,5,6,188,189)]
f = read.csv("Data/GazeSummary_TreatmentLevel.csv")
f$Treatment[is.na(f$Treatment)] = "NA"
f["BlinkRate"] = NA

sink(paste0("Logs/BlinkSummary_TreatmentLevel.txt"))

for (sub in unique(d$Participant_ID)) {
  cat(paste0("\n",sub,"\n\n"))
  d1 = filter(d, Participant_ID == sub)
  
  for (tr in unique(d1$Treatment)) {
    d2 = filter(d1, Treatment == tr)
    br = mean(d2$BlinkRate, na.rm = T)
    cat(paste0(tr," - ",br))
    f$BlinkRate[which(f$Participant_ID == sub & f$Treatment == tr)] = br
    cat("   - Mutate Successful\n")
  }
  cat("\n=========================\n")
}


cat("\n\n\n")

write.csv(f, "Data/Blink+Gaze_Summary_TreatmentLevel.csv", row.names = F)

cat(paste0("+++ Write Complete +++ \n\n\n\n"))

ee = Sys.time()

ee-ss

sink()
