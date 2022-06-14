ss = Sys.time()

library(tidyverse)
library(dplyr)
library(readxl)
library(zoo)
library(ggpubr)

d = read.csv("Data/Presenter-Judges_FSPB+CJ+Gaze(10Hz)+Blink+ALLStats_AllT_1HzMean.csv")
d$Treatment[is.na(d$Treatment)] = "NA"
d = d[,c(1,2,3,4,5,6,188,189)]
f = read.csv("Data/BlinkSummary_GroupLevel.csv")
f["BlinkRate"] = NA

sink(paste0("Logs/Blink_Gaze_Summary_GrouptWise.txt"))

for (sub in unique(d$Participant_ID)) {
  cat(paste0("\n",sub))
  d1 = filter(d, Participant_ID == sub)
  br = mean(d1$BlinkRate, na.rm = T)
  f$BlinkRate[which(f$Participant_ID == sub)] = br
  cat(paste0(" - ", br, "    - Mutate Successful"))
}


cat("\n\n\n")

write.csv(f, "Data/Blink+Gaze_Summary_GroupLevel.csv", row.names = F)

cat(paste0("+++ Write Complete +++ \n\n\n\n"))

ee = Sys.time()

ee-ss

sink()
