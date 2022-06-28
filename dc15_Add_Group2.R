ss = Sys.time()

library(tidyverse)
library(dplyr)
library(readxl)
library(zoo)
library(ggpubr)
library(scales)
library(rstatix)

sink("Logs/Add_Group2")

d = read.csv("Data/Presenter-Judges_FSPB+CJ+Gaze(10Hz)+Blink+ALLStats_AllT_1HzMean_v4.csv")
d["Group2"] = NA
d$Group2[which(d$Group %in% c("CH","BH"))] = "I"
d$Group2[which(d$Group %in% c("CL","BL"))] = "NI"
write.csv(d, "Data/Presenter-Judges_FSPB+CJ+Gaze(10Hz)+Blink+ALLStats_AllT_1HzMean_v4.csv", row.names = F)
cat("Presenter-Judges_FSPB+CJ+Gaze(10Hz)+Blink+ALLStats_AllT_1HzMean_v4.csv  -  Written\n")


d1 = read.csv("Data/Blink+Gaze_Summary_TreatmentLevel_v3.csv")
d1["Group2"] = NA
d1$Group2[which(d1$Group %in% c("CH","BH"))] = "I"
d1$Group2[which(d1$Group %in% c("CL","BL"))] = "NI"
write.csv(d1, "Data/Blink+Gaze_Summary_TreatmentLevel_v3.csv", row.names = F)
cat("Blink+Gaze_Summary_TreatmentLevel_v3.csv  -  Written\n")

d2 = read.csv("Data/Blink+Gaze_Summary_GroupLevel_v3.csv")
d2["Group2"] = NA
d2$Group2[which(d2$Group %in% c("CH","BH"))] = "I"
d2$Group2[which(d2$Group %in% c("CL","BL"))] = "NI"
write.csv(d2, "Data/Blink+Gaze_Summary_GroupLevel_v3.csv", row.names = F)
cat("Blink+Gaze_Summary_GroupLevel_v3.csv  -  Written\n")

d3 = read.csv("Data/Presenter-Judges_AllSummary_AllT_1HzMean_Clean_v3.csv")
d3["Group2"] = NA
d3$Group2[which(d3$Group %in% c("CH","BH"))] = "I"
d3$Group2[which(d3$Group %in% c("CL","BL"))] = "NI"
write.csv(d3, "Data/Presenter-Judges_AllSummary_AllT_1HzMean_Clean_v3.csv", row.names = F)
cat("Presenter-Judges_AllSummary_AllT_1HzMean_Clean_v3.csv  -  Written\n")


cat("\n\n")

ee = Sys.time()

ee-ss

sink()
