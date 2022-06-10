ss = Sys.time()

library(tidyverse)
library(dplyr)
library(readxl)
library(zoo)
library(ggpubr)

d = read.csv("Data/Presenter-Judges_FSPB+CJ+Gaze+Blink+ALLStats_AllT_1HzMean.csv")
#d$Treatment[is.na(d$Treatment)] = "NA"

sink(paste0("Logs/CalculateBlinkRateSummary_TreatmentWise.txt"))

df = data.frame()
for (sub in unique(d$Participant)) {
    cat(paste0("\n\n",sub,"\n"))
    d1 = filter(d, Participant_ID == sub)
    #dft = data.frame()
    d2 = d1[order(d1$Seconds)]
    for (tr in c("NA","RB","ST","PM","DT","PR")) {
        cat(paste0("\n",tr))
        d3 = filter(d1, Treatment == tr)
    }
        
    #dft = rbind(dft, d2)
    cat("\n==============================")
    #df = rbind(df, dft)
}

cat("\n\n\n\n")
#write.csv(df, "Data/BlinkSummary_TreatmentLevel.csv", row.names = F)

ee = Sys.time()

ee-ss

sink()
