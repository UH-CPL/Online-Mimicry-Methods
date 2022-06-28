ss = Sys.time()

library(tidyverse)
library(dplyr)
library(readxl)
library(zoo)
library(ggpubr)

d = read.csv("Data/Presenter-Judges_FSPB+CJ+Gaze(10Hz)+Blink+ALLStats_AllT_1HzMean.csv")
d$G_WOR[is.na(d$G_WOR)] = -1

sink(paste0("Logs/Fixing_dataset_back_to_Timeseries+SetConstantFrequency+SecondsBackto10Hz.txt"))

df = data.frame()
for (sub in unique(d$Participant)) {
    d1 = filter(d, Participant_ID == sub)
    #cat(paste0("\n",sub,"\n"))
    cat(paste0("\n\n",sub," - Time = ",nrow(d1)/30," s\n\n"))
    d1 = d1[order(d1$Seconds),]
    dft = data.frame()
    for (t in unique(d1$Seconds)) {
      cat(t)
      d3 = filter(d1, Seconds == t)
      d3 = aggregate(d3, by = list(d3$G_WOR), FUN = unique)
      d3$Seconds = d3$Seconds + seq(0, by = 0.1, length.out = nrow(d3))
      #d3 = d3[,-1]
      dft = rbind(dft, d3)
      cat(paste0(" Success [", nrow(d3),"]    -    "))
    }
    x = t(sapply(dft, class))
    x = colnames(x)[which(x == "list")]
    dft[c(x)] = sapply(dft[c(x)], unlist)
    write.csv(dft, paste0("Data/Presenter-Judges_FSPB+CJ+Gaze+Blink(10Hz)+ALLStats_AllT_1HzMean_v2/",sub,".csv"),row.names = F)
    df = rbind(df, dft)
    cat("\n=============================================\n")
}

cat("\n\n\n\n")

df$G_WOR[which(df$G_WOR == -1)] = NA
write.csv(df, "Data/Presenter-Judges_FSPB+CJ+Gaze(10Hz)+Blink+ALLStats_AllT_1HzMean_v2.csv", row.names = F)

ee = Sys.time()

ee-ss

sink()
