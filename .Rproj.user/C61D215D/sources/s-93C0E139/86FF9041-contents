ss = Sys.time()

library(tidyverse)
library(dplyr)
library(readxl)
library(zoo)

d = read.csv("Data/Presenter-Judges_FSPB+CJ+Gaze+ALLStats_AllT_1HzMean_Clean.csv")
d$Treatment[is.na(d$Treatment)] = "NA"
d$Treatment_Time = ceiling(d$Treatment_Time)


sink(paste0("Logs/CalculateBlinkRate.txt"))

for (sub in unique(d$Participant)) {
    cat(paste0("\n\n"),sub,"\n")
    d1 = filter(d, Participant_ID == sub)
    
    for (tr in unique(d1$Treatment)) {
        print(tr)
        d2 = filter(d1, Treatment == tr)
        if (tr == "NA") {
          d2$Treatment_Time = rep(seq(0, by = 1, length.out = nrow(d2)), each = 30)[1:nrow(d2)]
        }
        l = d3$G_WOR[which(d3$G_Direction == "LEFT")]
        c = d3$G_WOR[which(d3$G_Direction == "CENTER")]
        r = d3$G_WOR[which(d3$G_Direction == "RIGHT")]
        cl = d3$G_WOR[which(d3$G_Direction == "CLOSED")]
        d2[c("BlinkStatus","BlinkRate")] = NA
        d2$BlinkStatus[which(d2$G_WOR < 5.7)] = 0
        d2$BlinkStatus[which(d2$G_WOR >= 5.7)] = 1
        
        plot(y = d2$BlinkStatus, x =  d2$Treatment_Time, type = "l", xlab = "Time [s]", ylab = "Blink Status", main = unique(d2$Treatment), ylim = c(0,1))
        
        mins = max(d2$Treatment_Time, na.rm = T)
        mins = mins/60
        sec = (mins - as.integer(mins))*60
        mins = as.integer(mins)
        
        for (i in 1:(mins+1)) {
            print(i)
            start = (i-1)*60
            end = 
            print(start)
        }
    }
}

sink()

ee = Sys.time()

ee-ss

write.csv(d, "Data/Presenter-Judges_FGSPB_AllT_1HzMean_SemiFinal_2022-04-06_withCompositeJBinary.csv", row.names = F)
