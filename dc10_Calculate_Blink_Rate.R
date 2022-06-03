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
    cat("\n\n\n\n==============================\n")
    cat(paste0("\n\n\n"),sub,"\n")
    d1 = filter(d, Participant_ID == sub)
    
    for (tr in unique(d1$Treatment)) {
        cat(paste0("\n",tr,"\n"))
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
        
        # plot(y = d2$BlinkStatus, x =  d2$Treatment_Time, type = "l", xlab = "Time [s]", ylab = "Blink Status", main = unique(d2$Treatment), ylim = c(0,1))
        p = ggplot(d2, aes(x = Treatment_Time, y = BlinkStatus)) + geom_line() + theme_bw() + xlab("") + ylab(tr) + scale_y_continuous(breaks = c(0,1)) + theme(axis.title.y = element_text(angle = 0, face = "bold", size = 13), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
        # ggplot(d3, aes(x = Treatment_Time, y = BlinkStatus)) + geom_line() + theme_bw() + xlab("") + ylab(tr) + scale_y_continuous(breaks = c(0,1)) + theme(axis.title.y = element_text(angle = 0, face = "bold", size = 18))
        
        if (tr == "PR") {
          p = p + xlab("Time [s]")
        }
      
        assign(paste0("p",tr),p)
        
        mins = max(d2$Treatment_Time, na.rm = T)
        mins = mins/60
        sec = (mins - as.integer(mins))*60
        mins = as.integer(mins)
        
        for (i in 1:(mins+1)) {
            cat(paste0(i,"    "))
            if (i < (mins+1)) {
                start = (i-1)*60
                end = start+59
                print(paste0(start," - ",end))
                d3 = d2[which(d2$Treatment_Time >= start & d2$Treatment_Time <= end),]
                if (dim(table(rle(d3$BlinkStatus)))[2] > 1) {
                  blrate = table(rle(d3$BlinkStatus)$values)[[2]]
                }
                if (dim(table(rle(d3$BlinkStatus)))[2] == 1) {
                  if (is.na(unique(rle(d3$BlinkStatus)$values)[1])) {
                    blrate = NA
                    next
                  }
                  if (unique(rle(d3$BlinkStatus)$values)[1] == 1) {
                    blrate = 999
                  }
                  if (unique(rle(d3$BlinkStatus)$values)[1] == 0) {
                    blrate = 0
                  }
                }
                d2$BlinkRate[which(d2$Treatment_Time >= start & d2$Treatment_Time <= end)] = blrate
            }

            if (i == (mins+1)) {
                start = end+1
                end = start+sec
                print(paste0(start," - ",end))
                d3 = d2[which(d2$Treatment_Time >= start & d2$Treatment_Time <= end),]
                if (dim(table(rle(d3$BlinkStatus)))[2] > 1) {
                  blrate = table(rle(d3$BlinkStatus)$values)[[2]]
                }
                if (dim(table(rle(d3$BlinkStatus)))[2] == 1) {
                  if (unique(rle(d3$BlinkStatus)$values) == 1) {
                    blrate = 999
                  }
                  if (unique(rle(d3$BlinkStatus)$values) == 0) {
                    blrate = 0
                  }
                }
                d2$BlinkRate[which(d2$Treatment_Time >= start & d2$Treatment_Time <= end)] = blrate
            }
          
        }
    }
    pg = ggarrange(pNA, pRB, pST, pPM, pDT, pPR, nrow = 6)
    pg = annotate_figure(pg, top = text_grob(paste0(sub,"  -  Blink Status"), face = "bold", size = 14))
    

}


ee = Sys.time()

ee-ss

write.csv(d, "Data/Presenter-Judges_FGSPB_AllT_1HzMean_SemiFinal_2022-04-06_withCompositeJBinary.csv", row.names = F)

sink()