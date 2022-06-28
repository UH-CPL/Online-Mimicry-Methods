ss = Sys.time()

library(tidyverse)
library(dplyr)
library(readxl)
library(zoo)
library(ggpubr)
library(scales)
library(rstatix)


d = read.csv("Data/Presenter-Judges_FSPB+CJ+Gaze(10Hz)+Blink+ALLStats_AllT_1HzMean.csv")
d$BlinkRate[which(d$BlinkRate == -1)] = NA
d$BlinkRate[which(d$BlinkRate == 999)] = NA
d$BlinkRate[which(d$BlinkRate > 60)] = NA
write.csv(d, "Data/Presenter-Judges_FSPB+CJ+Gaze(10Hz)+Blink+ALLStats_AllT_1HzMean.csv", row.names = F)

f = read.csv("Data/BlinkSummary_GroupLevel.csv")