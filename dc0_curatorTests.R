library(tidyverse)
library(dplyr)

dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)

source("Scripts/s2t.r")

d = read.csv("Data/subject_n=57_F_10Hz.csv")

jd = read.csv("Data/All_J_F_Downsampled_10Hz.csv")

j = read.csv("Data/All_J_F_Raw.csv")

p = read.csv("Data/Physiological Data.csv")



d1 = select(d,"Participant_ID","Frame","Seconds","newTime","Treatment","Group")
x = filter(d1, Participant_ID == "T021")
x = filter(x, Treatment == "PR")
s2t(tail(x$Seconds,1))

d[which((d$Participant_ID == "T032") & (d$Seconds >= 4812) & (d$Seconds <= 4820)),34] = "PR"

x1 = d[which((d$Participant_ID == "T005") & (d$Seconds >= 4811) & (d$Seconds <= 4821)),]
