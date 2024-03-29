# Add CompositeJudges

ss = Sys.time()

library(tidyverse)
library(dplyr)
library(readxl)
library(zoo)

dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)

source("Scripts/getBinary.R")
source("Scripts/getDomSumEmo.R")

dx = read.csv("Data/Presenter-Judges_FGSPB_AllT_1HzMean_SemiFinal_2022-04-06.csv")
d1 = read.csv("Data/Participant-Group-Gender.csv")
asd = read.csv("Data/TimeDate.csv")$x

d[c("CJ_Binary")] = NA

sink(paste0("Logs/dc_06_CompositeJudges_",asd,"_.txt"))

for (i in 1:nrow(d)) {
  print(i)
  if (is.na(d$L_DomEmoBinary[i])) {
    print("NA")
    next
  }
  if (!is.na(d$L_DomEmoBinary[i])) {
    d2 = d[i,c("L_DomEmoBinary","C_DomEmoBinary","R_DomEmoBinary")]
    if (sum(d2, na.rm = T) > 0) {
      d$CJ_Binary[i] = 1
      #print(d$CJ_Binary[i])
      print(d[i,c("L_DomEmoBinary","C_DomEmoBinary","R_DomEmoBinary","CJ_Binary")])
    }
    if (sum(d2, na.rm = T) == 0) {
      d$CJ_Binary[i] = 0
      #print(d$CJ_Binary[i])
      print(d[i,c("L_DomEmoBinary","C_DomEmoBinary","R_DomEmoBinary","CJ_Binary")])
    }
  }
}


sink()

ee = Sys.time()

ee-ss

write.csv(d, "Data/Presenter-Judges_FGSPB_AllT_1HzMean_SemiFinal_2022-04-06_withCompositeJBinary.csv", row.names = F)



