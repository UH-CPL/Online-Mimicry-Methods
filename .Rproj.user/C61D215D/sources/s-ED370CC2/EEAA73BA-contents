# Create Log for checks
sink(paste0("Logs/Final_Dataset_CheckLog_",Sys.Date(),".txt"))

# Libraries needed
library(tidyverse)

#Retrieving current directory
dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)

d = read.csv("Data/Judges-Presenter_1Hz.csv")


d1 = read.csv("Data/subject+judges_F+G+S_allTreatment/T005.csv")
d2 = read.csv("Data/subject+judges_F+G+S+P_original_allTreatment/T005.csv")
d3 = read.csv("Data/subjectFiles_new/T005.csv")
