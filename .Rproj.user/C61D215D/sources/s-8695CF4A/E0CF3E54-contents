# Libraries Required and Loading
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(plotly)

# Retrieving current directory
dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)


# Read 1 Hz Data File
d = read.csv("Data/Judges-Presenter_1Hz.csv")
d["DM_"]

for (sub in unique(d$Participant_ID)) {
  d1 = filter(d, Participant_ID == sub)
  x = table(d1$J_1)
  x = prop.table(table(d1$J_1))*100
  
}
