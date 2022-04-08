# Add CompositeJudges + DomEmo + SumEmo + Binary

ss = Sys.time()

library(tidyverse)
library(dplyr)
library(readxl)
library(zoo)

dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)

d = read.csv("Data/Presenter-Judges_FGSP_PR_1HzMean_2022-04-06.csv")

d[c("J_Angry","J_Disgusted","J_Afraid","J_Happy","J_Sad","J_Surprised","J_Neutral","F_DomEmo", "F_DomEmoBinary", "F_SumEmo", "F_SumEmoBinary","L_DomEmo", "L_DomEmoBinary", "L_SumEmo", "L_SumEmoBinary","C_DomEmo", "C_DomEmoBinary","C_SumEmo", "C_SumEmoBinary","R_DomEmo", "R_DomEmoBinary","R_SumEmo", "R_SumEmoBinary","J_DomEmo", "J_DomEmoBinary", "J_SumEmo", "J_SumEmoBinary")]