library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(plotly)

#Retrieving current directory
dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)

dirn = paste0("Plots/NI_Timeseries_areaplots_1Hz",Sys.Date())
dir.create(dirn)

d = read.csv("Data/Judges-Presenter_1Hz.csv")
#d6 = read.csv("Data/Judges-Presenter_2020-07-27.csv")
#d7 = read.csv("Data/Judges-Presenter_2022-03-23.csv")
d = filter(d, newPR == 1)

dx = read.csv("Data/Participant-Group-Gender.csv")
dni = filter(dx, dx$Anticipatory.Stress == "High")$Participant
di = filter(dx, dx$Anticipatory.Stress == "Low")$Participant

source('Scripts/timeseries_stackplots.R')
source('Scripts/timeseries_stackedareabarplots.R')

scol = c(4:10)
lcol = c(40:46)
ccol = c(50:56)
rcol = c(60:66)


geom_text_size <- 2.5

emotion_cols <- c('F_Angry',
                  'F_Disgusted',
                  'F_Afraid',
                  'F_Happy',
                  'F_Sad',
                  'F_Surprised',
                  'F_Neutral')

plot_emotion_cols <- c('Angry',
                       'Disgusted',
                       'Afraid',
                       'Happy',
                       'Sad',
                       'Surprised',
                       'Neutral')

group_list <- c('B', 'C')


# NI - 1Hz
for (sub in dni) {
  print(sub)
  d1 = filter(d, Participant_ID == sub)
  d1 = d1[,c(68,19,scol,lcol,ccol,rcol,72)]
  d1["Treatment_Time_New"] = seq(0,length.out = nrow(d1), by = 1)
  #d1[c("LGaze","CGaze","RGaze","Closed")] = d1$Treatment_Time_New
  #d1[which(d1$G_Direction) == "LEFT",]
  
  f1 = fdraw_area_plot(d1,sub,"PR","bar")
  f2 = fdraw_area_plot(d1,sub,"PR","bar")
  f3 = fdraw_area_plot(d1,sub,"PR","bar")
  f4 = fdraw_area_plot(d1,sub,"PR","bar")
  f5 = fdraw_area_plot(d1,sub,"PR","bar")
  f6 = fdraw_area_plot(d1,sub,"PR","bar")
  
  x = ggarrange(f,l,c,r, ncol = 1, nrow = 6, common.legend = T)
  ggsave(x, filename = paste0(dirn,"/",sub,".pdf"))

}

