library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(plotly)

#Retrieving current directory
dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)

dirn = paste0("Plots/XXTimeseries_areaplots_",Sys.Date())
dir.create(dirn)

d = read.csv(paste0("Data/Judges-Presenter_F+P_newPR_",Sys.Date(),".csv"))
d6 = read.csv("Data/Judges-Presenter_2020-07-27.csv")
d7 = read.csv("Data/Judges-Presenter_2022-03-23.csv")
d = filter(d, newPR == 1)


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


# 30 Hz
for (sub in unique(d$Participant_ID)) {
   print(sub)
   d1 = filter(d, Participant_ID == sub)
   d1 = d1[,c(68,19,scol,lcol,ccol,rcol,72)]
   d1["Treatment_Time_New"] = seq(0,length.out = nrow(d1), by = 1/30)
   #d1[c("LGaze","CGaze","RGaze","Closed")] = d1$Treatment_Time_New
   #d1[which(d1$G_Direction) == "LEFT",]
   
   f = fdraw_area_plot(d1,sub,"PR","bar")
   
   l = ldraw_area_plot(d1,sub,"PR","bar")
   
   c = cdraw_area_plot(d1,sub,"PR","bar")
   
   r = rdraw_area_plot(d1,sub,"PR","bar")
   
   #pdf(paste0(dirn,"/",sub,".pdf"))
   
   x = ggarrange(f,l,c,r, ncol = 1, nrow = 4, common.legend = T)
   
   #dev.off()
   
   ggsave(x, filename = paste0(dirn,"/",sub,".pdf"))
}



## 10 Hz
d2 = read.csv("Data/Judges-Presenter_10Hz.csv")
d2 = filter(d2, newPR == 1)
dirn = paste0("Plots/10Hz_Timeseries_areaplots_",Sys.Date())
dir.create(dirn)

for (sub in unique(d$Participant_ID)) {
   print(sub)
   d1 = filter(d2, Participant_ID == sub)
   d1 = d1[,c(68,19,scol,lcol,ccol,rcol,72)]
   d1["Treatment_Time_New"] = seq(0,length.out = nrow(d1), by = 1/10)
   #d1[c("LGaze","CGaze","RGaze","Closed")] = d1$Treatment_Time_New
   #d1[which(d1$G_Direction) == "LEFT",]
   
   f = fdraw_area_plot(d1,sub,"PR","bar")
   
   l = ldraw_area_plot(d1,sub,"PR","bar")
   
   c = cdraw_area_plot(d1,sub,"PR","bar")
   
   r = rdraw_area_plot(d1,sub,"PR","bar")
   
   #pdf(paste0(dirn,"/",sub,".pdf"))
   
   x = ggarrange(f,l,c,r, ncol = 1, nrow = 4, common.legend = T)
   
   #dev.off()
   
   ggsave(x, filename = paste0(dirn,"/",sub,".pdf"))
}


### 1 Hz
d3 = read.csv("Data/Judges-Presenter_1Hz.csv")
d3 = filter(d3, newPR == 1)
dirn = paste0("Plots/1Hz_Timeseries_areaplots_",Sys.Date())
dir.create(dirn)

for (sub in unique(d$Participant_ID)) {
   print(sub)
   d1 = filter(d3, Participant_ID == sub)
   d1 = d1[,c(68,19,scol,lcol,ccol,rcol,72)]
   d1["Treatment_Time_New"] = seq(0,length.out = nrow(d1), by = 1)
   #d1[c("LGaze","CGaze","RGaze","Closed")] = d1$Treatment_Time_New
   #d1[which(d1$G_Direction) == "LEFT",]
   
   f = fdraw_area_plot(d1,sub,"PR","bar")
   
   l = ldraw_area_plot(d1,sub,"PR","bar")
   
   c = cdraw_area_plot(d1,sub,"PR","bar")
   
   r = rdraw_area_plot(d1,sub,"PR","bar")
   
   x = ggarrange(f,l,c,r, ncol = 1, nrow = 4, common.legend = T)
   
   ggsave(x, filename = paste0(dirn,"/",sub,".pdf"))
}











for (sub in unique(d$Participant_ID)) {
   print(sub)
   d1 = filter(d, Participant_ID == sub)
   d1 = d1[,c(68,19,scol,lcol,ccol,rcol,72)]
   d1["Treatment_Time_New"] = seq(0,length.out = nrow(d1), by = 1/30)
   # d1[c("LGaze","CGaze","RGaze","Closed")] = d1$Treatment_Time_New
   # d1[which(d1$G_Direction) == "LEFT"]
   
   f = fdraw_signal_plot(d1,sub,"PR")
   
   l = ldraw_signal_plot(d1,sub,"PR")
   
   c = cdraw_signal_plot(d1,sub,"PR")
   
   r = rdraw_signal_plot(d1,sub,"PR")
   
   ggarrange(f,l,c,r, ncol = 1, nrow = 4, common.legend = T)
   
   
   ggsave(plot = last_plot(), filename = paste0(dirn,"/",sub,".pdf"))
   
}


