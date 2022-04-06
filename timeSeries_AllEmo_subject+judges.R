# Libraries Required and Loading
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(plotly)

# Retrieving current directory
dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)

# Load required functions
source('Scripts/timeseries_stackplots.R')
source('Scripts/timeseries_stackedareabarplots.R')

# Read 1 Hz Data File
d = read.csv("Data/Judges-Presenter_1Hz.csv")
# Filter Presentation Treatment
d = filter(d, newPR == 1)

# Make directories to store plots
dirn = paste0("Plots/1Hz_Timeseries_areaplots_DomEmo_",Sys.Date())
dir.create(dirn)
dirni = paste0("Plots/1Hz_Timeseries_areaplots_DomEmo",Sys.Date(),"/NI")
dir.create(dirni)
diri = paste0("Plots/1Hz_Timeseries_areaplots_DomEmo",Sys.Date(),"/I")
dir.create(diri)


# Read relevant Participant ID List
par_id = read.csv("Data/Participant-Group-Gender.csv")
#List of High Stress - Informed participants
i = unique(par_id$Participant[which(par_id$Anticipatory.Stress == "High")])
#List of Low Stress - Not Informed Participants
ni = unique(par_id$Participant[which(par_id$Anticipatory.Stress == "Low")])

# Subject Emotion Columns
scol = c(4:10)
# Left Judge Emotion Columns
lcol = c(40:46)
# Center Judge Emotion Columns
ccol = c(50:56)
# Right Judge Emotion Columns
rcol = c(60:66)

# Parameters for Plotting
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

# Plotting I Group
for (sub in i) {
  print(sub)
  d1 = filter(d, Participant_ID == sub)
  d1 = d1[,c(68,19,scol,lcol,ccol,rcol,72)]
  d1["Treatment_Time_New"] = seq(0,length.out = nrow(d1), by = 1)
  #d1[c("LGaze","CGaze","RGaze","Closed")] = d1$Treatment_Time_New
  #d1[which(d1$G_Direction) == "LEFT",]
  
  f = fdraw_area_plot(d1,sub,"PR","bar")
  
  l = ldraw_area_plot(d1,sub,"PR","bar")
  
  c = cdraw_area_plot(d1,sub,"PR","bar")
  
  r = rdraw_area_plot(d1,sub,"PR","bar")
  
  x = ggarrange(f,l,c,r, ncol = 1, nrow = 4, common.legend = T)
  
  ggsave(x, filename = paste0(diri,"/",sub,".pdf"))
}


# Plotting NI Group
for (sub in ni) {
  print(sub)
  d1 = filter(d, Participant_ID == sub)
  d1 = d1[,c(68,19,scol,lcol,ccol,rcol,72)]
  d1["Treatment_Time_New"] = seq(0,length.out = nrow(d1), by = 1)
  #d1[c("LGaze","CGaze","RGaze","Closed")] = d1$Treatment_Time_New
  #d1[which(d1$G_Direction) == "LEFT",]
  
  f = fdraw_area_plot(d1,sub,"PR","bar")
  
  l = ldraw_area_plot(d1,sub,"PR","bar")
  
  c = cdraw_area_plot(d1,sub,"PR","bar")
  
  r = rdraw_area_plot(d1,sub,"PR","bar")
  
  x = ggarrange(f,l,c,r, ncol = 1, nrow = 4, common.legend = T)
  
  ggsave(x, filename = paste0(dirni,"/",sub,".pdf"))
}





















# i = 1
# for (sub in ni) {
#   print(sub)
#   d1 = filter(d, Participant_ID == sub)
#   d1 = d1[,c(68,19,scol,lcol,ccol,rcol,72)]
#   d1["Treatment_Time_New"] = seq(0,length.out = nrow(d1), by = 1)
#   #d1[c("LGaze","CGaze","RGaze","Closed")] = d1$Treatment_Time_New
#   #d1[which(d1$G_Direction) == "LEFT",]
#   
#   f = fdraw_area_plot(d1,sub,"PR","bar")
#   
#   assign(paste0("f",i),f)
#   
#   # l = ldraw_area_plot(d1,sub,"PR","bar")
#   # 
#   # c = cdraw_area_plot(d1,sub,"PR","bar")
#   # 
#   # r = rdraw_area_plot(d1,sub,"PR","bar")
#   # 
#   # x = ggarrange(f,l,c,r, ncol = 1, nrow = 4, common.legend = T)
#   # 
#   # ggsave(x, filename = paste0(dirni,"/",sub,".pdf"))
#   i = i+1
# }
# x = ggarrange(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21, f22, f23, f24, f25, f26, ncol = 2, nrow = 6, common.legend = T)
# 
# x2 = ggarrange(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21, f22, f23, f24, f25, f26, ncol = 2, nrow = 13, common.legend = T)
# x3 = ggarrange(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21, f22, f23, f24, f25, f26, ncol = 2, nrow = 13, common.legend = T)
# x4 = ggarrange(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21, f22, f23, f24, f25, f26, ncol = 2, nrow = 13, common.legend = T)
# x4 = ggarrange(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, ncol = 3, nrow = 5, common.legend = T)
#  
# ggsave(x, filename = paste0(dirni,"/",sub,"NI.pdf"))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# for (sub in unique(d$Participant_ID)) {
#   print(sub)
#   d1 = filter(d, Participant_ID == sub)
#   d1 = d1[,c(68,19,scol,lcol,ccol,rcol,72)]
#   d1["Treatment_Time_New"] = seq(0,length.out = nrow(d1), by = 1/30)
#   # d1[c("LGaze","CGaze","RGaze","Closed")] = d1$Treatment_Time_New
#   # d1[which(d1$G_Direction) == "LEFT"]
#   
#   f = fdraw_signal_plot(d1,sub,"PR")
#   
#   l = ldraw_signal_plot(d1,sub,"PR")
#   
#   c = cdraw_signal_plot(d1,sub,"PR")
#   
#   r = rdraw_signal_plot(d1,sub,"PR")
#   
#   ggarrange(f,l,c,r, ncol = 1, nrow = 4, common.legend = T)
#   
#   
#   ggsave(plot = last_plot(), filename = paste0(dirn,"/",sub,".pdf"))
#   
# }
# 

