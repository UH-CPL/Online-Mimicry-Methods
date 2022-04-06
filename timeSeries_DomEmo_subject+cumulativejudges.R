library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(plotly)

#Retrieving current directory
dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)

### 1 Hz
d = read.csv("Data/Judges-Presenter_1Hz.csv")
dirn = paste0("Plots/1Hz_Timeseries_areaplots_DomEmo_",Sys.Date())
dir.create(dirn)
dirni = paste0("Plots/1Hz_Timeseries_areaplots_DomEmo_",Sys.Date(),"/NI")
dir.create(dirni)
diri = paste0("Plots/1Hz_Timeseries_areaplots_DomEmo_",Sys.Date(),"/I")
dir.create(diri)


source("Scripts/timeseries_stackedareabarplots_DomEmo.R")

ni = unique(d$Participant_ID[which(d$Group %in% c("BL","CL"))])
i = unique(d$Participant_ID[which(d$Group %in% c("BH","CH"))])




for (sub in i) {
  print(sub)
  d1 = filter(d, Participant_ID == sub)
  d1["Treatment_Time_New"] = seq(0,length.out = nrow(d1), by = 1)
  d2 = d1[c("Participant_ID","F_1","L_1","C_1","R_1","Treatment_Time_New")]
  #d1 = d1[,c(68,19,scol,lcol,ccol,rcol,72)]
  #d1[c("LGaze","CGaze","RGaze","Closed")] = d1$Treatment_Time_New
  #d1[which(d1$G_Direction) == "LEFT",]
  
  f = fdraw_area_plot_DomEmo(d1,sub,"bar")
  
  l = ldraw_area_plot_DomEmo(d1,sub,"bar")
  
  c = cdraw_area_plot_DomEmo(d1,sub,"bar")
  
  r = rdraw_area_plot_DomEmo(d1,sub,"bar")
  
  x = ggarrange(f,l,c,r, ncol = 1, nrow = 4, common.legend = T)
  
  ggsave(x, filename = paste0(diri,"/",sub,".pdf"))
}







i = 1
for (sub in ni) {
  print(sub)
  d1 = filter(d, Participant_ID == sub)
  d1 = d1[,c(68,19,scol,lcol,ccol,rcol,72)]
  d1["Treatment_Time_New"] = seq(0,length.out = nrow(d1), by = 1)
  #d1[c("LGaze","CGaze","RGaze","Closed")] = d1$Treatment_Time_New
  #d1[which(d1$G_Direction) == "LEFT",]
  
  f = fdraw_area_plot(d1,sub,"PR","bar")
  
  assign(paste0("f",i),f)
  
  # l = ldraw_area_plot(d1,sub,"PR","bar")
  # 
  # c = cdraw_area_plot(d1,sub,"PR","bar")
  # 
  # r = rdraw_area_plot(d1,sub,"PR","bar")
  # 
  # x = ggarrange(f,l,c,r, ncol = 1, nrow = 4, common.legend = T)
  # 
  # ggsave(x, filename = paste0(dirni,"/",sub,".pdf"))
  i = i+1
}
x = ggarrange(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21, f22, f23, f24, f25, f26, ncol = 2, nrow = 6, common.legend = T)

x2 = ggarrange(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21, f22, f23, f24, f25, f26, ncol = 2, nrow = 13, common.legend = T)
x3 = ggarrange(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21, f22, f23, f24, f25, f26, ncol = 2, nrow = 13, common.legend = T)
x4 = ggarrange(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21, f22, f23, f24, f25, f26, ncol = 2, nrow = 13, common.legend = T)
x4 = ggarrange(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, ncol = 3, nrow = 5, common.legend = T)

ggsave(x, filename = paste0(dirni,"/",sub,"NI.pdf"))
