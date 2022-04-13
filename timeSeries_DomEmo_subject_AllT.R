library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(plotly)

#Retrieving current directory
dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)

dp = read.csv("Data/Participant-Group-Gender.csv")

asd = read.csv("Data/TimeDate.csv")$x

### 1 Hz
d = read.csv("Data/Presenter-Judges_FGSPB_AllT_1HzMean_SemiFinal_2022-04-06.csv")
dirn = paste0("Plots/1Hz_Timeseries_areaplots_DomEmo_AllT_Subjects_",asd)
dir.create(dirn)
dirni = paste0("Plots/1Hz_Timeseries_areaplots_DomEmo_AllT_Subjects_",asd,"/NI")
dir.create(dirni)
diri = paste0("Plots/1Hz_Timeseries_areaplots_DomEmo_AllT_Subjects_",asd,"/I")
dir.create(diri)


source("Scripts/timeseries_stackedareabarplots_DomEmo.R")

ni = unique(dp$Participant[which(dp$Anticipatory.Stress == "Low")])
i = unique(dp$Participant[which(dp$Anticipatory.Stress == "High")])




for (sub in i) {
  print(sub)
  d1 = filter(d, Participant_ID == sub)
  #d1["Treatment_Time_New"] = seq(0,length.out = nrow(d1), by = 1)
  #d2 = d1[c("Participant_ID","F_1","L_1","C_1","R_1","Treatment_Time_New")]
  #d1 = d1[,c(68,19,scol,lcol,ccol,rcol,72)]
  #d1[c("LGaze","CGaze","RGaze","Closed")] = d1$Treatment_Time_New
  #d1[which(d1$G_Direction) == "LEFT",]
  
  rb = filter(d1, Treatment == "RB")
  rb["Treatment_Time_New"] = seq(0,length.out = nrow(rb), by = 1)
  d2 = rb[c("Participant_ID","F_DomEmo","Treatment_Time_New")]
  frb = fdraw_area_plot_DomEmo(d2, sub,"bar", paste0(sub," - ",unique(rb$Treatment)))
  
  st = filter(d1, Treatment == "ST")
  st["Treatment_Time_New"] = seq(0,length.out = nrow(st), by = 1)
  d2 = st[c("Participant_ID","F_DomEmo","Treatment_Time_New")]
  fst = fdraw_area_plot_DomEmo(d2,sub,"bar", unique(st$Treatment))
  
  pm = filter(d1, Treatment == "PM")
  pm["Treatment_Time_New"] = seq(0,length.out = nrow(pm), by = 1)
  d2 = pm[c("Participant_ID","F_DomEmo","Treatment_Time_New")]
  fpm = fdraw_area_plot_DomEmo(d2,sub,"bar",unique(pm$Treatment))
  
  dt = filter(d1, Treatment == "DT")
  dt["Treatment_Time_New"] = seq(0,length.out = nrow(dt), by = 1)
  d2 = dt[c("Participant_ID","F_DomEmo","Treatment_Time_New")]
  fdt = fdraw_area_plot_DomEmo(d2,sub,"bar", unique(dt$Treatment))
  
  per = filter(d1, newPR == 1)
  per["Treatment_Time_New"] = seq(0,length.out = nrow(per), by = 1)
  d2 = per[c("Participant_ID","F_DomEmo","Treatment_Time_New")]
  fper = fdraw_area_plot_DomEmo(d2,sub,"bar", "PR")
  
  
  
  x = ggarrange(frb, fst, fpm, fdt, fper, ncol = 1, nrow = 5, common.legend = T)
  
  ggsave(x, filename = paste0(diri,"/",sub,".pdf"))
}







i = 1
for (sub in ni) {
  print(sub)
  d1 = filter(d, Participant_ID == sub)
  #d1["Treatment_Time_New"] = seq(0,length.out = nrow(d1), by = 1)
  #d2 = d1[c("Participant_ID","F_1","L_1","C_1","R_1","Treatment_Time_New")]
  #d1 = d1[,c(68,19,scol,lcol,ccol,rcol,72)]
  #d1[c("LGaze","CGaze","RGaze","Closed")] = d1$Treatment_Time_New
  #d1[which(d1$G_Direction) == "LEFT",]
  
  rb = filter(d1, Treatment == "RB")
  rb["Treatment_Time_New"] = seq(0,length.out = nrow(rb), by = 1)
  d2 = rb[c("Participant_ID","F_DomEmo","Treatment_Time_New")]
  frb = fdraw_area_plot_DomEmo(d2, sub,"bar", paste0(sub," - ",unique(rb$Treatment)))
  
  st = filter(d1, Treatment == "ST")
  st["Treatment_Time_New"] = seq(0,length.out = nrow(st), by = 1)
  d2 = st[c("Participant_ID","F_DomEmo","Treatment_Time_New")]
  fst = fdraw_area_plot_DomEmo(d2,sub,"bar", unique(st$Treatment))
  
  pm = filter(d1, Treatment == "PM")
  pm["Treatment_Time_New"] = seq(0,length.out = nrow(pm), by = 1)
  d2 = pm[c("Participant_ID","F_DomEmo","Treatment_Time_New")]
  fpm = fdraw_area_plot_DomEmo(d2,sub,"bar",unique(pm$Treatment))
  
  dt = filter(d1, Treatment == "DT")
  dt["Treatment_Time_New"] = seq(0,length.out = nrow(dt), by = 1)
  d2 = dt[c("Participant_ID","F_DomEmo","Treatment_Time_New")]
  fdt = fdraw_area_plot_DomEmo(d2,sub,"bar", unique(dt$Treatment))
  
  per = filter(d1, newPR == 1)
  per["Treatment_Time_New"] = seq(0,length.out = nrow(per), by = 1)
  d2 = per[c("Participant_ID","F_DomEmo","Treatment_Time_New")]
  fper = fdraw_area_plot_DomEmo(d2,sub,"bar", "PR")
  
  
  
  x = ggarrange(frb, fst, fpm, fdt, fper, ncol = 1, nrow = 5, common.legend = T)
  
  ggsave(x, filename = paste0(dirni,"/",sub,".pdf"))
}


















x = ggarrange(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21, f22, f23, f24, f25, f26, ncol = 2, nrow = 6, common.legend = T)

x2 = ggarrange(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21, f22, f23, f24, f25, f26, ncol = 2, nrow = 13, common.legend = T)
x3 = ggarrange(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21, f22, f23, f24, f25, f26, ncol = 2, nrow = 13, common.legend = T)
x4 = ggarrange(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21, f22, f23, f24, f25, f26, ncol = 2, nrow = 13, common.legend = T)
x4 = ggarrange(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, ncol = 3, nrow = 5, common.legend = T)

ggsave(x, filename = paste0(dirni,"/",sub,"NI.pdf"))




