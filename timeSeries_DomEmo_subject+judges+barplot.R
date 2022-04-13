library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(plotly)
library(gridExtra)
library(data.table)

#Retrieving current directory
dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)

### 1 Hz
dcopy = read.csv("Data/2021/Judges-Presenter_1Hz.csv")
d = read.csv("Data/Presenter-Judges_FGSPB_PR_1HzMean_SemiFinal_2022-04-06.csv")

dp = read.csv("Data/Participant-Group-Gender.csv")

asd = read.csv("Data/TimeDate.csv")$x

### 1 Hz
dirn = paste0("Plots/Presenter_Judges_DomEmo+Bar_PR_",asd)
dir.create(dirn)
dirni = paste0("Plots/Presenter_Judges_DomEmo+Bar_PR_",asd,"/NI")
dir.create(dirni)
diri = paste0("Plots/Presenter_Judges_DomEmo+Bar_PR_",asd,"/I")
dir.create(diri)


source("Scripts/timeseries_stackedareabarplots_DomEmo.R")

ni = unique(dp$Participant[which(dp$Anticipatory.Stress == "Low")])
i = unique(dp$Participant[which(dp$Anticipatory.Stress == "High")])




for (sub in i) {
  print(sub)
  d1 = filter(d, Participant_ID == sub)
  d1["Treatment_Time_New"] = seq(0,length.out = nrow(d1), by = 1)
  d2 = d1[c("Participant_ID","F_DomEmo","L_DomEmo","C_DomEmo","R_DomEmo","Treatment_Time_New","F_DomEmoBinary","L_DomEmoBinary","C_DomEmoBinary","R_DomEmoBinary")]
  #d1 = d1[,c(68,19,scol,lcol,ccol,rcol,72)]
  #d1[c("LGaze","CGaze","RGaze","Closed")] = d1$Treatment_Time_New
  #d1[which(d1$G_Direction) == "LEFT",]
  
  f = fdraw_area_plot_DomEmo(d2,sub,"bar",sub)
  fb = prop.table(table(d2$F_DomEmoBinary))*100
  fb = data.frame(c("Ne","Em"), c(fb[1],fb[2]))
  colnames(fb) = c("Binary","Percentage")
  fb["Emo"] = "Displayed Emotion"
  fbp = ggplot(fb, aes(x = Emo, y = Percentage, fill = Binary), xla)+geom_bar(position = "stack", stat = "identity") + labs(y = "%", x = "") +scale_fill_manual(values = c("black","lightgrey")) + theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  l = ldraw_area_plot_DomEmo(d2,sub,"bar")
  lb = prop.table(table(d2$L_DomEmoBinary))*100
  lb = data.frame(c("Ne","Em"), c(lb[1],lb[2]))
  colnames(lb) = c("Binary","Percentage")
  lb["Emo"] = "Displayed Emotion"
  lbp = ggplot(lb, aes(x = Emo, y = Percentage, fill = Binary), xla)+geom_bar(position = "stack", stat = "identity") + labs(y = "%", x = "") +scale_fill_manual(values = c("black","lightgrey")) + theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  c = cdraw_area_plot_DomEmo(d2,sub,"bar")
  cb = prop.table(table(d2$C_DomEmoBinary))*100
  cb = data.frame(c("Ne","Em"), c(cb[1],cb[2]))
  colnames(cb) = c("Binary","Percentage")
  cb["Emo"] = "Displayed Emotion"
  cbp = ggplot(cb, aes(x = Emo, y = Percentage, fill = Binary), xla)+geom_bar(position = "stack", stat = "identity") + labs(y = "%", x = "") +scale_fill_manual(values = c("black","lightgrey")) + theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  r = rdraw_area_plot_DomEmo(d2,sub,"bar")
  rb = prop.table(table(d2$R_DomEmoBinary))*100
  rb = data.frame(c("Ne","Em"), c(rb[1],rb[2]))
  colnames(rb) = c("Binary","Percentage")
  rb["Emo"] = "Displayed Emotion"
  rbp = ggplot(rb, aes(x = Emo, y = Percentage, fill = Binary), xla)+geom_bar(position = "stack", stat = "identity") + labs(y = "%", x = "") +scale_fill_manual(values = c("black","lightgrey")) + theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  #x = ggarrange(f,l,c,r, ncol = 1, nrow = 4, common.legend = T)
  
  xf = ggarrange(f, fbp, widths = c(5,1), common.legend = T)
  xl = ggarrange(l, lbp, widths = c(5,1), common.legend = T)
  xc = ggarrange(c, cbp, widths = c(5,1), common.legend = T)
  xr = ggarrange(r, rbp, widths = c(5,1), common.legend = T)
  
  x = ggarrange(f, fbp, l, lbp, c, cbp, r, rbp, widths = c(5,1,5,1,5,1,5,1), common.legend = T, align = "h", ncol = 2, nrow = 4)
  
  ggsave(x, width = 300, height = 200, units = "mm", filename = paste0(diri,"/",sub,".pdf"))
}







i = 1
for (sub in ni) {
  print(sub)
  d1 = filter(d, Participant_ID == sub)
  d1["Treatment_Time_New"] = seq(0,length.out = nrow(d1), by = 1)
  d2 = d1[c("Participant_ID","F_DomEmo","L_DomEmo","C_DomEmo","R_DomEmo","Treatment_Time_New","F_DomEmoBinary","L_DomEmoBinary","C_DomEmoBinary","R_DomEmoBinary")]
  #d1 = d1[,c(68,19,scol,lcol,ccol,rcol,72)]
  #d1[c("LGaze","CGaze","RGaze","Closed")] = d1$Treatment_Time_New
  #d1[which(d1$G_Direction) == "LEFT",]
  
  f = fdraw_area_plot_DomEmo(d2,sub,"bar",sub)
  fb = prop.table(table(d2$F_DomEmoBinary))*100
  if (dim(fb) == 1) {
    fb2 = cbind(fb, table(0))
    fb2[2] = 0
    fb = fb2
  }
  fb = data.frame(c("Ne","Em"), c(fb[1],fb[2]))
  colnames(fb) = c("Binary","Percentage")
  fb["Emo"] = "Displayed Emotion"
  fbp = ggplot(fb, aes(x = Emo, y = Percentage, fill = Binary), xla)+geom_bar(position = "stack", stat = "identity") + labs(y = "%", x = "") +scale_fill_manual(values = c("black","lightgrey")) + theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  l = ldraw_area_plot_DomEmo(d2,sub,"bar")
  lb = prop.table(table(d2$L_DomEmoBinary))*100
  lb = data.frame(c("Ne","Em"), c(lb[1],lb[2]))
  colnames(lb) = c("Binary","Percentage")
  lb["Emo"] = "Displayed Emotion"
  lbp = ggplot(lb, aes(x = Emo, y = Percentage, fill = Binary), xla)+geom_bar(position = "stack", stat = "identity") + labs(y = "%", x = "") +scale_fill_manual(values = c("black","lightgrey")) + theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  c = cdraw_area_plot_DomEmo(d2,sub,"bar")
  cb = prop.table(table(d2$C_DomEmoBinary))*100
  cb = data.frame(c("Ne","Em"), c(cb[1],cb[2]))
  colnames(cb) = c("Binary","Percentage")
  cb["Emo"] = "Displayed Emotion"
  cbp = ggplot(cb, aes(x = Emo, y = Percentage, fill = Binary), xla)+geom_bar(position = "stack", stat = "identity") + labs(y = "%", x = "") +scale_fill_manual(values = c("black","lightgrey")) + theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  r = rdraw_area_plot_DomEmo(d2,sub,"bar")
  rb = prop.table(table(d2$R_DomEmoBinary))*100
  rb = data.frame(c("Ne","Em"), c(rb[1],rb[2]))
  colnames(rb) = c("Binary","Percentage")
  rb["Emo"] = "Displayed Emotion"
  rbp = ggplot(rb, aes(x = Emo, y = Percentage, fill = Binary), xla)+geom_bar(position = "stack", stat = "identity") + labs(y = "%", x = "") +scale_fill_manual(values = c("black","lightgrey")) + theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  #x = ggarrange(f,l,c,r, ncol = 1, nrow = 4, common.legend = T)
  
  xf = ggarrange(f, fbp, widths = c(5,1), common.legend = T)
  xl = ggarrange(l, lbp, widths = c(5,1), common.legend = T)
  xc = ggarrange(c, cbp, widths = c(5,1), common.legend = T)
  xr = ggarrange(r, rbp, widths = c(5,1), common.legend = T)
  
  x = ggarrange(f, fbp, l, lbp, c, cbp, r, rbp, widths = c(5,1,5,1,5,1,5,1), common.legend = T, align = "h", ncol = 2, nrow = 4)
  
  ggsave(x, width = 300, height = 200, units = "mm", filename = paste0(dirni,"/",sub,".pdf"))
}


