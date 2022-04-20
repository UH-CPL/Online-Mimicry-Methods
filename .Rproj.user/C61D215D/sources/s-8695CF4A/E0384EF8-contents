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
dirn = paste0("Plots/Presenter_CumulativeJudges_DomEmo+Bar_PR_",asd)
dir.create(dirn)
dirni = paste0("Plots/Presenter_CumulativeJudges_DomEmo+Bar_PR_",asd,"/NI")
dir.create(dirni)
diri = paste0("Plots/Presenter_CumulativeJudges_DomEmo+Bar_PR_",asd,"/I")
dir.create(diri)


source("Scripts/timeseries_stackedareabarplots_DomEmo.R")

ni = unique(dp$Participant[which(dp$Anticipatory.Stress == "Low")])
i = unique(dp$Participant[which(dp$Anticipatory.Stress == "High")])




for (sub in i) {
  print(sub)
  d1 = filter(d, Participant_ID == sub)
  d1["Treatment_Time_New"] = seq(0,length.out = nrow(d1), by = 1)
  d2 = d1[c("Participant_ID","F_DomEmo","J_DomEmo","Treatment_Time_New","J_DomEmoBinary","F_DomEmoBinary")]
  #d1 = d1[,c(68,19,scol,lcol,ccol,rcol,72)]
  #d1[c("LGaze","CGaze","RGaze","Closed")] = d1$Treatment_Time_New
  #d1[which(d1$G_Direction) == "LEFT",]
  
  f = fdraw_area_plot_DomEmo(d2,sub,"bar",sub)
  fb = prop.table(table(d2$F_DomEmoBinary))*100
  fb = data.frame(c("Ne","Em"), c(fb[1],fb[2]))
  colnames(fb) = c("Binary","Percentage")
  fb["Emo"] = "Displayed Emotion"
  fbp = ggplot(fb, aes(x = Emo, y = Percentage, fill = Binary), xla)+geom_bar(position = "stack", stat = "identity") + labs(y = "%", x = "") +scale_fill_manual(values = c("black","lightgrey")) + theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  j = jdraw_area_plot_DomEmo(d2,sub,"bar")
  jb = prop.table(table(d2$J_DomEmoBinary))*100
  jb = data.frame(c("Ne","Em"), c(jb[1],jb[2]))
  colnames(jb) = c("Binary","Percentage")
  jb["Emo"] = "Displayed Emotion"
  jbp = ggplot(jb, aes(x = Emo, y = Percentage, fill = Binary), xla)+geom_bar(position = "stack", stat = "identity") + labs(y = "%", x = "") +scale_fill_manual(values = c("black","lightgrey")) + theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  
  x = ggarrange(f, fbp, j, jbp, widths = c(5,1,5,1), common.legend = T, align = "h", ncol = 2, nrow = 2)
  
  ggsave(x, width = 300, height = 150, units = "mm", filename = paste0(diri,"/",sub,".pdf"))
}







i = 1
for (sub in ni) {
  print(sub)
  d1 = filter(d, Participant_ID == sub)
  d1["Treatment_Time_New"] = seq(0,length.out = nrow(d1), by = 1)
  d2 = d1[c("Participant_ID","F_DomEmo","J_DomEmo","Treatment_Time_New","J_DomEmoBinary","F_DomEmoBinary")]
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
  
  j = jdraw_area_plot_DomEmo(d2,sub,"bar")
  jb = prop.table(table(d2$J_DomEmoBinary))*100
  jb = data.frame(c("Ne","Em"), c(jb[1],jb[2]))
  colnames(jb) = c("Binary","Percentage")
  jb["Emo"] = "Displayed Emotion"
  jbp = ggplot(jb, aes(x = Emo, y = Percentage, fill = Binary), xla)+geom_bar(position = "stack", stat = "identity") + labs(y = "%", x = "") +scale_fill_manual(values = c("black","lightgrey")) + theme_bw() + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  
  x = ggarrange(f, fbp, j, jbp, widths = c(5,1,5,1), common.legend = T, align = "h", ncol = 2, nrow = 2)
  
  ggsave(x, width = 300, height = 150, units = "mm", filename = paste0(dirni,"/",sub,".pdf"))
}