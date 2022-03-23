ss = Sys.time()

library(tidyverse)
library(dplyr)
library(readxl)

dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)

d = read.csv("Data/Judges-Presenter_2020-07-27.csv")
d1 = filter(d, newPR == 1)

validation_plots_foldername = paste0("Plots/validation_plots_",Sys.Date())
#dir.create(validation_plots_foldername)
barplot_foldername = paste0("Plots/validation_plots/barplots_",Sys.Date())
#dir.create(barplot_foldername)

cn = c("Ne","Sa","Af","An","Ha")
cl = c("grey","blue","orange","red","green")

#individual barplots
for (sub in unique(d1$Participant_ID)) {
   print(paste0("---------------------",sub," Read-------------------"))
   d2 = filter(d1, Participant_ID == sub)
   d3 = filter(d2, !is.na(d2$S_Afraid))
   
   if (nrow(d3) == 0) {
      print(paste0(sub," is a bad subject"))
      pdf(paste0(barplot_foldername,"/",sub,".pdf"), height = 10, width = 7)
      plot(0, type = 'n', xlab = paste0(sub, "'s Speech is corrupt"), ylab = "Mean", ylim = c(0,1), cex.lab = 1.5)
      dev.off()
      next()
      
   }
   
   s = d3[,c(32,31,29,28,30)]
   sn = nrow(s)
   s = colMeans(s, na.rm = T)
   s = as.data.frame(s)
   row.names(s) = cn
   
   f = d3[,c(10,8,6,4,7),]
   fn = nrow(f[!is.na(f$F_Neutral),])
   f = colMeans(f,na.rm = T)
   f = as.data.frame(f)
   row.names(f) = cn
   
   f1 = d2[,c(10,8,6,4,7),]
   f1n = nrow(f1[!is.na(f1$F_Neutral),])
   f1 = colMeans(f1,na.rm = T)
   f1 = as.data.frame(f1)
   row.names(f1) = cn
   
   pdf(paste0(barplot_foldername,"/",sub,".pdf"), height = 8, width = 6)
   
   par(mfrow = c(2,1))
   
   x = cbind(s,f)
   x1 = as.matrix(x)
   colnames(x1) = c(paste0("Speech  n=",sn),paste0("FACS intersection Speech  n=",fn))
   barplot(x1, beside = T, col = cl, main = sub, ylab = "Mean")
   legend("top",cn, fill = cl, horiz = T, box.lty = 0)
   
   y = cbind(s,f1)
   y1 = as.matrix(y)
   colnames(y1) = c(paste0("Speech  n=",sn),paste0("FACS Full PR  n=",f1n))
   barplot(y1, beside = T, col = cl, main = sub, ylab = "Mean")
   legend("top",cn, fill = cl, horiz = T, box.lty = 0)
   
   dev.off()
   print(paste0("---------------",sub," Write Complete---------------"))
   
}


grouped_barplot_foldername = paste0("Plots/validation_plots/barplots_grouped_",Sys.Date())
#dir.create(barplot_foldername)

#grouped barplots
for (sub in unique(d1$Participant_ID)) {
   print(paste0("---------------------",sub," Read-------------------"))
   d2 = filter(d1, Participant_ID == sub)
   d3 = filter(d2, !is.na(d2$S_Afraid))
   
   if (nrow(d3) == 0) {
      print(paste0(sub," is a bad subject"))
      pdf(paste0(grouped_barplot_foldername,"/",sub,".pdf"), height = 8, width = 6)
      plot(0, type = 'n', xlab = paste0(sub, "'s Speech is corrupt"), ylab = "Mean", ylim = c(0,1), cex.lab = 1.5)
      dev.off()
      next()
      
   }
   
   s = d3[,c(32,31,29,28,30)]
   sn = nrow(s)
   s = colMeans(s, na.rm = T)
   s = as.data.frame(s)
   row.names(s) = cn
   
   f = d3[,c(10,8,6,4,7),]
   fn = nrow(f[!is.na(f$F_Neutral),])
   f = colMeans(f,na.rm = T)
   f = as.data.frame(f)
   row.names(f) = cn
   
   f1 = d2[,c(10,8,6,4,7),]
   f1n = nrow(f1[!is.na(f1$F_Neutral),])
   f1 = colMeans(f1,na.rm = T)
   f1 = as.data.frame(f1)
   row.names(f1) = cn
   
   pdf(paste0(grouped_barplot_foldername,"/",sub,".pdf"), height = 8, width = 6)
   
   par(mfrow = c(2,1))
   
   x = cbind(s,f)
   x1 = as.matrix(x)
   colnames(x1) = c(paste0("Speech  n=",sn),paste0("FACS intersection Speech  n=",fn))
   x1t = t(x1)
   barplot(x1t, beside = T, col = c("green","red"), main = sub, ylab = "Mean")
   legend("topright", c("Speech","FACS intersection Speech"), fill = c("green","red"), box.lty = 0)
   
   y = cbind(s,f1)
   y1 = as.matrix(y)
   colnames(y1) = c(paste0("Speech  n=",sn),paste0("FACS Full PR  n=",f1n))
   y1t = t(y1)
   barplot(y1t, beside = T, col = c("green","red"), main = sub, ylab = "Mean")
   legend("topright", c("Speech","FACS Full PR"), fill = c("green","red"), box.lty = 0)
   
   dev.off()
   print(paste0("---------------",sub," Write Complete---------------"))
   
}



all_grouped_barplot_foldername = paste0("Plots/validation_plots/barplots_grouped_all_",Sys.Date())
#dir.create(all_grouped_barplot_foldername)

#grouped barplots
#for (sub in unique(d1$Participant_ID)) {
   #print(paste0("---------------------",sub," Read-------------------"))
   #d2 = filter(d1, Participant_ID == sub)
   d2 = d1
   d3 = filter(d2, !is.na(d2$S_Afraid))
   
   # if (nrow(d3) == 0) {
   #    print(paste0(sub," is a bad subject"))
   #    pdf(paste0(grouped_barplot_foldername,"/",sub,".pdf"), height = 10, width = 7)
   #    plot(0, type = 'n', xlab = paste0(sub, "'s Speech is corrupt"), ylab = "Mean", ylim = c(0,1), cex.lab = 1.5)
   #    dev.off()
   #    next()
   #    
   # }
   
   s = d3[,c(32,31,29,28,30)]
   sn = nrow(s)
   s = colMeans(s, na.rm = T)
   s = as.data.frame(s)
   row.names(s) = cn
   
   f = d3[,c(10,8,6,4,7),]
   fn = nrow(f[!is.na(f$F_Neutral),])
   f = colMeans(f,na.rm = T)
   f = as.data.frame(f)
   row.names(f) = cn
   
   f1 = d2[,c(10,8,6,4,7),]
   f1n = nrow(f1[!is.na(f1$F_Neutral),])
   f1 = colMeans(f1,na.rm = T)
   f1 = as.data.frame(f1)
   row.names(f1) = cn
   
   pdf("Plots/validation_plots_2020-07-28/NSF-OfficeTasks.pdf", height = 7, width = 6)
   
   par(mfrow = c(2,1))
   
   # x = cbind(s,f)
   # x1 = as.matrix(x)
   # colnames(x1) = c(paste0("Speech  n=",sn),paste0("FACS intersection Speech  n=",fn))
   # x1t = t(x1)
   # barplot(x1t, beside = T, col = c("green","red"), main = "NSF - Office Tasks", ylab = "Mean")
   # legend("topright", c("Speech","FACS intersection Speech"), fill = c("green","red"), box.lty = 0)
   
   
   y = cbind(s,f1)
   y1 = as.matrix(y)
   colnames(y1) = c(paste0("Speech  n=",sn),paste0("FACS Full PR  n=",f1n))
   y1t = t(y1)
   barplot(y1t, beside = T, col = c("green","red"), ylab = "Mean", main = "NSF - Office Tasks (RAW)")
   legend("topright", c("Speech","FACS"), fill = c("green","red"), box.lty = 0)
   
   barplot(x4t, beside = T, col = c("green","red"), ylab = "Mean", main = "NSF - Office Tasks (Transformed)")
   #legend("topright", c("Speech","FACS"), fill = c("green","red"), box.lty = 0)
   
   dev.off()
   #print(paste0("---------------",sub," Write Complete---------------"))
   
#}




ee = Sys.time()

print(ee-ss)
