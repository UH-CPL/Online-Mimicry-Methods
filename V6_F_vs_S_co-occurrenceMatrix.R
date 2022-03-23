logname = paste0("Validation_Normalized_Log_",Sys.Date(),".txt")


ss = Sys.time()

#Libraries Needed
library(plyr)
library(ggplot2)
library(dplyr)
library(miceadds)
library(gtools)
library(ggpubr)
library(gvlma)
library(MASS)
library(reshape2)
library(ggcorrplot)
library(GGally)
library(BBmisc)
library(scales)
library(viridis)
library(Scale)
library(ggnewscale)


#Setting Directory
dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)


#Reading the Data
d = read.csv("Data/Judges-Presenter_2020-07-27.csv")
d = filter(d, newPR == 1)

#Write code to create dir for speech and facs Co-Matrix <--------------------------
dirn = paste0("Data/speech_matrices_",Sys.Date())
#dir.create(dirn)
dirn1 = paste0("Data/facs_matrices_",Sys.Date())
#dir.create(dirn1)
dirn2 = paste0("Data/facs_matrices_normalized_",Sys.Date())
#dir.create(dirn2)

#Write code to create dir for speech and facs Barplot using diagonals <------------
dirn3 = paste0("Plots/speechVSfacs_barplots_pureEmotions_",Sys.Date())
#dir.create(dirn3)
dirn4 = paste0("Plots/speechVSfacs_barplots_pureEmotions_normalised_",Sys.Date())
#dir.create(dirn4)
dirn5 = paste0("Plots/speech_matrix_plots_",Sys.Date())
#dir.create(dirn5)


#Importing Heatman Function
source('Script/heatmap_improved_speech.r')
#Importing Create Matrix FUnction
source('Script/createMatrix_new_speech.r')
#Importing Create Matrix FUnction
source('Script/createMatrix_new_facsN.R')
#Importing Matrix Renaming Function
source('Script/renameMatrix_speech.R')

semocol = c(28:32)
femocol = c(4,6,7,8,10)

cn = c("Ne","Sa","Af","An","Ha")
cl = c("grey","blue","orange","red","green")

flag = 0

sink(paste0("Logs/",logname))

for (sub in unique(d$Participant_ID)) {
   print(sub)
   d1 = filter(d, Participant_ID == sub)
   
   if (nrow(d1[!is.na(d1$S_Afraid),]) > 0) {
      print(paste0("----------Working on ",sub," ----------"))
      print("-----Speech-----")
      emocol = semocol
      creatematrix3(d1, 5)
      print(paste0("Determinant = ",sum(mt, na.rm = T)," -- ",nrow(d1[!is.na(d1$S_Afraid),])," = Number of Frames"))
      write.csv(mt,paste0(dirn,"/",sub,"_speechMatrix.csv"))
      draw_heatmap2(mt, fn, sub, "x")
      ggsave(paste0(dirn5,"/",sub,"speechMatrix.pdf"), px, height = 6, width = 6)
      s = diag(mt)
      sn = sum(mt, na.rm = T)
      s1 = s/sum(s)
      s1 = as.data.frame(s1)
      print("-----Speech Complete-----")
      
      print("-----FACS-----")
      emocol = femocol
      creatematrix3(d1, 5)
      print(paste0("Determinant = ",sum(mt, na.rm = T)," -- ",nrow(d1[!is.na(d1$F_Afraid),])," = Number of Frames"))
      write.csv(mt,paste0(dirn1,"/",sub,"_facsMatrix.csv"))
      print("-----facs Complete-----")
      
      print("-----FACS N-----")
      emocol = femocol
      creatematrix3N(d1, 5)
      print(paste0("Determinant = ",sum(mt, na.rm = T)," -- ",nrow(d1[!is.na(d1$F_Afraid),])," = Number of Frames"))
      write.csv(mt,paste0(dirn2,"/",sub,"_facsMatrix_normalized.csv"))
      f = diag(mt)
      fn = sum(mt, na.rm = T)
      f1 = f/sum(f)
      f1 = as.data.frame(f1)
      print("-----facs N Complete-----")
      
      x = cbind(s1,f1)
      x1 = as.matrix(x)
      colnames(x1) = c("Speech","FACS")
      x1t = t(x1)
      x2t = x1t[,c(5,4,2,1,3)]
      
      if (flag == 0) {
         x3t = x2t
      }
      
      if (flag > 0) {
         x3t = x3t + x2t
      }
      
      pdf(paste0(dirn4,"/",sub,"_speechVSfacs_normalized.pdf"), height = 8, width = 6)
      
      barplot(x2t, beside = T, col = c("green","red"), ylab = "Mean", main = sub)
      legend("topright", c("Speech","FACS"), fill = c("green","red"), box.lty = 0)
      
      dev.off()
      print(paste0("---------------",sub," Plot Complete---------------"))
      
      print("--------------------------")
      print("")
      print("")
      print("")
      flag = flag + 1
   }
   

   
   #creatematrix3(d1,5)
   #Write code to ggsave <--------------------------
   
   #Write code to cal normalised 5x5 FACS matrix <--------------------------
   
   
}


x4t = x3t/flag

pdf(paste0(dirn4,"/AllSubjects","_speechVSfacs_normalized.pdf"), height = 5, width = 7)

barplot(x4t, beside = T, col = c("green","red"), ylab = "Mean", main = "NSF - Office Tasks")
legend("topright", c("Speech","FACS"), fill = c("green","red"), box.lty = 0)

dev.off()
print(paste0("---------------",sub," Plot Complete---------------"))

ee = Sys.time()

ee-ss

sink()