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
j = read.csv("Data/Final_Judges_Combined_NoDuplicates_30fps.csv")


#Importing Heatman Function
source('Script/heatmap_improved.r')
#Importing Create Matrix FUnction
source('Script/createMatrix_new.r')
#Importing Matrix Renaming Function
source('Script/renameMatrix.R')


matrix_plots_foldername = paste0("Plots/matrix_plots_",Sys.Date())
#dir.create(matrix_plots_foldername)
subject_matrix_foldername = paste0("Plots/matrix_plots_",Sys.Date(),"/subject_matrices")
#dir.create(subject_matrix_foldername)

subject_matrix_csv_foldername = paste0("Data/subjectMatrix_",Sys.Date())
#dir.create(subject_matrix_csv_foldername)



#Getting the emotions column
l_emocol = c(40:46) #For Left
c_emocol = c(50:56) #For Center
r_emocol = c(60:66) #For Right
s_emocol = c(4:10) #For Subject

for (sub in unique(d$Participant_ID)) {
   print(sub)
   d1 = filter(d, Participant_ID == sub)
   
   #Subject
   emocol = s_emocol
   creatematrix3(d1, 7)
   write.csv(mt, paste0(subject_matrix_csv_foldername,"/",sub,"-subject.csv"))
   print(paste0("The determinant of the matrix is ",sum(mt, na.rm = T)," --> The number of frames is ",sum(!is.na(d1$F_Angry))))
   draw_heatmap2(mt, "fileName", sub, "subject")
   ggsave(paste0(subject_matrix_foldername,"/",sub,"-subject.pdf"), psubject, height = 5, width = 6)
   
   #Left Judge
   print("Left Judge")
   emocol = l_emocol
   creatematrix3(d1, 7)
   write.csv(mt, paste0(subject_matrix_csv_foldername,"/",sub,"-LeftJudge.csv"))
   print(paste0("The determinant of the matrix is ",sum(mt, na.rm = T)," --> The number of frames is ",sum(!is.na(d1$L_angry))))
   draw_heatmap2(mt, "fileName", "Left Judge", "left")
   ggsave(paste0(subject_matrix_foldername,"/",sub,"-LeftJudge.pdf"), pleft, height = 5, width = 6)
   
   #Center Judge
   print("Center Judge")
   emocol = c_emocol
   creatematrix3(d1, 7)
   write.csv(mt, paste0(subject_matrix_csv_foldername,"/",sub,"-CenterJudge.csv"))
   print(paste0("The determinant of the matrix is ",sum(mt, na.rm = T)," --> The number of frames is ",sum(!is.na(d1$C_angry))))
   draw_heatmap2(mt, "fileName", "Center Judge", "center")
   ggsave(paste0(subject_matrix_foldername,"/",sub,"-CenterJudge.pdf"), pcenter, height = 5, width = 6)
   
   #Right Judge
   print("Right Judge")
   emocol = r_emocol
   creatematrix3(d1, 7)
   write.csv(mt, paste0(subject_matrix_csv_foldername,"/",sub,"-RightJudge.csv"))
   print(paste0("The determinant of the matrix is ",sum(mt, na.rm = T)," --> The number of frames is ",sum(!is.na(d1$R_angry))))
   draw_heatmap2(mt, "fileName", "Right Judge", "right")
   ggsave(paste0(subject_matrix_foldername,"/",sub,"-RightJudge.pdf"), pright, height = 5, width = 6)
   
   #All
   print("Cumulative Plot")
   a = ggarrange(pleft,psubject,pcenter,pright, nrow = 2, ncol = 2)
   ggsave(paste0(subject_matrix_foldername,"/",sub,"-PR.pdf"), a, height = 5, width = 6)

   print("----------------------------------------------")
}

#Creating Co-Occurance matrix for the subject
creatematrix3(j, 7)

#Log for checksums
print(paste0("The determinant of the matrix is ",sum(mt, na.rm = T)," --> The number of frames is ",sum(!is.na(j1$L_angry))))

draw_heatmap2(mt, "fileName", "Right Judge", "right")
ggsave(paste0(matrix_plots_foldername,"/RightJ.pdf"), pright, height = 5, width = 6)

a = ggarrange(pleft,pall,pcenter,pright, nrow = 2, ncol = 2)
ggsave(paste0(matrix_plots_foldername,"/PRMatrix.pdf"), a, height = 5, width = 6)