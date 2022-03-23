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





#Getting the emotions column
#emocol = c(5:11) #For Left
#emocol = c(15:21) #For Center
emocol = c(25:31) #For Right


#Creating Co-Occurance matrix for the subject
creatematrix3(j, 7)

#Log for checksums
print(paste0("The determinant of the matrix is ",sum(mt, na.rm = T)," --> The number of frames is ",sum(!is.na(j1$L_angry))))
   
draw_heatmap2(mt, "fileName", "Right Judge", "right")
ggsave(paste0(matrix_plots_foldername,"/RightJ.pdf"), pright, height = 5, width = 6)





emocol = c(4:10) #For Subject

d1 = filter(d, newPR == 1)

#Creating Co-Occurance matrix for the subject
creatematrix3(d1, 7)

#Log for checksums
print(paste0("The determinant of the matrix is ",sum(mt, na.rm = T)," --> The number of frames is ",sum(!is.na(j1$L_angry))))

draw_heatmap2(mt, "fileName", "All subject's PR", "all")
ggsave(paste0(matrix_plots_foldername,"/AllSubjectPR.pdf"), pall, height = 5, width = 6)


