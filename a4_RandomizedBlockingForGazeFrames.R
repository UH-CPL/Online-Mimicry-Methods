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
#library(Scales)
library(ggnewscale)
library(patchwork)
library(gridExtra)
library(rstatix)
library(data.table)
library(R.utils)


#Reading the Data
d = read.csv("Final Reports/Final Reports/Judges-Presenter_1Hz_final.csv")
sub = read.csv("Final Reports/Final Reports/Gaze_data_presentation_for_R_40_subjects.csv")$Participant_ID
sub = sub[-24]
d = filter(d, Participant_ID %in% sub)