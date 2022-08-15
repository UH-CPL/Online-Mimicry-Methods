ss = Sys.time()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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
library(randtests)


#Reading the Data
d = read.csv("Data/Presenter-Judges_FSPB+CJ+Gaze(10Hz)+Blink+ALLStats_AllT_1HzMean_v4.csv")
#d1 = read.csv("Data/Presenter-Judges_FSPB+CJ+Gaze(10Hz)+Blink+ALLStats_AllT_1HzMean.csv")
#d2 = read.csv("Data/Presenter-Judges_FSPB+CJ+Gaze+ALLStats_AllT_1HzMean_Clean.csv")
d1 = d
d1["UID"] = 1:nrow(d1)
d1 = d1[,c(191,1, 190, 2, 4, 8, 10, 9, 26)]

write.csv(d1, "Data/GroundTruth_Gaze_DataFile_Testing.csv", row.names = F)

#d1 = filter(d, Participant_ID == "T005")
d2 = d1[which(d1$G_Direction != ""),]
d2 = filter(d2, Treatment %in% c("RB", "DT", "PR"))

sub = read.csv("Final Reports/Final Reports/Gaze_data_presentation_for_R_40_subjects.csv")$Participant_ID
sub = sub[-24]
d2 = filter(d2, Participant_ID %in% sub)

write.csv(d2, "Data/GroundTruth_SelectionPool_Testing.csv", row.names = F)

d5 = data.frame()
for (sub in unique(d2$Participant_ID)) {
  print(sub)
  d3 = filter(d2, Participant_ID == sub)
  for (tr in unique(d3$Treatment)) {
    print(tr)
    d4 = filter(d3, Treatment == tr)
    if (tr == "RB") {
      ct = 10
    }
    if (tr %in% c("DT","PR")) {
      ct = 20
    }
    randompool = 1:nrow(d4)
    result = sample(1:nrow(d4), ct, replace = F)
    d5 = rbind(d5, d4[result,])
  }
  print("")
  print("----------------------------------")
  print("")
}
dim(d5)
write.csv(d5, "Data/Randompool_with_key_2000_Testing.csv", row.names = F)


d6 = d5[,c(1,2,4)]
d6["Aman"] = ""
d6["Fettah"] = ""
write.csv(d6, "Data/Template_2000.csv_Testing", row.names = F)

d7 = d6[order(d6$UID),]
write.csv(d7, "Data/Template_ordered_2000.csv_Testing", row.names = F)

d8 = d7
d8["RandomKey"] = sample(d7$UID)
d8 = d8[order(d8$RandomKey),]
d8 = d8[,c(1,6,2:5)]
write.csv(d8, "Data/UniformRandomDistribution_2000_Testing.csv", row.names = F)

plot(d8$UID, type = "l", xlab = "Number of Frame", ylab = "Unique ID of the Frame", main = "Frames randomized in uniform distribution")
