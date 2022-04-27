library(ggplot2)
library(hrbrthemes)
library(reshape2)
library(dplyr)
library(tidyverse)
library(caret)
library(e1071)

#Retrieving current directory
dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)

d = read.csv("C:/Users/cpladmin/Desktop/MS_Face/Data/CSV/07-07-2021_Speech.csv")
d = filter(d, Emotion %in% c("Neutral","Happy","Sad","Afraid","Angry","Disgusted","Surprised"))


df = filter(d, (GTBinary == 1 & Seconds > 0.5 & Seconds < 2.5) | GTBinary == 0)

d10 = read.csv("RAVDESS_labfacs_10Hz_v2.csv")
d10 = filter(d10, GroundTruth != "Calm")

d10f = filter(d10, (GTBinary == 1 & Seconds_10Hz > 0.5 & Seconds_10Hz < 2.5) | GTBinary == 0)

d1 = read.csv("RAVDESS_labfacs_1Hz_v2.csv")
d1 = filter(d1, GroundTruth != "Calm")

d1f = read.csv("RAVDESS_labfacs_1Hz_filtered_v2.csv")
d1f = filter(d1f, GroundTruth != "Calm")





sink("C:/Users/cpladmin/Desktop/MS_Face/Data/Logs/Speech_Neutral.txt")
cat("Performance of Lab Speech Algorithm to detect {0 = Neutral} \n\n\n")


RAVDESS = d$GT_Binary
Prediction = d$MS1_Binary

tab = t(table(Prediction, RAVDESS))
colnames(tab) = c("Neutral","Emotion")
rownames(tab) = c("Neutral","Emotion")
print(tab)
cat("\n")
cmn = confusionMatrix(as.factor(RAVDESS), as.factor(Prediction))
cme = confusionMatrix(as.factor(RAVDESS), as.factor(Prediction), positive = "0")
print(cmn)
#cat("\n")
xn = cmn[[4]][5:7]
xn = append(xn, cmn[[4]][3])
xn = round(xn, 3)
names(xn) = c("Precision", "Recall", "F1", "Accuracy")
cat("#######################################\n")
cat("For Neutral\n")
print(xn)
cat("#######################################")
#cat("\n\n\n\n\n")
sink()



sink("C:/Users/cpladmin/Desktop/MS_Face/Data/Logs/Speech_Emotion.txt")
cat("Performance of Lab Speech Algorithm to detect {1 = Emotion} \n\n\n")
print(tab)
cat("\n")
cmn = confusionMatrix(as.factor(RAVDESS), as.factor(Prediction))
cme = confusionMatrix(as.factor(RAVDESS), as.factor(Prediction), positive = "1")
print(cme)
#cat("\n")
xe = cme[[4]][5:7]
xe = append(xe, cme[[4]][3])
xe = round(xe, 3)
names(xe) = c("Precision", "Recall", "F1", "Accuracy")
cat("#######################################\n")
cat("For Emotion\n")
print(xe)
cat("#######################################")
sink()







sink("Data/Performance_labFACS_30Hz_filtered.txt")
cat("Performance of Lab FACS @ 30Hz Filtered to detect {0 = Neutral, 1 = Emotion} \n\n\n")


RAVDESS = df$GTBinary
Prediction = df$FACSBinary

tab = t(table(Prediction, RAVDESS))
colnames(tab) = c("Neutral","Emotion")
rownames(tab) = c("Neutral","Emotion")
print(tab)
cat("\n")
cmn = confusionMatrix(as.factor(RAVDESS), as.factor(Prediction))
cme = confusionMatrix(as.factor(RAVDESS), as.factor(Prediction), positive = "0")
print(cmn)
#cat("\n")
xn = cmn[[4]][5:7]
xn = append(xn, cmn[[4]][3])
xn = round(xn, 3)
names(xn) = c("Precision", "Recall", "F1", "Accuracy")
cat("#######################################\n")
cat("For Neutral\n")
print(xn)
cat("#######################################")
cat("\n\n\n\n\n")

print(tab)
cat("\n")
cmn = confusionMatrix(as.factor(RAVDESS), as.factor(Prediction))
cme = confusionMatrix(as.factor(RAVDESS), as.factor(Prediction), positive = "1")
print(cme)
#cat("\n")
xe = cme[[4]][5:7]
xe = append(xe, cme[[4]][3])
xe = round(xe, 3)
names(xe) = c("Precision", "Recall", "F1", "Accuracy")
cat("#######################################\n")
cat("For Emotion\n")
print(xe)
cat("#######################################")
cat("\n\n\n")

sink()










sink("Data/Performance_labFACS_10Hz.txt")
cat("Performance of Lab FACS @ 10Hz to detect {0 = Neutral, 1 = Emotion} \n\n\n")


RAVDESS = d10$GTBinary
Prediction = d10$FACSBinary

tab = t(table(Prediction, RAVDESS))
colnames(tab) = c("Neutral","Emotion")
rownames(tab) = c("Neutral","Emotion")
print(tab)
cat("\n")
cmn = confusionMatrix(as.factor(RAVDESS), as.factor(Prediction))
cme = confusionMatrix(as.factor(RAVDESS), as.factor(Prediction), positive = "0")
print(cmn)
#cat("\n")
xn = cmn[[4]][5:7]
xn = append(xn, cmn[[4]][3])
xn = round(xn, 3)
names(xn) = c("Precision", "Recall", "F1", "Accuracy")
cat("#######################################\n")
cat("For Neutral\n")
print(xn)
cat("#######################################")
cat("\n\n\n\n\n")

print(tab)
cat("\n")
cmn = confusionMatrix(as.factor(RAVDESS), as.factor(Prediction))
cme = confusionMatrix(as.factor(RAVDESS), as.factor(Prediction), positive = "1")
print(cme)
#cat("\n")
xe = cme[[4]][5:7]
xe = append(xe, cme[[4]][3])
xe = round(xe, 3)
names(xe) = c("Precision", "Recall", "F1", "Accuracy")
cat("#######################################\n")
cat("For Emotion\n")
print(xe)
cat("#######################################")
cat("\n\n\n")

sink()






sink("Data/Performance_labFACS_10Hz_filtered.txt")
cat("Performance of Lab FACS @ 10Hz Filtered to detect {0 = Neutral, 1 = Emotion} \n\n\n")


RAVDESS = d10f$GTBinary
Prediction = d10f$FACSBinary

tab = t(table(Prediction, RAVDESS))
colnames(tab) = c("Neutral","Emotion")
rownames(tab) = c("Neutral","Emotion")
print(tab)
cat("\n")
cmn = confusionMatrix(as.factor(RAVDESS), as.factor(Prediction))
cme = confusionMatrix(as.factor(RAVDESS), as.factor(Prediction), positive = "0")
print(cmn)
#cat("\n")
xn = cmn[[4]][5:7]
xn = append(xn, cmn[[4]][3])
xn = round(xn, 3)
names(xn) = c("Precision", "Recall", "F1", "Accuracy")
cat("#######################################\n")
cat("For Neutral\n")
print(xn)
cat("#######################################")
cat("\n\n\n\n\n")

print(tab)
cat("\n")
cmn = confusionMatrix(as.factor(RAVDESS), as.factor(Prediction))
cme = confusionMatrix(as.factor(RAVDESS), as.factor(Prediction), positive = "1")
print(cme)
#cat("\n")
xe = cme[[4]][5:7]
xe = append(xe, cme[[4]][3])
xe = round(xe, 3)
names(xe) = c("Precision", "Recall", "F1", "Accuracy")
cat("#######################################\n")
cat("For Emotion\n")
print(xe)
cat("#######################################")
cat("\n\n\n")

sink()










sink("Data/Performance_labFACS_1Hz.txt")
cat("Performance of Lab FACS @ 1Hz to detect {0 = Neutral, 1 = Emotion} \n\n\n")


RAVDESS = d1$GTBinary
Prediction = d1$FACSBinary

tab = t(table(Prediction, RAVDESS))
colnames(tab) = c("Neutral","Emotion")
rownames(tab) = c("Neutral","Emotion")
print(tab)
cat("\n")
cmn = confusionMatrix(as.factor(RAVDESS), as.factor(Prediction))
cme = confusionMatrix(as.factor(RAVDESS), as.factor(Prediction), positive = "0")
print(cmn)
#cat("\n")
xn = cmn[[4]][5:7]
xn = append(xn, cmn[[4]][3])
xn = round(xn, 3)
names(xn) = c("Precision", "Recall", "F1", "Accuracy")
cat("#######################################\n")
cat("For Neutral\n")
print(xn)
cat("#######################################")
cat("\n\n\n\n\n")

print(tab)
cat("\n")
cmn = confusionMatrix(as.factor(RAVDESS), as.factor(Prediction))
cme = confusionMatrix(as.factor(RAVDESS), as.factor(Prediction), positive = "1")
print(cme)
#cat("\n")
xe = cme[[4]][5:7]
xe = append(xe, cme[[4]][3])
xe = round(xe, 3)
names(xe) = c("Precision", "Recall", "F1", "Accuracy")
cat("#######################################\n")
cat("For Emotion\n")
print(xe)
cat("#######################################")
cat("\n\n\n")

sink()






sink("Data/Performance_labFACS_1Hz_filtered.txt")
cat("Performance of Lab FACS @ 1Hz Filtered to detect {0 = Neutral, 1 = Emotion} \n\n\n")


RAVDESS = d1f$GTBinary
Prediction = d1f$FACSBinary

tab = t(table(Prediction, RAVDESS))
colnames(tab) = c("Neutral","Emotion")
rownames(tab) = c("Neutral","Emotion")
print(tab)
cat("\n")
cmn = confusionMatrix(as.factor(RAVDESS), as.factor(Prediction))
cme = confusionMatrix(as.factor(RAVDESS), as.factor(Prediction), positive = "0")
print(cmn)
#cat("\n")
xn = cmn[[4]][5:7]
xn = append(xn, cmn[[4]][3])
xn = round(xn, 3)
names(xn) = c("Precision", "Recall", "F1", "Accuracy")
cat("#######################################\n")
cat("For Neutral\n")
print(xn)
cat("#######################################")
cat("\n\n\n\n\n")

print(tab)
cat("\n")
cmn = confusionMatrix(as.factor(RAVDESS), as.factor(Prediction))
cme = confusionMatrix(as.factor(RAVDESS), as.factor(Prediction), positive = "1")
print(cme)
#cat("\n")
xe = cme[[4]][5:7]
xe = append(xe, cme[[4]][3])
xe = round(xe, 3)
names(xe) = c("Precision", "Recall", "F1", "Accuracy")
cat("#######################################\n")
cat("For Emotion\n")
print(xe)
cat("#######################################")
cat("\n\n\n")

sink()




