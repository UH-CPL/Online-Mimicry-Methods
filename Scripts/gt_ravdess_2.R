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

d = read.csv("ravdess-allsubjects-labFACS.csv")
nrow(d)
d = dplyr::filter(d, GroundTruth != "Calm")
nrow(d)
d = dplyr::filter(d, !is.na(F_Angry))
nrow(d)
d = dplyr::filter(d, Seconds >= 0.8 & Seconds >= 2.25)
nrow(d)

dgt = as.factor(d$GroundTruth)
dfacs = as.factor(d$DomE1)
cm = confusionMatrix(dgt, dfacs)


sink("Data/Binary_Predicting_BigPicture.txt")
cat("Lab FACS overall performance to detect {0 = Neutral ; 1 = Emotion}\n\n\n")
RAVDESS = d$GTBinary
Prediction = d$FACSBinary
tab = t(table(Prediction, RAVDESS))
colnames(tab) = c("Neutral","Emotion")
rownames(tab) = c("Neutral","Emotion")
print(tab)
cat("\n")
cmn = confusionMatrix(as.factor(RAVDESS), as.factor(Prediction))
cme = confusionMatrix(as.factor(RAVDESS), as.factor(Prediction), positive = "1")
print(cmn)
cat("\n")
xn = cmn[[4]][5:7]
xn = append(xn, cmn[[4]][3])
xn = round(xn, 3)
names(xn) = c("Precision", "Recall", "F1", "Accuracy")
cat("#######################################\n")
cat("For Neutral\n")
print(xn)
cat("#######################################")
cat("\n\n\n")

print(tab)
cat("\n")
cmn = confusionMatrix(as.factor(RAVDESS), as.factor(Prediction))
cme = confusionMatrix(as.factor(RAVDESS), as.factor(Prediction), positive = "1")
print(cme)
cat("\n")
xe = cme[[4]][5:7]
xe = append(xe, cme[[4]][3])
xe = round(xe, 3)
names(xe) = c("Precision", "Recall", "F1", "Accuracy")
cat("#######################################\n")
cat("For Neutral\n")
print(xe)
cat("#######################################")
cat("\n\n\n")

sink()


dne = dplyr::filter(d, GroundTruth == "Neutral")
daf = dplyr::filter(d, GroundTruth == "Afraid")
dan = dplyr::filter(d, GroundTruth == "Angry")
ddi = dplyr::filter(d, GroundTruth == "Disgusted")
dha = dplyr::filter(d, GroundTruth == "Happy")
dsa = dplyr::filter(d, GroundTruth == "Sad")
dsu = dplyr::filter(d, GroundTruth == "Surprised")

#Neutral
t3 = as.factor(dne$GTBinary)
t4 = as.factor(dne$FACSBinary)
ne = confusionMatrix(t3, t4)

#Afriad
t3 = as.factor(daf$GTBinary)
t4 = as.factor(daf$FACSBinary)
af = confusionMatrix(t3, t4, positive = "1")

#Angry
t3 = as.factor(dan$GTBinary)
t4 = as.factor(dan$FACSBinary)
an = confusionMatrix(t3, t4, positive = "1")

#Disgusted
t3 = as.factor(ddi$GTBinary)
t4 = as.factor(ddi$FACSBinary)
di = confusionMatrix(t3, t4, positive = "1")

#Happy
t3 = as.factor(dha$GTBinary)
t4 = as.factor(dha$FACSBinary)
ha = confusionMatrix(t3, t4, positive = "1")

#Sad
t3 = as.factor(dsa$GTBinary)
t4 = as.factor(dsa$FACSBinary)
sa = confusionMatrix(t3, t4, positive = "1")


#Surprised
t3 = as.factor(dsu$GTBinary)
t4 = as.factor(dsu$FACSBinary)
su = confusionMatrix(t3, t4, positive = "1")



sink("Data/Emotionwise_Full_Stats.txt")
cat("Emotionwise breakdown of Lab FACS performance\n")
cat(" {0 = Neutral, 1 = Emotion} ")
cat("\n\n\n\n\n")

cat("###########################################################\n")
cat(paste0(" Ground Truth = Neutral   ", "n = ",nrow(dne), "\n"))
cat("###########################################################\n")
cat("\n")
print(ne)
cat("\n\n\n\n")

cat("###########################################################\n")
cat(paste0(" Ground Truth = Angry   ", "n = ",nrow(dan), "\n"))
cat("###########################################################\n")
cat("\n")
print(an)
cat("\n\n\n\n")

cat("###########################################################\n")
cat(paste0(" Ground Truth = Afraid   ", "n = ",nrow(daf), "\n"))
cat("###########################################################\n")
cat("\n")
print(af)
cat("\n\n\n\n")

cat("###########################################################\n")
cat(paste0(" Ground Truth = Disgusted   ", "n = ",nrow(ddi), "\n"))
cat("###########################################################\n")
cat("\n")
print(di)
cat("\n\n\n\n")

cat("###########################################################\n")
cat(paste0(" Ground Truth = Happy   ", "n = ",nrow(dha), "\n"))
cat("###########################################################\n")
cat("\n")
print(ha)
cat("\n\n\n\n")

cat("###########################################################\n")
cat(paste0(" Ground Truth = Sad   ", "n = ",nrow(dsa), "\n"))
cat("###########################################################\n")
cat("\n")
print(sa)
cat("\n\n\n\n")

cat("###########################################################\n")
cat(paste0(" Ground Truth = Surprised   ", "n = ",nrow(dsu), "\n"))
cat("###########################################################\n")
cat("\n")
print(su)

sink()








sink("Data/Emotionwise_SelectedStats+KappaStats_v2.txt")
cat("Performance of Lab FACS to detect {0 = Neutral, 1 = Emotion} \n")
cat("Given a particular emotion expressed by the subjects")
cat("\n\n\n\n")

cat("###################################\n")
cat(paste0(" Ground Truth = Neutral   ", "n = ",nrow(dne), "\n"))
cat("###################################\n")
cat("\n")
RAVDESS = dne$GTBinary
Prediction = dne$FACSBinary
tab = t(table(Prediction, RAVDESS))
colnames(tab) = c("Neutral","Emotion")
rownames(tab) = "Neutral"
print(tab)
cat("\n")
print(ne$overall)
cat("\n")
x = ne[[4]][5:7]
x = append(x, ne[[3]][1])
print(round(x, 3))
# cat("\nPrecision = 1\n")
# ner = round(tab[[1]]/(tab[[1]]+tab[[2]]),3)
# cat(paste0("Recall = ", ner,"\n"))
# nef1 = round(2*(ner/(ner+1)), 3)
# cat(paste0("F1 = ",nef1))
cat("\n\n\n")



cat("###################################\n")
cat(paste0(" Ground Truth = Angry   ", "n = ",nrow(dan), "\n"))
cat("###################################\n")
cat("\n")
RAVDESS = dan$GTBinary
Prediction = dan$FACSBinary
tab = t(table(Prediction, RAVDESS))
colnames(tab) = c("Neutral","Emotion")
rownames(tab) = "Emotion"
print(tab)
cat("\n")
print(an$overall)
cat("\n")
x = an[[4]][5:7]
x = append(x, an[[3]][1])
print(round(x, 3))
# cat("\nPrecision = 1\n")
# anr = round(tab[[2]]/(tab[[1]]+tab[[2]]),3)
# cat(paste0("Recall = ", anr,"\n"))
# anf1 = round(2*(anr/(anr+1)), 3)
# cat(paste0("F1 = ",anf1))
cat("\n\n\n")



cat("###################################\n")
cat(paste0(" Ground Truth = Afraid   ", "n = ",nrow(daf), "\n"))
cat("###################################\n")
cat("\n")
RAVDESS = daf$GTBinary
Prediction = daf$FACSBinary
tab = t(table(Prediction, RAVDESS))
colnames(tab) = c("Neutral","Emotion")
rownames(tab) = "Emotion"
print(tab)
cat("\n")
print(af$overall)
cat("\n")
x = af[[4]][5:7]
x = append(x, af[[3]][1])
print(round(x, 3))
# cat("\nPrecision = 1\n")
# afr = round(tab[[2]]/(tab[[1]]+tab[[2]]),3)
# cat(paste0("Recall = ", afr,"\n"))
# aff1 = round(2*(afr/(afr+1)), 3)
# cat(paste0("F1 = ",aff1))
cat("\n\n\n")



cat("###################################\n")
cat(paste0(" Ground Truth = Disgusted   ", "n = ",nrow(ddi), "\n"))
cat("###################################\n")
cat("\n")
RAVDESS = ddi$GTBinary
Prediction = ddi$FACSBinary
tab = t(table(Prediction, RAVDESS))
colnames(tab) = c("Neutral","Emotion")
rownames(tab) = "Emotion"
print(tab)
cat("\n")
print(di$overall)
cat("\n")
x = di[[4]][5:7]
x = append(x, di[[3]][1])
print(round(x, 3))
# cat("\nPrecision = 1\n")
# dir = round(tab[[2]]/(tab[[1]]+tab[[2]]),3)
# cat(paste0("Recall = ", dir,"\n"))
# dif1 = round(2*(dir/(dir+1)), 3)
# cat(paste0("F1 = ",dif1))
cat("\n\n\n")



cat("###################################\n")
cat(paste0(" Ground Truth = Happy   ", "n = ",nrow(dha), "\n"))
cat("###################################\n")
cat("\n")
RAVDESS = dha$GTBinary
Prediction = dha$FACSBinary
tab = t(table(Prediction, RAVDESS))
colnames(tab) = c("Neutral","Emotion")
rownames(tab) = "Emotion"
print(tab)
cat("\n")
print(ha$overall)
cat("\n")
x = ha[[4]][5:7]
x = append(x, ha[[3]][1])
print(round(x, 3))
# cat("\nPrecision = 1\n")
# har = round(tab[[2]]/(tab[[1]]+tab[[2]]),3)
# cat(paste0("Recall = ", har,"\n"))
# haf1 = round(2*(har/(har+1)), 3)
# cat(paste0("F1 = ",haf1))
cat("\n\n\n")


cat("###################################\n")
cat(paste0(" Ground Truth = Sad   ", "n = ",nrow(dsa), "\n"))
cat("###################################\n")
cat("\n")
RAVDESS = dsa$GTBinary
Prediction = dsa$FACSBinary
tab = t(table(Prediction, RAVDESS))
colnames(tab) = c("Neutral","Emotion")
rownames(tab) = "Emotion"
print(tab)
cat("\n")
print(sa$overall)
cat("\n")
x = sa[[4]][5:7]
x = append(x, sa[[3]][1])
print(round(x, 3))
# cat("\nPrecision = 1\n")
# sar = round(tab[[2]]/(tab[[1]]+tab[[2]]),3)
# cat(paste0("Recall = ", sar,"\n"))
# saf1 = round(2*(sar/(sar+1)), 3)
# cat(paste0("F1 = ",saf1))
cat("\n\n\n")



cat("###################################\n")
cat(paste0(" Ground Truth = Surprised   ", "n = ",nrow(dsu), "\n"))
cat("###################################\n")
cat("\n")
RAVDESS = dsu$GTBinary
Prediction = dsu$FACSBinary
tab = t(table(Prediction, RAVDESS))
colnames(tab) = c("Neutral","Emotion")
rownames(tab) = "Emotion"
print(tab)
cat("\n")
print(su$overall)
cat("\n")
x = su[[4]][5:7]
x = append(x, su[[3]][1])
print(round(x, 3))
# cat("\nPrecision = 1\n")
# sur = round(tab[[2]]/(tab[[1]]+tab[[2]]),3)
# cat(paste0("Recall = ", sur,"\n"))
# suf1 = round(2*(sur/(sur+1)), 3)
# cat(paste0("F1 = ",suf1))
cat("\n\n\n")



sink()



