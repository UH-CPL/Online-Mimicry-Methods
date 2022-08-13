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
d = read.csv("Data/Blink+Gaze_Summary_TreatmentLevel_v3.csv")
#Treatment Working On
d = filter(d, Treatment == "PR")

#Subject List
sub = read.csv("Final Reports/Final Reports/Gaze_data_presentation_for_R_40_subjects.csv")$Participant_ID
sub = sub[-24]
d = filter(d, Participant_ID %in% sub)

gazcol = c("red","green","blue","grey")

######################## PR ##########################


a1<-as.matrix(d[,c(9, 10, 8, 7, 13)])

a1<-a1[order(a1[,1]),]
lbl = a1[,5]
lbl[which(lbl == "I")] = ""
lbl[which(lbl == "NI")] = "^"
a1 = a1[,1:4]
par(mar=c(3, 5, 3, 3), xpd = T)
barplot(t(a1),beside = FALSE,col=gazcol,ylab="Observations [%]",xlab="",
        names.arg = lbl)
legend("top", inset=c(0, -0.14), legend=c("Right","Center","Left","Closed"), 
       fill=gazcol,horiz=T,bty="n",title="Gaze - PR")
r = recordPlot()


a1<-as.matrix(d[,c(9, 10, 8, 7, 13)])

a1<-a1[order(a1[,2]),]
lbl = a1[,5]
lbl[which(lbl == "I")] = ""
lbl[which(lbl == "NI")] = "^"
a1 = a1[,1:4]
par(mar=c(3, 4, 3, 3), xpd=TRUE)
barplot(t(a1),beside = FALSE,col=gazcol,ylab="Observations [%]",xlab="",
        names.arg = lbl)
# legend("top", inset=c(0, -0.15), legend=c("Closed","Left","Center","Right"), 
#        fill=gazcol,horiz=T,bty="n",title="")
c = recordPlot()




a1<-as.matrix(d[,c(9, 10, 8, 7, 13)])

a1<-a1[order(a1[,3]),]
lbl = a1[,5]
lbl[which(lbl == "I")] = ""
lbl[which(lbl == "NI")] = "^"
a1 = a1[,1:4]
par(mar=c(3, 4, 3, 3), xpd=TRUE)
barplot(t(a1),beside = FALSE,col=gazcol,ylab="Observations [%]",xlab="",
        names.arg = lbl)
# legend("top", inset=c(0, -0.15), legend=c("Closed","Left","Center","Right"), 
#        fill=gazcol,horiz=T,bty="n",title="")
l = recordPlot()




a1<-as.matrix(d[,c(9, 10, 8, 7, 13)])

a1<-a1[order(a1[,4]),]
lbl = a1[,5]
lbl[which(lbl == "I")] = ""
lbl[which(lbl == "NI")] = "^"
a1 = a1[,1:4]
par(mar=c(3, 4, 3, 3), xpd=TRUE)
barplot(t(a1),beside = FALSE,col=gazcol,ylab="Observations [%]",xlab="",
        names.arg = lbl)
# legend("top", inset=c(0, -0.15), legend=c("Closed","Left","Center","Right"), 
#        fill=gazcol,horiz=T,bty="n",title="")
cl = recordPlot()


par(mfrow = c(4,1))



######################## DT ##########################
#Reading the Data
d = read.csv("Data/Blink+Gaze_Summary_TreatmentLevel_v3.csv")
#Treatment Working On
d = filter(d, Treatment == "DT")

#Subject List
sub = read.csv("Final Reports/Final Reports/Gaze_data_presentation_for_R_40_subjects.csv")$Participant_ID
sub = sub[-24]
d = filter(d, Participant_ID %in% sub)

gazcol = c("red","green","blue","grey")

a1<-as.matrix(d[,c(9, 10, 8, 7, 13)])

a1<-a1[order(a1[,1]),]
lbl = a1[,5]
lbl[which(lbl == "I")] = ""
lbl[which(lbl == "NI")] = "^"
a1 = a1[,1:4]
par(mar=c(3, 5, 3, 3), xpd=TRUE)
barplot(t(a1),beside = FALSE,col=gazcol,ylab="Observations [%]",xlab="",
        names.arg = lbl)
legend("top", inset=c(0, -0.14), legend=c("Right","Center","Left","Closed"), 
       fill=gazcol,horiz=T,bty="n",title="Gaze - DT")
r = recordPlot()


a1<-as.matrix(d[,c(9, 10, 8, 7, 13)])

a1<-a1[order(a1[,2]),]
lbl = a1[,5]
lbl[which(lbl == "I")] = ""
lbl[which(lbl == "NI")] = "^"
a1 = a1[,1:4]
par(mar=c(3, 4, 3, 3), xpd=TRUE)
barplot(t(a1),beside = FALSE,col=gazcol,ylab="Observations [%]",xlab="",
        names.arg = lbl)
# legend("top", inset=c(0, -0.15), legend=c("Closed","Left","Center","Right"), 
#        fill=gazcol,horiz=T,bty="n",title="")
c = recordPlot()




a1<-as.matrix(d[,c(9, 10, 8, 7, 13)])

a1<-a1[order(a1[,3]),]
lbl = a1[,5]
lbl[which(lbl == "I")] = ""
lbl[which(lbl == "NI")] = "^"
a1 = a1[,1:4]
par(mar=c(3, 4, 3, 3), xpd=TRUE)
barplot(t(a1),beside = FALSE,col=gazcol,ylab="Observations [%]",xlab="",
        names.arg = lbl)
# legend("top", inset=c(0, -0.15), legend=c("Closed","Left","Center","Right"), 
#        fill=gazcol,horiz=T,bty="n",title="")
l = recordPlot()




a1<-as.matrix(d[,c(9, 10, 8, 7, 13)])

a1<-a1[order(a1[,4]),]
lbl = a1[,5]
lbl[which(lbl == "I")] = ""
lbl[which(lbl == "NI")] = "^"
a1 = a1[,1:4]
par(mar=c(3, 4, 3, 3), xpd=TRUE)
barplot(t(a1),beside = FALSE,col=gazcol,ylab="Observations [%]",xlab="",
        names.arg = lbl)
# legend("top", inset=c(0, -0.15), legend=c("Closed","Left","Center","Right"), 
#        fill=gazcol,horiz=T,bty="n",title="")
cl = recordPlot()



######################## ST ##########################
#Reading the Data
d = read.csv("Data/Blink+Gaze_Summary_TreatmentLevel_v3.csv")
#Treatment Working On
d = filter(d, Treatment == "ST")

#Subject List
sub = read.csv("Final Reports/Final Reports/Gaze_data_presentation_for_R_40_subjects.csv")$Participant_ID
sub = sub[-24]
d = filter(d, Participant_ID %in% sub)

gazcol = c("red","green","blue","grey")

a1<-as.matrix(d[,c(9, 10, 8, 7, 13)])

a1<-a1[order(a1[,1]),]
lbl = a1[,5]
lbl[which(lbl == "I")] = ""
lbl[which(lbl == "NI")] = "^"
a1 = a1[,1:4]
par(mar=c(3, 5, 3, 3), xpd=TRUE)
barplot(t(a1),beside = FALSE,col=gazcol,ylab="Observations [%]",xlab="",
        names.arg = lbl)
legend("top", inset=c(0, -0.14), legend=c("Right","Center","Left","Closed"), 
       fill=gazcol,horiz=T,bty="n",title="Gaze - ST")
r = recordPlot()


a1<-as.matrix(d[,c(9, 10, 8, 7, 13)])

a1<-a1[order(a1[,2]),]
lbl = a1[,5]
lbl[which(lbl == "I")] = ""
lbl[which(lbl == "NI")] = "^"
a1 = a1[,1:4]
par(mar=c(3, 4, 3, 3), xpd=TRUE)
barplot(t(a1),beside = FALSE,col=gazcol,ylab="Observations [%]",xlab="",
        names.arg = lbl)
# legend("top", inset=c(0, -0.15), legend=c("Closed","Left","Center","Right"), 
#        fill=gazcol,horiz=T,bty="n",title="")
c = recordPlot()




a1<-as.matrix(d[,c(9, 10, 8, 7, 13)])

a1<-a1[order(a1[,3]),]
lbl = a1[,5]
lbl[which(lbl == "I")] = ""
lbl[which(lbl == "NI")] = "^"
a1 = a1[,1:4]
par(mar=c(3, 4, 3, 3), xpd=TRUE)
barplot(t(a1),beside = FALSE,col=gazcol,ylab="Observations [%]",xlab="",
        names.arg = lbl)
# legend("top", inset=c(0, -0.15), legend=c("Closed","Left","Center","Right"), 
#        fill=gazcol,horiz=T,bty="n",title="")
l = recordPlot()




a1<-as.matrix(d[,c(9, 10, 8, 7, 13)])

a1<-a1[order(a1[,4]),]
lbl = a1[,5]
lbl[which(lbl == "I")] = ""
lbl[which(lbl == "NI")] = "^"
a1 = a1[,1:4]
par(mar=c(3, 4, 3, 3), xpd=TRUE)
barplot(t(a1),beside = FALSE,col=gazcol,ylab="Observations [%]",xlab="",
        names.arg = lbl)
# legend("top", inset=c(0, -0.15), legend=c("Closed","Left","Center","Right"), 
#        fill=gazcol,horiz=T,bty="n",title="")
cl = recordPlot()




######################## RB ##########################
#Reading the Data
d = read.csv("Data/Blink+Gaze_Summary_TreatmentLevel_v3.csv")
#Treatment Working On
d = filter(d, Treatment == "RB")

#Subject List
sub = read.csv("Final Reports/Final Reports/Gaze_data_presentation_for_R_40_subjects.csv")$Participant_ID
sub = sub[-24]
d = filter(d, Participant_ID %in% sub)

gazcol = c("red","green","blue","grey")

a1<-as.matrix(d[,c(9, 10, 8, 7, 13)])
a1[is.na(a1)] = 0
a1<-a1[order(a1[,1]),]
lbl = a1[,5]
lbl[which(lbl == "I")] = ""
lbl[which(lbl == "NI")] = "^"
a1 = a1[,1:4]
par(mar=c(3, 5, 3, 3), xpd=TRUE)
barplot(t(a1),beside = FALSE,col=gazcol,ylab="Observations [%]",xlab="",
        names.arg = lbl)
legend("top", inset=c(0, -0.14), legend=c("Right","Center","Left","Closed"), 
       fill=gazcol,horiz=T,bty="n",title="Gaze - RB")
r = recordPlot()


a1<-as.matrix(d[,c(9, 10, 8, 7, 13)])
a1[is.na(a1)] = 0
a1<-a1[order(a1[,2]),]
lbl = a1[,5]
lbl[which(lbl == "I")] = ""
lbl[which(lbl == "NI")] = "^"
a1 = a1[,1:4]
par(mar=c(3, 4, 3, 3), xpd=TRUE)
barplot(t(a1),beside = FALSE,col=gazcol,ylab="Observations [%]",xlab="",
        names.arg = lbl)
# legend("top", inset=c(0, -0.15), legend=c("Closed","Left","Center","Right"), 
#        fill=gazcol,horiz=T,bty="n",title="")
c = recordPlot()




a1<-as.matrix(d[,c(9, 10, 8, 7, 13)])
a1[is.na(a1)] = 0
a1<-a1[order(a1[,3]),]
lbl = a1[,5]
lbl[which(lbl == "I")] = ""
lbl[which(lbl == "NI")] = "^"
a1 = a1[,1:4]
par(mar=c(3, 4, 3, 3), xpd=TRUE)
barplot(t(a1),beside = FALSE,col=gazcol,ylab="Observations [%]",xlab="",
        names.arg = lbl)
# legend("top", inset=c(0, -0.15), legend=c("Closed","Left","Center","Right"), 
#        fill=gazcol,horiz=T,bty="n",title="")
l = recordPlot()




a1<-as.matrix(d[,c(9, 10, 8, 7, 13)])
a1[is.na(a1)] = 0
a1<-a1[order(a1[,4]),]
lbl = a1[,5]
lbl[which(lbl == "I")] = ""
lbl[which(lbl == "NI")] = "^"
a1 = a1[,1:4]
par(mar=c(3, 4, 3, 3), xpd=TRUE)
barplot(t(a1),beside = FALSE,col=gazcol,ylab="Observations [%]",xlab="",
        names.arg = lbl)
# legend("top", inset=c(0, -0.15), legend=c("Closed","Left","Center","Right"), 
#        fill=gazcol,horiz=T,bty="n",title="")
cl = recordPlot()





