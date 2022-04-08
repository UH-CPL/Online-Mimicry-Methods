library(ggplot2)
library(hrbrthemes)
library(reshape2)
library(dplyr)

#Retrieving current directory
dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)

gt = read.csv("Data/RAV1GT.csv")
gt$Start = as.numeric(gt$Start)
gt$Stop = as.numeric(gt$Stop)

d = read.csv("Data/RAV1_FACS+Gaze2.csv")
d["GroundTruth"] = NA

for (r in 1:nrow(gt)) {
   d1 = gt[r,]
   start = d1$Start
   stop = d1$Stop
   emo = d1$Emotion
   d$GroundTruth[(which(d$Seconds >= start & d$Seconds <= stop))] = emo
}

colnames(d)
df = d[,c(1:10,18:20,28)]
#write.csv(df, "Data/RAV1_Tracked_FACS_Gaze.csv", row.names = F)

df1 = df[,c(2:10,14)]

df2 = melt(df1, 
           id.vars = c("Frame","Seconds","GroundTruth"),
           measure.vars = c(colnames(df1)[3:9]))

df3 = df2[order(df2$Frame),]

df3$value[is.na(df3$value)] = 0
df3$value = as.numeric(df3$value)






# Stacked density plot:
p <- ggplot(data=df3, aes(x=Seconds, y=value, fill=variable)) +
   geom_area() + theme_bw() +
   #scale_x_continuous(limits = c(3,42)) +
   scale_fill_manual(values = c("red","brown","orange","green","blue","yellow","grey"), 
                     labels = c("An","Di","Af","Ha","Sa","Su","Ne")) +
   xlab("Time [s]") + ylab("Probability") + labs(title = "Lab FACS vs Ground Truth", fill = "Emotion")
p

p + geom_segment(aes(x = 3.1, y = 1.025, xend = 6.47, yend = 1.025), size = 5, color = "grey") +
   geom_segment(aes(x = 7.5, y = 1.025, xend = 11.43, yend = 1.025), size = 5, color = "grey") +
   geom_segment(aes(x = 13.27, y = 1.025, xend = 16.47, yend = 1.025), size = 5, color = "green") +
   geom_segment(aes(x = 17.5, y = 1.025, xend = 21.67, yend = 1.025), size = 5, color = "blue") +
   geom_segment(aes(x = 23.27, y = 1.025, xend = 25.57, yend = 1.025), size = 5, color = "red") +
   geom_segment(aes(x = 28.13, y = 1.025, xend = 31.2, yend = 1.025), size = 5, color = "orange") +
   geom_segment(aes(x = 32.8, y = 1.025, xend = 36.6, yend = 1.025), size = 5, color = "brown") +
   geom_segment(aes(x = 38.8, y = 1.025, xend = 41.3, yend = 1.025), size = 5, color = "yellow")






#################################################################################################################################################################






library(ggplot2)
library(hrbrthemes)
library(reshape2)
library(dplyr)

#Retrieving current directory
dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)

gt = read.csv("Data/RAV1_Tracked_GT.csv")
gt$Start = as.numeric(gt$Start)
gt$Stop = as.numeric(gt$Stop)

d = read.csv("Data/RAV1_Tracked_FACS+Gaze+GroundTruth.csv")
d = distinct(d, Frame, .keep_all = T)
d["GroundTruth"] = NA

for (r in 1:nrow(gt)) {
   d1 = gt[r,]
   start = d1$Start
   stop = d1$Stop
   emo = d1$Emotion
   d$GroundTruth[(which(d$Seconds >= start & d$Seconds <= stop))] = emo
}

colnames(d)
df = d[,c(1:10,18:20,28)]
write.csv(df, "Data/RAV1_Tracked_FACS_Gaze.csv", row.names = F)

df1 = df[,c(2:10,14)]

df2 = melt(df1, 
           id.vars = c("Frame","Seconds","GroundTruth"),
           measure.vars = c(colnames(df1)[3:9]))

df3 = df2[order(df2$Frame),]

df3$value[is.na(df3$value)] = 0
df3$value = as.numeric(df3$value)






# Stacked density plot:
p <- ggplot(data=df3, aes(x=Seconds, y=value, fill=variable)) +
   geom_area() + theme_bw() +
   scale_x_continuous(limits = c(3,42)) +
   scale_fill_manual(values = c("red","brown","orange","green","blue","yellow","grey"), 
                     labels = c("An","Di","Af","Ha","Sa","Su","Ne")) +
   xlab("Time [s]") + ylab("Probability") + labs(title = "Lab FACS vs Ground Truth - On Annotated Video", fill = "Emotion")
p

p + geom_segment(aes(x = 3.03, y = 1.025, xend = 6.43, yend = 1.025), size = 5, color = "grey") +
   geom_segment(aes(x = 7.47, y = 1.025, xend = 11.37, yend = 1.025), size = 5, color = "grey") +
   geom_segment(aes(x = 13, y = 1.025, xend = 16.4, yend = 1.025), size = 5, color = "green") +
   geom_segment(aes(x = 17.43, y = 1.025, xend = 21.57, yend = 1.025), size = 5, color = "blue") +
   geom_segment(aes(x = 23.27, y = 1.025, xend = 26.4, yend = 1.025), size = 5, color = "red") +
   geom_segment(aes(x = 28, y = 1.025, xend = 30.63, yend = 1.025), size = 5, color = "orange") +
   geom_segment(aes(x = 32.5, y = 1.025, xend = 36.3, yend = 1.025), size = 5, color = "brown") +
   geom_segment(aes(x = 38.67, y = 1.025, xend = 40.73, yend = 1.025), size = 5, color = "yellow")


# Stacked density plot:
p <- ggplot(data=df3, aes(x=Seconds, y=value, fill=variable)) +
   geom_area(color = c("red","brown","orange","green","blue","yellow","purple"))
p



###########################################################################################
#######################################SPEECH##############################################





gt = read.csv("Data/RAV1GT.csv")
gt$Start = as.numeric(gt$Start)
gt$Stop = as.numeric(gt$Stop)

d = read.csv("Data/RAV1_Sound.csv")
d["GroundTruth"] = NA

for (r in 1:nrow(gt)) {
   d1 = gt[r,]
   start = d1$Start
   stop = d1$Stop
   emo = d1$Emotion
   d$GroundTruth[(which(d$Seconds >= start & d$Seconds <= stop))] = emo
}

colnames(d)
df = d[,c(1:10,18:20,28)]
write.csv(d, "Data/RAV1_Speech+GroundTruth.csv", row.names = F)

df1 = df[,c(2:10,14)]

df2 = melt(d, 
           id.vars = c("Seconds","GroundTruth"),
           measure.vars = c(colnames(d)[3:7]))

df3 = df2[order(df2$Seconds),]

df3$value[is.na(df3$value)] = 0
df3$value = as.numeric(df3$value)






# Stacked density plot:
p <- ggplot(data=df3, aes(x=Seconds, y=value, fill=variable)) +
   geom_area() + theme_bw() +
   scale_x_continuous(limits = c(3,42)) +
   scale_fill_manual(values = c("red","orange","green","blue","grey"), 
                     labels = c("An","Af","Ha","Sa","Ne")) +
   xlab("Time [s]") + ylab("Probability") + labs(title = "Speech vs Ground Truth", fill = "Emotion")
p

p + geom_segment(aes(x = 3.1, y = 1.025, xend = 6.47, yend = 1.025), size = 5, color = "grey") +
   #geom_segment(aes(x = 7.5, y = 1.025, xend = 11.43, yend = 1.025), size = 5, color = "grey") +
   geom_segment(aes(x = 13.27, y = 1.025, xend = 16.47, yend = 1.025), size = 5, color = "green") +
   geom_segment(aes(x = 17.5, y = 1.025, xend = 21.67, yend = 1.025), size = 5, color = "blue") +
   geom_segment(aes(x = 23.27, y = 1.025, xend = 25.57, yend = 1.025), size = 5, color = "red") +
   geom_segment(aes(x = 28.13, y = 1.025, xend = 31.2, yend = 1.025), size = 5, color = "orange")
   #geom_segment(aes(x = 32.8, y = 1.025, xend = 36.6, yend = 1.025), size = 5, color = "brown") +
   #geom_segment(aes(x = 38.8, y = 1.025, xend = 41.3, yend = 1.025), size = 5, color = "yellow")









