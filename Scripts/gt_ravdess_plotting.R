library(ggplot2)
library(hrbrthemes)
library(reshape2)
library(dplyr)

#Retrieving current directory
dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)

source("Scripts/createMatrix_new_facsN.R")
source("Scripts/heatmap_improved.R")
source("Scripts/renameMatrix.R")

d = read.csv("labFACS_on_ravdess.csv")



# Creating and Writing Individual CM's + log
sink("Data/CSV/Co-OccurrenceMatrices/IndividualFiles/log.txt")
s = Sys.time()
print(s)
cat("\n\n")
print("Determinant = Rows")
cat("\n\n\n")
i=1
emocol = 8:14
for (file in files) {
   print(paste0(i," - ",file))
   filedest = paste0(dir,"/Data/convertedCSV/",file)
   d = read.csv(filedest)
   creatematrix3N(d, 7)
   dest = paste0("Data/CSV/Co-OccurrenceMatrices/IndividualFiles/",file)
   #write.csv(mt, dest, row.names = F)
   print(paste0(sum(mt, na.rm = T), " == ", nrow(d)))
   fn = strsplit(file, split = "_")[[1]][1]
   fn = strsplit(fn, split = "-")
   subject = fn[[1]][7]
   emotion = fn[[1]][3]
   cat("\n\n\n")
   i = i+1
}
e = Sys.time()
cat("\n\n\n")
print(e-s)
sink()





n = filter(d, GroundTruth == "Neutral")
c = filter(d, GroundTruth == "Calm")
h = filter(d, GroundTruth == "Happy")
s = filter(d, GroundTruth == "Sad")
an = filter(d, GroundTruth == "Angry")
af = filter(d, GroundTruth == "Afrad")
di = filter(d, GroundTruth == "Disgusted")
su = filter(d, GroundTruth == "Surprised")

plot(n[8:14], main = "Neutral")
plot(c[8:14], main = "Calm")
plot(h[8:14], main = "Happy")
plot(s[8:14], main = "Sad")
plot(an[8:14], main = "Angry")
plot(af[8:14], main = "Afraid")
plot(di[8:14], main = "Disgusted")
plot(su[8:14], main = "Surprised")






pn = # Stacked density plot:
pn = ggplot(data=df3, aes(x=Seconds, y=value, fill=variable)) +
   geom_area() + theme_bw() +
   #scale_x_continuous(limits = c(3,42)) +
   scale_fill_manual(values = c("red","brown","orange","green","blue","yellow","grey"), 
                     labels = c("An","Di","Af","Ha","Sa","Su","Ne")) +
   xlab("Time [s]") + ylab("Probability") + labs(title = "Lab FACS vs Ground Truth", fill = "Emotion")