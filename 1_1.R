library(dplyr)

#Retrieving current directory
dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)

d1 = read.csv("Data/Judges-Presenter_1Hz_v4.csv")

d1["S_Binary"] = NA

d2 = d1[,c(1:32,185,33:184)]

#Binarize Data
for (i in 1:nrow(d2)) {
   if (!is.na(d2[i,28])) {
      x = d2[i,28:32]
      #print(x)
      x = names(sort(x, decreasing = T)[1])
      x = strsplit(x, split = "_")[[1]][2]
      #print(x)
      if (x == "Neutral") {
         #print(d2[i,28:32])
         d2$S_Binary[i] = 0
      }
      else {
         d2$S_Binary[i] = 1
      }
   }
 print("================================================") 
}

d3 = select(d2, S_Binary, F_Binary)

d4 = filter(d3, !is.na(S_Binary))
d5 = filter(d4, !is.na(F_Binary))

x = 0
for (i in 1:nrow(d5)) {
   if (d5$S_Binary[i] == d5$F_Binary[i]) {
      print(d5[i,])
      x = x+1
   }
}
y = nrow(d5)-x
z = nrow(d3)-(x+y)

barplot(c(x,y,z), col = c("#00CC33","#FF0000","#CCCCCC"))
barplot(c(x,y,z), col = c("forestgreen","firebrick","#CCCCCC"), 
        names.arg = c("Agree","Disagree","NA"), ylim = c(0,5000),
        ylab = "Frequency", main = "Speech vs FACS")
mtext(side=3, line=0.25, at=1.75, adj=0, cex=1, paste0("n = ",(x+y+z)))

barplot(c(x,y), col = c("forestgreen","firebrick"), 
        names.arg = c("Agree","Disagree"), ylim = c(0,2500),
        ylab = "Frequency", main = "Speech vs FACS validity")
mtext(side=3, line=0.25, at=1.2, adj=0, cex=1, paste0("n = ",(x+y)))









