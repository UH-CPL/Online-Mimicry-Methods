ss = Sys.time()

#sink(paste0("Logs/Final_Dataset_CheckLog_",Sys.Date(),".txt"))

library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(plotly)
library(BBmisc)
library(vioplot)
library(beanplot)


#Retrieving current directory
dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)

d = read.csv("Data/Judges-Presenter_1Hz_v3.csv")
d["SameEmo"] = NA
d["N_N"] = NA
d["non-N_non-N"] = NA
d["N_non-N"] = NA
d["non-N_N"] = NA
d["emo_locking_Time"] = NA
d["ELT_non-N_non-N"] = NA
d["ELT_N_N"] = NA
d["ELT_non-N_N"] = NA
d["ELT_N_non-N"] = NA

badSubjects = c("T011", "T032", "T035", "T082", "T091", "T092", "T093", "T096", "T121", "T122", "T151", "T157", "T178")
totalSubjects = unique(d$Participant_ID)
goodSubjects = setdiff(totalSubjects, badSubjects)

#sink(paste0("Logs/BinaryCoupling_10Hz_",Sys.Date(),".txt"))

for (i in 1:nrow(d)) {
   
   if (!is.na(d$J_Binary[i]) && !is.na(d$F_Binary[i])) {
      
      print(d[i,166:165])
      
      if (d$J_Binary[i] == 1 && d$F_Binary[i] == 1) {
         d$`non-N_non-N`[i] = 1
         d$SameEmo[i] = 1
         print("11")
         cat("\n\n\n\n")
      }
      
      if (d$J_Binary[i] == 0 && d$F_Binary[i] == 0) {
         d$SameEmo[i] = 1
         d$N_N[i] = 1
         print("00")
         cat("\n\n\n\n")
      }
      
      if (d$J_Binary[i] == 1 && d$F_Binary[i] == 0) {
         d$`non-N_N`[i] = 1
         print("10")
         cat("\n\n\n\n")
      }
      
      if (d$J_Binary[i] == 0 && d$F_Binary[i] == 1) {
         d$`N_non-N`[i] = 1
         print("01")
         cat("\n\n\n\n")
      }
      
   }
   
}
#sink()

#ELT sameEmo
s = 0
c = 0
d$emo_locking_Time = NA
for ( i in 1:(nrow(d)-1)) {
   #print(i)
   
   if (!is.na(d$SameEmo[i])) {
      s = s + 1
      c = c+1
      if (s == 1) {
         print(i)
         ss = i
         print("---")
         print(ss)
         #print(d$SameEmo[i])
      }
      if (is.na(d$SameEmo[i+1])) {
         d$emo_locking_Time[ss:i] = c
         print(i)
         print(ss)
         print(c)
         print("----------------")
         cat("\n\n\n")
      }
   }
   
   if (is.na(d$SameEmo[i])) {
      c = 0
      s = 0
   }
   
}



#ELT non-N_non-N
s = 0
c = 0
for ( i in 1:(nrow(d)-1)) {
   #print(i)
   
   if (!is.na(d$`non-N_non-N`[i])) {
      s = s + 1
      c = c+1
      if (s == 1) {
         print(i)
         ss = i
         print("---")
         print(ss)
         #print(d$SameEmo[i])
      }
      if (is.na(d$`non-N_non-N`[i+1])) {
         d$`ELT_non-N_non-N`[ss:i] = c
         print(i)
         print(ss)
         print(c)
         print("----------------")
         cat("\n\n\n")
      }
   }
   
   if (is.na(d$`non-N_non-N`[i])) {
      c = 0
      s = 0
   }
   
}




# N_N
s = 0
c = 0
for ( i in 1:(nrow(d)-1)) {
   #print(i)
   
   if (!is.na(d$N_N[i])) {
      s = s + 1
      c = c+1
      if (s == 1) {
         print(i)
         ss = i
         print("---")
         print(ss)
         #print(d$SameEmo[i])
      }
      if (is.na(d$N_N[i+1])) {
         d$ELT_N_N[ss:i] = c
         print(i)
         print(ss)
         print(c)
         print("----------------")
         cat("\n\n\n")
      }
   }
   
   if (is.na(d$N_N[i])) {
      c = 0
      s = 0
   }
   
}




# non-N_N
s = 0
c = 0
for ( i in 1:(nrow(d)-1)) {
   #print(i)
   
   if (!is.na(d$`non-N_N`[i])) {
      s = s + 1
      c = c+1
      if (s == 1) {
         print(i)
         ss = i
         print("---")
         print(ss)
         #print(d$SameEmo[i])
      }
      if (is.na(d$`non-N_N`[i+1])) {
         d$`ELT_non-N_N`[ss:i] = c
         print(i)
         print(ss)
         print(c)
         print("----------------")
         cat("\n\n\n")
      }
   }
   
   if (is.na(d$`non-N_N`[i])) {
      c = 0
      s = 0
   }
   
}






# N_non-N
s = 0
c = 0
for ( i in 1:(nrow(d)-1)) {
   #print(i)
   
   if (!is.na(d$`N_non-N`[i])) {
      s = s + 1
      c = c+1
      if (s == 1) {
         print(i)
         ss = i
         print("---")
         print(ss)
         #print(d$SameEmo[i])
      }
      if (is.na(d$`N_non-N`[i+1])) {
         d$`ELT_N_non-N`[ss:i] = c
         print(i)
         print(ss)
         print(c)
         print("----------------")
         cat("\n\n\n")
      }
   }
   
   if (is.na(d$`N_non-N`[i])) {
      c = 0
      s = 0
   }
   
}


#write.csv(d, "Data/10hz_test.csv", row.names = F)








d["Total locking observations"] = NA
d["Total valid observations"] = NA
d["Locking per valid observations"] = NA
d["LPVO_non-N_non-N"] = NA
d["LPVO_N_N"] = NA
d["LPVO_N_non-N"] = NA
d["LPVO_non-N_N"] = NA


d3 = as.data.frame(matrix(nrow = 0, ncol = ncol(d)))
colnames(d3) = colnames(d)
#ELT same emo
for (sub in unique(d$Participant_ID)) {
   print(sub)
   d1 = filter(d, Participant_ID == sub)
   
   totl = sum(d1$SameEmo, na.rm = T)
   d2 = filter(d1, is.na(d1$F_Angry) == F)
   
   toto = dim(d2)[1]
   tnonN_nonN = sum(d1$`non-N_non-N`, na.rm = T)
   tN_N = sum(d1$N_N, na.rm = T)
   tnonN_N = sum(d1$`non-N_N`, na.rm = T)
   tN_nonN = sum(d1$`N_non-N`, na.rm = T)
   
   lo = totl/toto
   
   d1$`Total locking observations` = totl
   d1$`Total valid observations` = toto
   d1$`Locking per valid observations` = lo
   
   d1$`LPVO_non-N_non-N` = tnonN_nonN/toto
   d1$LPVO_N_N = tN_N/toto
   d1$`LPVO_N_non-N` = tN_nonN/toto
   d1$`LPVO_non-N_N` = tnonN_N/toto
   
   d3 = rbind(d3,d1)
   
}





#write.csv(d3, "Data/Judges-Presenter_10Hz_v4.csv", row.names = F)





d3 = read.csv("Data/Judges-Presenter_1Hz_v4.csv")

badSubjects = c("T011", "T032", "T035", "T082", "T091", "T092", "T093", "T096", "T121", "T122", "T151", "T157", "T178")
totalSubjects = unique(d3$Participant_ID)
goodSubjects = setdiff(totalSubjects, badSubjects)

d4 = as.data.frame(matrix(nrow = 0, ncol = 4))
colnames(d4) = c("Sub","Grp","Obs#","nonon","nn","nonn","nnon")

for ( sub in goodSubjects ) {
   print(sub)
   d5 = filter(d3, Participant_ID == sub)
   d6 = d5[1,c(1,34,178,181:184)]
   d4 = rbind(d4,d6)
}



subj = "T019"
lockn = 15
colsneed = c(1,2,3,4:10,36,40:46,50:56,60:66,165:166,169:172)
d8 = filter(d3, Participant_ID == subj)
d9 = d8
#d9 = filter(d8, emo_locking_Time == lockn)
d9 = d9[,colsneed]
write.csv(d9, "testfigT019.csv", row.names = F)




nonnon = d$`non-N_non-N`
nonnon = rle(nonnon)$lengths - 1
nonnon1 = nonnon[which(nonnon > 0)]

nn = d$N_N
nn = rle(nn)$lengths - 1
nn1 = nn[which(nn > 0)]

nonn = d$`non-N_N`
nonn = rle(nonn)$lengths - 1
nonn1 = nonn[which(nonn > 0)]

nnon = d$`N_non-N`
nnon = rle(nnon)$lengths - 1
nnon1 = nnon[which(nnon > 0)]


beanplot(nonnon1, nn1, yaxt = "n",
         ylab = "Time [s]", col = "light blue", 
         names = c("Non-Non", "N-N"))
axis(side = 2, at = c(1:25), las=1)
xm = round(mean(pr$pppp),4)
text(0.55, 0.0023, paste0("?? = ",xm))
text(0.875 ,max(pr$pppp), round(max(pr$pppp),4))
nonnonb = recordPlot()


hist(nonnon1, # histogram
     col = "peachpuff", # column color
     border = "black",
     prob = TRUE, # show densities instead of frequencies
     #xlim = c(36,38.5),
     xlab = "Time [s]",
     breaks = 15,
     ylab = "pdf",
     las = 1,
     main = "NonN - NonN Distribution",
     ylim = c(0.0,0.7), xaxt = "n")
axis(side = 1, at = c(1:17))

lines(density(nonnon1, na.rm = T, ), # density plot
      lwd = 2, # thickness of line
      col = "chocolate3")

lines(c(mean(nonnon1),mean(nonnon1)),
      c(0,0.09),
      lwd = 3,
      col = "steelblue",
      lty = 2)
mn = round(mean(nonnon1, na.rm = T),2)
text(mean(nonnon1, na.rm = T), 0.11, paste0("?? = ",mn))

lines(c(max(nonnon1, na.rm = T), max(nonnon1, na.rm = T)),
      c(0, 0.09),
      lwd = 2,
      col = "steelblue",
      lty = 2)
text(max(nonnon1, na.rm = T), 0.11, round(max(nonnon1, na.rm = T),2))
print(sd(nonnon1))
text(10, 0.035, expression(paste(sigma, " = 2.67")))
nonnonh = recordPlot()
#dev.off()
      