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
d = filter(d, Treatment == "DT")

p = d$F_1
l = d$L_1
c = d$C_1
r = d$R_1

p1 = as.data.frame((table(p)/sum(table(p)))*100)

l1 = as.data.frame((table(l)/sum(table(l)))*100)
dd <- data.frame(l = c("disgusted","surprised"), Freq = c(0,0))
l1 = rbind(l1[1:2,], dd[1,], l1[3:5,], dd[2,])
l1$l = capitalize(l1$l)

c1 = as.data.frame((table(c)/sum(table(c)))*100)
dd = data.frame(c = c("disgusted","surprised"),
                Freq = c(0,0))
c1 = rbind(c1[1:2,], dd[1,], c1[3:5,], dd[2,])
c1$c = capitalize(c1$c)

r1 = as.data.frame((table(r)/sum(table(r)))*100)
dd = data.frame(r = c("disgusted","surprised"),
                Freq = c(0,0))
r1 = rbind(r1[1:2,], dd[1,], r1[3:5,], dd[2,])
r1$r = capitalize(r1$r)

clr = c("orange","red","brown","green","grey", "deepskyblue2","yellow")

pp = ggplot(p1, aes(x = p, y = Freq)) + 
  geom_bar(stat = 'identity', color = clr, fill = clr) +
  xlab("") + 
  ylab("observation (%)") + 
  ggtitle("Participants") + 
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        #plot.background = element_rect(colour = "black", size = 2),
        # panel.background = element_rect(fill = "white"),
        # panel.grid = element_line(colour = "grey")
        )

pl = ggplot(l1, aes(x = l, y = Freq)) + 
  geom_bar(stat = 'identity', color = clr, fill = clr) +
  xlab("") + 
  ylab("") + 
  ggtitle("Left Judge") + 
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        #plot.background = element_rect(colour = "black", size = 2),
        # panel.background = element_rect(fill = "white"),
        # panel.grid = element_line(colour = "grey")
        )

pc = ggplot(c1, aes(x = c, y = Freq)) + 
  geom_bar(stat = 'identity', color = clr, fill = clr) +
  xlab("") + 
  ylab("observation (%)") + 
  ggtitle("Center Judge") + 
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        #plot.background = element_rect(colour = "black", size = 2),
        # panel.background = element_rect(fill = "white"),
        # panel.grid = element_line(colour = "grey")
        )

pr = ggplot(r1, aes(x = r, y = Freq)) + 
  geom_bar(stat = 'identity', color = clr, fill = clr) +
  xlab("") + 
  ylab("") + 
  ggtitle("Right Judge") + 
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        #plot.background = element_rect(colour = "black", size = 2),
        # panel.background = element_rect(fill = "white"),
        # panel.grid = element_line(colour = "grey")
        )


x = ggarrange(pp, pl, pc, pr, nrow = 2, ncol = 2)
pdf("Plots/emotion_barplot_PR_40.pdf", width = 12, height = 9)
plot(x)
dev.off()

ggplot(d1, aes(fill = variable, y = value, x = Participant_ID)) + 
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual('variable', values = c("grey","blue","green","red")) + scale_x_discrete(labels = lbl)
