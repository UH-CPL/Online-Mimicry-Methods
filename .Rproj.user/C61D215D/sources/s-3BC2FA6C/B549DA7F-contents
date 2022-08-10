library(tidyverse)
library(ggpubr)


d = read.csv("Data/Presenter-Judges_AllSummary_AllT_1HzMean_Clean_v3.csv")
sub = read.csv("Final Reports/Final Reports/Gaze_data_presentation_for_R_40_subjects.csv")$Participant_ID
sub = sub[-24]
d = filter(d, Participant_ID %in% sub)

d = d[,c(1, 2, 3, 107, 108, 138)]
d = filter(d, Treatment == "RB")

a = d$Age
g = table(d$Gender)

d$Gender[which(d$Gender == 1)] = "Male"
d$Gender[which(d$Gender == 2)] = "Female"

ba = ggplot(d, aes(y=Age, x = "Participant_ID")) + 
  geom_boxplot() + stat_summary(fun = "mean", color = "red", shape = 19) +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  xlab("") + ylab("Age [years]") + 
  scale_y_continuous(breaks = seq(18, 52, 2)) + 
  labs(subtitle = bquote(italic("n = ")~.(length(a)))) +
  annotate("text", x = 1., y = 30, size = 4,
           label = bquote(sigma~" = "~.(round(sd(a), digits = 2))))


bg = ggplot(d, aes(x = Gender)) + geom_bar(color = "black", 
                                            fill = c("lightpink","lightskyblue"),
                                            alpha = 0.7) + 
  #theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  xlab("") + ylab("Frequency") + 
  labs(subtitle = bquote(italic("n = ")~.(nrow(d)))) +
  annotate("text", x = 1, y = 31, label = expression(italic("32"))) +
  annotate("text", x = 2, y = 7, label = expression(italic("8")))

b1 = ggarrange(ba, bg, nrow = 1, ncol = 2)
pdf("Plots/biographic_40.pdf", width = 7, height = 7)
plot(b1)
dev.off()



i = filter(d, Group2 == "I")
bai = ggplot(i, aes(y=Age, x = "Participant_ID")) + 
  geom_boxplot() + stat_summary(fun = "mean", color = "red", shape = 19) +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  xlab("") + ylab("Age [years]") + 
  scale_y_continuous(breaks = seq(18, 52, 2)) + 
  labs(subtitle = bquote(italic("n = ")~.(nrow(i)))) +
  annotate("text", x = 1., y = 30, size = 4,
           label = bquote(sigma~" = "~.(round(sd(i$Age), digits = 2))))


bgi = ggplot(i, aes(x = Gender)) + geom_bar(color = "black", 
                                            fill = c("lightpink","lightskyblue"),
                                            alpha = 0.7) + 
  #theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  xlab("") + ylab("Frequency") + 
  labs(subtitle = bquote(italic("n = ")~.(nrow(i)))) +
  annotate("text", x = 1, y = 17, label = expression(italic("18"))) +
  annotate("text", x = 2, y = 3, label = expression(italic("4")))

bi1 = ggarrange(bai, bgi, nrow = 1, ncol = 2)
pdf("Plots/biographic_I.pdf", width = 7, height = 7)
plot(bi1)
dev.off()



ni = filter(d, Group2 == "NI")
bani = ggplot(ni, aes(y=Age, x = "Participant_ID")) + 
  geom_boxplot() + stat_summary(fun = "mean", color = "red", shape = 19) +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  xlab("") + ylab("Age [years]") + 
  scale_y_continuous(breaks = seq(18, 52, 2)) + 
  labs(subtitle = bquote(italic("n = ")~.(nrow(ni)))) +
  annotate("text", x = 1., y = 30, size = 4,
           label = bquote(sigma~" = "~.(round(sd(ni$Age), digits = 2))))


bgni = ggplot(ni, aes(x = Gender)) + geom_bar(color = "black", 
                                            fill = c("lightpink","lightskyblue"),
                                            alpha = 0.7) + 
  #theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  xlab("") + ylab("Frequency") + 
  labs(subtitle = bquote(italic("n = ")~.(nrow(ni)))) +
  annotate("text", x = 1, y = 13, label = expression(italic("14"))) +
  annotate("text", x = 2, y = 3, label = expression(italic("4")))

bni1 = ggarrange(bani, bgni, nrow = 1, ncol = 2)
pdf("Plots/biographic_NI.pdf", width = 7, height = 7)
plot(bni1)
dev.off()





ai = ggplot(i, aes(y=Age, x = "Participant_ID")) + 
  geom_boxplot() + stat_summary(fun = "mean", color = "red", shape = 19) +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  xlab("") + ylab("Age [years]") + ggtitle("I") +
  scale_y_continuous(breaks = seq(18, 52, 2)) + 
  labs(subtitle = bquote(italic("n = ")~.(nrow(i)))) +
  annotate("text", x = 1, y = 30, size = 4,
           label = bquote(sigma~" = "~.(round(sd(i$Age), digits = 2))))

ani = ggplot(ni, aes(y=Age, x = "Participant_ID")) + 
  geom_boxplot() + stat_summary(fun = "mean", color = "red", shape = 19) +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), 
        plot.title = element_text(hjust = 0.5)) +
  xlab("") + ylab("") + ggtitle("NI") +
  scale_y_continuous(breaks = seq(18, 52, 2)) + 
  labs(subtitle = bquote(italic("n = ")~.(nrow(ni)))) +
  annotate("text", x = 1, y = 30, size = 4,
           label = bquote(sigma~" = "~.(round(sd(ni$Age), digits = 2)))) 

gi = ggplot(i, aes(x = Gender)) + geom_bar(color = "black", 
                                           fill = c("lightpink","lightskyblue"),
                                           alpha = 0.7) + 
  #theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  xlab("") + ylab("Frequency") + 
  # labs(subtitle = bquote(italic("n = ")~.(nrow(i)))) +
  annotate("text", x = 1, y = 17, label = expression(italic("18"))) +
  annotate("text", x = 2, y = 3, label = expression(italic("4")))

gni = ggplot(ni, aes(x = Gender)) + geom_bar(color = "black", 
                                             fill = c("lightpink","lightskyblue"),
                                             alpha = 0.7) + 
  #theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  xlab("") + ylab("") + 
  # labs(subtitle = bquote(italic("n = ")~.(nrow(ni)))) +
  annotate("text", x = 1, y = 13, label = expression(italic("14"))) +
  annotate("text", x = 2, y = 3, label = expression(italic("4")))



pb = ggarrange(ai, ani, gi, gni, nrow = 2, ncol = 2)
pdf("Plots/biographic_groupwise.pdf", width = 8, height = 8)
plot(pb)
dev.off()




