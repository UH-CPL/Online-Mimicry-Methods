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
library(Scale)
library(ggnewscale)
library(patchwork)
library(gridExtra)
library(AICcmodavg)
library(broom)
library(tidyverse)
library(ggstatsplot)
library(palmerpenguins)
library(rstantools)
library(PMCMRplus)

sub = read.csv("Final Reports/Final Reports/Gaze_data_presentation_for_R_40_subjects.csv")
sub = sub$Participant_ID

#Reading the Data
d = read.csv("Data/Blink+Gaze_Summary_TreatmentLevel_v3.csv")
d$Treatment[is.na(d$Treatment)] = "NA"
d = filter(d, Participant_ID %in% sub)
# d$Group2[which(d$Group %in% c("CH","BH"))] = "I"
# d$Group2[which(d$Group %in% c("CL","BL"))] = "NI"


for (grp in unique(d$Group)) {
  cat(paste0(grp,"\n\n"))
  d1 = filter(d, Group == grp)
  assign(grp, d1$BlinkRate)
}
length(CH) <- length(BH) <- length(CL) <- length(BL) <- max(c(length(CH), length(BH), length(CL), length(BL)))
bb1 = as.data.frame(cbind(CH,BH,BL,CL))
summary(bb1)
summary.aov(bb1)



achcl = aov(CH ~ CL , data = b1)
achbl = aov(CH ~ BL , data = b1)
achbh = aov(CH ~ BH , data = b1)
abh = aov(CH ~ CL , data = b1)
achcl = aov(CH ~ CL , data = b1)
achcl = aov(CH ~ CL , data = b1)
achcl = aov(CH ~ CL , data = b1)


for (grp in unique(d$Group2)) {
  cat(paste0(grp,"\n\n"))
  d1 = filter(d, Group2 == grp)
  assign(grp, d1$BlinkRate)
}

length(I) <- length(NI) <- max(c(length(I), length(NI)))
b2 = as.data.frame(cbind(I, NI))
bb2 = aov(NI ~ I , data = b2)
summary(bb2)
summary.aov(bb2)


for (grp in unique(d$Group2)) {
  cat(paste0(grp,"\n\n"))
  d2 = filter(d, Group2 == grp)
  assign(grp, d2$BlinkRate)
}
boxplot(I, NI)






x = d[,c("Treatment","BlinkRate")]
px = ggplot(x, aes(x = Group, y = BlinkRate)) + geom_boxplot(notch = T) + stat_summary(fun = mean, geom = "point", size = 3) + labs(x = "")


p <- ggbetweenstats(
  data = x,
  x = Treatment,
  y = BlinkRate
)

p1 <- p + 
  # Add labels and title
  labs(
    x = "",
    #y = "Bill Length",
    #title = "Distribution of bill length across penguins species"
  ) + 
  # Customizations
  theme(
    # This is the new default font in the plot
    text = element_text(family = "Roboto", size = 8, color = "black"),
    plot.title = element_text(
      family = "Lobster Two", 
      size = 20,
      face = "bold",
      color = "#2a475e"
    ),
    # Statistical annotations below the main title
    plot.subtitle = element_text(
      family = "Roboto", 
      size = 15, 
      face = "bold",
      color="#1b2838"
    ),
    plot.title.position = "plot", # slightly different from default
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 12)
  )


p2 <- p1  +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_line(colour = "grey50"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4")
  )

ggsave(
  filename = "Plots/TreatmentWise_BlinkRate/TreatmentWise_BlinkRate_v2.png",
  plot = p2,
  width = 8,
  height = 8,
  device = "png"
)









xy = d[,c("Treatment","Group2","BlinkRate")]
#x1 = melt(x1)
p1 = ggplot(x1, aes(x = Group, y = BlinkRate)) + geom_boxplot(notch = T) + stat_summary(fun = mean, geom = "point", size = 3) + labs(x = "")

na = filter(xy, Treatment == "NA")
rb = filter(xy, Treatment == "RB")
st = filter(xy, Treatment == "ST")
pr = filter(xy, Treatment == "PR")
pm = filter(xy, Treatment == "PM")
dt = filter(xy, Treatment == "DT")

q = dt

# pxy <- ggwithinstats(
#   data = q,
#   x = Group2,
#   y = BlinkRate
# )
px <- ggbetweenstats(
  data = q,
  x = Group2,
  y = BlinkRate
)

px1 <- px + 
  # Add labels and title
  labs(
    x = "DT",
    #y = "Bill Length",
    #title = "Distribution of bill length across penguins species"
  ) + 
  # Customizations
  theme(
    # This is the new default font in the plot
    text = element_text(family = "Roboto", size = 8, color = "black"),
    plot.title = element_text(
      family = "Lobster Two", 
      size = 20,
      face = "bold",
      color = "#2a475e"
    ),
    # Statistical annotations below the main title
    plot.subtitle = element_text(
      family = "Roboto", 
      size = 15, 
      face = "bold",
      color="#1b2838"
    ),
    plot.title.position = "plot", # slightly different from default
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 12)
  )


px2 <- px1  +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_line(colour = "grey50"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4")
  )

ggsave(
  filename = "Plots/TreatmentWise_BlinkRate/DT_BlinkRate_Welch.png",
  plot = px2,
  width = 8,
  height = 8,
  device = "png"
)
xy = d[,c("Treatment","Group2","BlinkRate")]
#x1 = melt(x1)
p1 = ggplot(x1, aes(x = Group, y = BlinkRate)) + geom_boxplot(notch = T) + stat_summary(fun = mean, geom = "point", size = 3) + labs(x = "")

na = filter(xy, Treatment == "NA")
rb = filter(xy, Treatment == "RB")
st = filter(xy, Treatment == "ST")
pr = filter(xy, Treatment == "PR")
pm = filter(xy, Treatment == "PM")
dt = filter(xy, Treatment == "DT")

q = dt

# pxy <- ggwithinstats(
#   data = q,
#   x = Group2,
#   y = BlinkRate
# )
px <- ggbetweenstats(
  data = q,
  x = Group2,
  y = BlinkRate
)

px1 <- px + 
  # Add labels and title
  labs(
    x = "DT",
    #y = "Bill Length",
    #title = "Distribution of bill length across penguins species"
  ) + 
  # Customizations
  theme(
    # This is the new default font in the plot
    text = element_text(family = "Roboto", size = 8, color = "black"),
    plot.title = element_text(
      family = "Lobster Two", 
      size = 20,
      face = "bold",
      color = "#2a475e"
    ),
    # Statistical annotations below the main title
    plot.subtitle = element_text(
      family = "Roboto", 
      size = 15, 
      face = "bold",
      color="#1b2838"
    ),
    plot.title.position = "plot", # slightly different from default
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 12)
  )


px2 <- px1  +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_line(colour = "grey50"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4")
  )

ggsave(
  filename = "Plots/TreatmentWise_BlinkRate/DT_BlinkRate_Welch.png",
  plot = px2,
  width = 8,
  height = 8,
  device = "png"
)






xz = d[,c("Treatment","Group","BlinkRate")]
xz$Group[which(xz$Group %in% c("CH","BH"))] = "High"
xz$Group[which(xz$Group %in% c("CL","BL"))] = "Low"
#x1 = melt(x1)
p1 = ggplot(x1, aes(x = Group, y = BlinkRate)) + geom_boxplot(notch = T) + stat_summary(fun = mean, geom = "point", size = 3) + labs(x = "")

na = filter(xz, Treatment == "NA")
rb = filter(xz, Treatment == "RB")
st = filter(xz, Treatment == "ST")
pr = filter(xz, Treatment == "PR")
pm = filter(xz, Treatment == "PM")
dt = filter(xz, Treatment == "DT")

q = dt

# pxy <- ggwithinstats(
#   data = q,
#   x = Group2,
#   y = BlinkRate
# )
px <- ggwithinstats(
  data = q,
  x = Group,
  y = BlinkRate
)

px1 <- px + 
  # Add labels and title
  labs(
    x = "DT",
    #y = "Bill Length",
    #title = "Distribution of bill length across penguins species"
  ) + 
  # Customizations
  theme(
    # This is the new default font in the plot
    text = element_text(family = "Roboto", size = 8, color = "black"),
    plot.title = element_text(
      family = "Lobster Two", 
      size = 20,
      face = "bold",
      color = "#2a475e"
    ),
    # Statistical annotations below the main title
    plot.subtitle = element_text(
      family = "Roboto", 
      size = 15, 
      face = "bold",
      color="#1b2838"
    ),
    plot.title.position = "plot", # slightly different from default
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 12)
  )


px2 <- px1  +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_line(colour = "grey50"),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4")
  )

ggsave(
  filename = "Plots/TreatmentWise_BlinkRate/DT_BlinkRate_HvsL.png",
  plot = px2,
  width = 8,
  height = 8,
  device = "png"
)
