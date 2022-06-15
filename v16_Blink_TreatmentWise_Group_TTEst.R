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
library(rstatix)


#Reading the Data
d = read.csv("Data/Blink+Gaze_Summary_TreatmentLevel.csv")
d$Treatment[is.na(d$Treatment)] = "NA"
d$BlinkRate[which(d$BlinkRate == -1)] = NA
d$BlinkRate[which(d$BlinkRate == 999)] = NA
d$BlinkRate[which(d$BlinkRate > 60)] = NA
write.csv(d, "Data/Blink+Gaze_Summary_TreatmentLevel.csv", row.names = F)


df1 = d[,c(2,12)]
df1 = pivot_longer(df, -BlinkRate, names_to = "variables", values_to = "value")
st = df1 %>%
  group_by(variables) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance()
mp <- ggboxplot(
  df1, x = "BlinkRate", y = "value",
  fill = "BlinkRate", palette = "npg", legend = "none",
  ggtheme = theme_pubr(border = TRUE)
) +
  facet_wrap(~variables)

df2 = d[,c(2,3,12)]
df2 = filter(df2, Treatment == "ST")
df2 = as.tibble(melt(df2))

st2 = df2 %>%
  group_by(variable) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance()

mp2 <- ggboxplot(
  df2, x = "Group", y = "value",
  fill = "Group", palette = "npg", legend = "none",
  ggtheme = theme_pubr(border = TRUE)
) +
  facet_wrap(~variable)


for (grp in unique(d$Group)) {
  print(grp)
  d1 = filter(d, Group == grp)
  for (sub in unique(d1$Participant_ID)) {
    print(sub)
    d2 = filter(d1, Participant_ID == sub)
    for (tr in unique(d3$Treatment)) {
      print(tr)
      d4 = filter(d3, Treatment == tr)
    }
    g = plot(ggarrange(stgNA, stgRB, stgPM, stgST, stgDT, stgPR, nrow = 1, common.legend = T, legend = "bottom"))
    pdf(paste0("Plots/Gaze_Subject_TreatmentWise_Portrait/", sub,"Gaze.pdf"), width = 18, height = 6)
    plot(g)
    dev.off()
    assign(paste0("g",i), g)
  }
  
  
}






