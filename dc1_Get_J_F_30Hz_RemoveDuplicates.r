library(tidyverse)

dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)

x = read.csv("Data/All_J_F_Raw.csv")
colnames(x)

s = x$Frame[1]
e = x$Frame[nrow(x)]

x1 = filter(x, Sector == 1)
colnames(x1) = paste0("L_",colnames(x1))
x1 = x1[!duplicated(x1$L_Frame),]

x2 = filter(x, Sector == 2)
colnames(x2) = paste0("C_",colnames(x2))
x2 = x2[!duplicated(x2$C_Frame),]

x3 = filter(x, Sector == 3)
colnames(x3) = paste0("R_",colnames(x3))
x3 = x3[!duplicated(x3$R_Frame),]


x4 = data.frame(MainFrame = x$Frame[1]:x$Frame[nrow(x)])
x5 = merge(x4, x1, by.x = "MainFrame", by.y = "L_Frame", all.x = T)
#write.csv(x5, "x4x1.csv", row.names = F)

x6 = merge(x5, x2, by.x = "MainFrame", by.y = "C_Frame", all.x = T)
#write.csv(x6, "x4x2.csv", row.names = F)

x7 = merge(x6, x3, by.x = "MainFrame", by.y = "R_Frame", all.x = T)
#write.csv(x7, "FACS_combined_noDuplicates.csv", row.names = F)

x7["S"] = NA
x7["ActualFrame"] = x7$MainFrame

x7$S = seq.int(0,538.23333, 1/30)
write.csv(x7, "Data/All_J_F_30Hz_No_Duplicates.csv", row.names = F)
