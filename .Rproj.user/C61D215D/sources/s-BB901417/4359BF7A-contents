# Add CompositeJudges + DomEmo + SumEmo + Binary

ss = Sys.time()

library(tidyverse)
library(dplyr)
library(readxl)
library(zoo)

dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)

source("Scripts/getBinary.R")
source("Scripts/getDomSumEmo.R")

d = read.csv("Data/Presenter-Judges_FGSP_PR_1HzMean_2022-04-06.csv")

d2 = read.csv("Data/2021/Judges-Presenter_1Hz.csv")

d[c("J_Angry","J_Disgusted","J_Afraid","J_Happy","J_Sad","J_Surprised","J_Neutral","F_DomEmo", "F_DomEmoBinary", "F_SumEmo", "F_SumEmoBinary","L_DomEmo", "L_DomEmoBinary", "L_SumEmo", "L_SumEmoBinary","C_DomEmo", "C_DomEmoBinary","C_SumEmo", "C_SumEmoBinary","R_DomEmo", "R_DomEmoBinary","R_SumEmo", "R_SumEmoBinary","J_DomEmo", "J_DomEmoBinary", "J_SumEmo", "J_SumEmoBinary","F_SumCheck","L_SumCheck","C_SumCheck","R_SumCheck","J_SumCheck")] = NA

cnm = colnames(d2[,c(136:164)])
d[cnm] = NA

for (i in 1:nrow(d)) {
  print(i)
  d$F_SumCheck[i] = sum(d[i,6:12])
  d$L_SumCheck[i] = sum(d[i,42:48])
  d$C_SumCheck[i] = sum(d[i,52:58])
  d$R_SumCheck[i] = sum(d[i,62:68])
  
  d$J_Angry[i] = mean(c(d$L_angry[i], d$C_angry[i], d$R_angry[i]), na.rm = T)
  d$J_Disgusted[i] = mean(c(d$L_disgusted[i], d$C_disgusted[i], d$R_disgusted[i]), na.rm = T)
  d$J_Afraid[i] = mean(c(d$L_afraid[i], d$C_afraid[i], d$R_afraid[i]), na.rm = T)
  d$J_Happy[i] = mean(c(d$L_happy[i], d$C_happy[i], d$R_happy[i]), na.rm = T)
  d$J_Sad[i] = mean(c(d$L_sad[i], d$C_sad[i], d$R_sad[i]), na.rm = T)
  d$J_Surprised[i] = mean(c(d$L_surprised[i], d$C_surprised[i], d$R_surprised[i]), na.rm = T)
  d$J_Neutral[i] = mean(c(d$L_neutral[i], d$C_neutral[i], d$R_neutral[i]), na.rm = T)
  
  d$J_SumCheck[i] = sum(d[i,81:87])
  
  fd = d[i,6:12]
  ld = d[i,42:48]
  cd = d[i,52:58]
  rd = d[i,62:68]
  jd = d[i,81:87]
  
  d$F_DomEmo[i] = getDomEmo(fd)
  d$L_DomEmo[i] = getDomEmo(ld)
  d$C_DomEmo[i] = getDomEmo(cd)
  d$R_DomEmo[i] = getDomEmo(rd)
  d$J_DomEmo[i] = getDomEmo(jd)
  
  d$F_DomEmoBinary[i] = getDomBinary(getDomEmo(fd))
  d$L_DomEmoBinary[i] = getDomBinary(getDomEmo(ld))
  d$C_DomEmoBinary[i] = getDomBinary(getDomEmo(cd))
  d$R_DomEmoBinary[i] = getDomBinary(getDomEmo(rd))
  d$J_DomEmoBinary[i] = getDomBinary(getDomEmo(jd))
  
  d$F_SumEmo[i] = getSumEmo(d$F_Neutral[i])
  d$L_SumEmo[i] = getSumEmo(d$L_neutral[i])
  d$C_SumEmo[i] = getSumEmo(d$C_neutral[i])
  d$R_SumEmo[i] = getSumEmo(d$R_neutral[i])
  d$J_SumEmo[i] = getSumEmo(d$J_Neutral[i])
  
  d$F_SumEmoBinary[i] = getSumBinary(d$F_Neutral[i])
  d$L_SumEmoBinary[i] = getSumBinary(d$L_neutral[i])
  d$C_SumEmoBinary[i] = getSumBinary(d$C_neutral[i])
  d$R_SumEmoBinary[i] = getSumBinary(d$R_neutral[i])
  d$J_SumEmoBinary[i] = getSumBinary(d$J_Neutral[i])
  
  sub = d$Participant_ID[i]
  d3 = filter(d2, Participant_ID == sub)
  
  d$LengthPR[i] = d3$LengthPR[1]
  
  d$Age[i] = d3$Age[1]
  d$Gender[i] = d3$Gender[1]
  d$Nationality[i] = d3$Nationality[1]
  d$Other_Nationality[i] = d3$Nationality[1]
  d$Native_Language[i] = d3$Native_Language[1]
  d$Other_Native_Language[i] = d3$Other_Native_Language[1]
  d$Writing_Proficiency[i] = d3$Writing_Proficiency[1]
  
  d$BFI_Agreeableness[i] = d3$BFI_Agreeableness[1]
  d$BFI_Conscientiousness[i] = d3$BFI_Conscientiousness[1]
  d$BFI_Extraversion[i] = d3$BFI_Extraversion[1]
  d$BFI_Neuroticism[i] = d3$BFI_Neuroticism[1]
  d$BFI_Openness[i] = d3$BFI_Openness[1]
  
  d$ERQ_Cognitive_Reappraisal[i] = d3$ERQ_Cognitive_Reappraisal[1]
  d$ERQ_Expressive_Suppression[i] = d3$ERQ_Expressive_Suppression[1]
  d$Percieved_Stress_Scale[i] = d3$Percieved_Stress_Scale[1]
  
  d$NASA_Mental_Demand[i] = d3$NASA_Mental_Demand[1]
  d$NASA_Physical_Demand[i] = d3$NASA_Physical_Demand[1]
  d$NASA_Temporal_Demand[i] = d3$NASA_Temporal_Demand[1]
  d$NASA_Performance[i] = d3$NASA_Performance[1]
  d$NASA_Effort[i] = d3$NASA_Effort[1]
  d$NASA_Frustration[i] = d3$NASA_Frustration[1]
  
  d$Word_Count[i] = d3$Word_Count[1]
  d$Character_Count[i] = d3$Word_Count[1]
  d$Criterion_Score[i] = d3$Criterion_Score[1]
  d$Grammar_Errors.WC[i] = d3$Grammar_Errors.WC[1]
  d$Usage_Errors.WC[i] = d3$Grammar_Errors.WC[1]
  d$Style_Errors.WC[i] = d3$Style_Errors.WC[1]
  d$Mechanic_Errors.WC[i] =d3$Mechanic_Errors.WC[1]
  
}

write.csv(d, "Data/Presenter-Judges_FGSPB_PR_1HzMean_SemiFinal_2022-04-06.csv")




