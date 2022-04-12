### Code updated: Sept 13 2020
### This is the code for the CHI-2021 paper submitted regarding the 
### presentation data (Office Task 2019) that provided by Amanveer.
### The concept is that a subject is presenting their work to a 
### 3 member committee/judges (online which are actually pre-recorded 
### and presented the same to all subjects) which have various emotions
### expressed, which act as stimulus to the subjects, whose emotions
### are also recorded and we are interested in finding how they correlate
### with the stimulus
### _____________________________________________________________________
library(gplots)
library(lme4)
library(sjPlot)
library(lattice)
library(ltm)
library(glmmTMB)
library(dplyr)
library(nnet)
library(ggplot2)

### Here we have a list of Subjects that gave a presentation
Sub<-c("T005","T009","T011","T016","T019","T021","T032","T035","T046","T051",
       "T061","T063","T064","T065","T066","T068","T077","T078","T079","T082",
       "T083","T084","T091","T092","T093","T094","T096","T098","T099","T106",
       "T112","T114","T121","T122","T124","T126","T128","T138","T141","T144",
       "T151","T152","T154","T157","T162","T166","T172","T173","T174","T175",
       "T176","T178")

N<-length(Sub)  ### Total number of subjects participating in the study

### Names used multiple times in the code
EmNames<-c("An","Di","Af","Ha","Sa","Su","Ne")  ### Emotion Names
EmColor<-c("red","orange","blue","green","purple","yellow","cyan")  ### Emotion colors in plots
EmNamesbin<-c("N","E") ### Binary Emotion Names (N=Neutral, E=Emotion)
EmColorbin<-c("cyan","brown") ### Binary Emotion colors in plots

### setwd("C:/Users/PT_office/Desktop/CHI2021/Data/")		### This is for the Office PC
###setwd("/Users/pt/CPL/CHI2021_presentation_task/Data/")    ### This is for the MAC
setwd("/Users/cpladmin/Desktop/judges-presenter/Data") ### Amanveer

### Here we will load the latest version of the data (03 Sept 2020)
### ignore the fact that the name of the variable tmpData01Sept refers to 01 Sept
### The next refers to the data at a second level (i.e. 1 Hz)
### Name of the file: Judges-Presenter_1Hz_final.csv
tmpData01Sept<-read.csv("Judges-Presenter_1Hz_v3.csv",header=T,sep=",")

RV<-dim(tmpData01Sept)[2]  ### Recorded Variables in the study

### Here we record the maximum length of the presentation over all subjects
### which we will need to create the maximum number of rows of 3D array
### where Subjects will be stuck one on the top of the other (i.e. rows=
### maximum presentation length, columns=study variables, layers=subjects)
PRmaxL<-table(tmpData01Sept[,1])
MPRmaxL<-max(PRmaxL)


### We will create a 3d array named Dpr (Data Presentation) that will have:
### rows=MPRmaxL columns=163 (variables of the study) and layers=N (number of subjects)
Dpr<-array(NA,dim=c(MPRmaxL,RV,N))
tmpl<-1
for (i in Sub){
  tmpRowInd<-tmpData01Sept[,1]==i
  tmpD<-tmpData01Sept[tmpRowInd,]
  tmpPRind<-which(tmpD[,33]=="PR")
  tmpD1<-as.matrix(tmpD[tmpPRind,])
  if (dim(tmpD1)[1]>0){
    Dpr[1:length(tmpPRind),,tmpl]<-tmpD1
    print(i)
  } else {
    print(paste("No Presentation Data for Subject",i))
  }
  tmpl<-tmpl+1
} 

### Here we have a list of Subjects that gave a presentation
Sub<-c("S005","S009","S011","S016","S019","S021","S032","S035","S046","S051",
       "S061","S063","S064","S065","S066","S068","S077","S078","S079","S082",
       "S083","S084","S091","S092","S093","S094","S096","S098","S099","S106",
       "S112","S114","S121","S122","S124","S126","S128","S138","S141","S144",
       "S151","S152","S154","S157","S162","S166","S172","S173","S174","S175",
       "S176","S178")


### Next we will record the Data for emotions (Dem) of the subject (FACS+sound) 
### and the emotions (FACS) of the three judges (Left, Center, Right). The 
### relevant variables (columns) that we will record in matrix Dem is:
### Columns 4-10: FACS for Subject
### Columns 28-32: Sound based emotion for Subject
### Columns 40-46: FACS for Left Judge
### Columns 50-56: FACS for Center Judge
### Columns 60-66: FACS for Right Judge
EmV<-c(4:10,28:32,40:46,50:56,60:66)  ### Emotion related Variables
LEmV<-length(EmV)
Dem<-array(NA,dim=c(MPRmaxL,LEmV,N))
for (k in 1:N){ ### here we loop over Subjects
  Dem[,1:7,k]<-as.numeric(Dpr[,4:10,k]) ### FACS for Subject
  Dem[,8:14,k]<-as.numeric(Dpr[,40:46,k]) ### FACS for Left Judge
  Dem[,15:21,k]<-as.numeric(Dpr[,50:56,k]) ### FACS for Center Judge
  Dem[,22:28,k]<-as.numeric(Dpr[,60:66,k]) ### FACS for Right Judge
  Dem[,29:33,k]<-as.numeric(Dpr[,28:32,k]) ### Sound based emotion for Subject
}

### In the following matrix we will record the average of each of the 
### Emotion related Variables per Subject. So rows=number of Subjects
### columns=all the EmV variables
MeanEm<-matrix(NA,N,LEmV)
for (k in 1:N){ ### here we loop over Subjects
  MeanEm[k,]<-colMeans(Dem[,,k],na.rm=T)
}

setwd("/Users/cpladmin/Desktop/judges-presenter/Plots/DrP")    ### This is for the MAC

###setEPS()
###postscript("Mean_FACS_participant_and_judges_all_emotions.eps")
### Plot of Mean FACS values (all emotions)
ylim1<-c(0,max(MeanEm[,1:28],na.rm=T))
par(mfrow=c(2,2),mar=c(2,2,3,2),oma=c(0.5,0.5,0.5,0.5))
boxplot(MeanEm[,1:7],names=EmNames,ylim=ylim1,col=EmColor,main="Mean Participant")
boxplot(MeanEm[,8:14],names=EmNames,ylim=c(0,1),col=EmColor,main="Left Judge")
boxplot(MeanEm[,15:21],names=EmNames,ylim=ylim1,col=EmColor,main="Center Judge")
boxplot(MeanEm[,22:28],names=EmNames,ylim=ylim1,col=EmColor,main="Right Judge")
###dev.off()


### Here we will record the Dominant Emotion at each recorded frame of each
### subject and judge of the study. DomEm will be an array with rows=MPRmaxL,
### columns=5 (Col1=dominant FACS emotion of Subject, Col2-4: dominant FACS 
### emotion of Left/Center/Right Judge, Col5=dominant Speech emotion of Subject)
DomEm<-array(NA,dim=c(MPRmaxL,5,N))
for (k in 1:N){ ### here we loop over Subjects
  for (i in 1:MPRmaxL){ ### here we loop over frames
    if (sum(!is.na(Dem[i,1:7,k]))==7){  ### check that we have no NA data
      DomEm[i,1,k]<-which.max(Dem[i,1:7,k]) ###Dominant FACS Subject
    }
    if (sum(!is.na(Dem[i,8:14,k]))==7){  ### check that we have no NA data
      DomEm[i,2,k]<-which.max(Dem[i,8:14,k]) ###Dominant FACS Left Judge
    }
    if (sum(!is.na(Dem[i,15:21,k]))==7){  ### check that we have no NA data
      DomEm[i,3,k]<-which.max(Dem[i,15:21,k]) ###Dominant FACS Center Judge
    }
    if (sum(!is.na(Dem[i,22:28,k]))==7){  ### check that we have no NA data
      DomEm[i,4,k]<-which.max(Dem[i,22:28,k]) ###Dominant FACS Right Judge
    }
    if (sum(!is.na(Dem[i,29:33,k]))==5){  ### check that we have no NA data
      DomEm[i,5,k]<-which.max(Dem[i,29:33,k]) ###Dominant Speech Subject
    }
  }
  print(k)
}

### These are the % of each Dominant emotion of Subject FACS during presentations
rbind(EmNames,round(prop.table(table(DomEm[,1,]))*100,2))


### Here we will find the length (in frames) of the NOT-missing emotion data
### for each subject and each of the judges. We will ignore the speech and 
### concentrate only on FACS (of Subject and the three judges).
### In the matrix DomEmLength we will put how many non-NA frames we have for
### each subject (rows) with col1=Subject's not-NA FACS frames and col2-4=
### not-NA FACS frames of the Left/Center/Right judges respectively
DomEmLength<-matrix(NA,N,4)
for (k in 1:N){ ### here we loop over Subjects
  for (j in 1:4){ ### here we loop over Subject (1) and Judges (2-4)
    DomEmLength[k,j]<-sum(!is.na(DomEm[,j,k]))
  }
}

### report the minimum Length (over Subject and Judges)
MinDomEmLength<-rep(NA,N)   
for (i in 1:N){ ### here we loop over Subjects
  MinDomEmLength[i]<-min(DomEmLength[i,],na.rm=T)
}

### Here we record which subjects have too few response data (i.e. Subject's FACS<cutoff)
Cutoff<-10 ### The threshold of frames below which we exclude the case for data at 1Hz LEVEL
ExcludeSubjectsInd<-which(MinDomEmLength<Cutoff)
ExcludeSubjects<-Sub[which(MinDomEmLength<Cutoff)]
ExcludeSubjectsFrames<-MinDomEmLength[MinDomEmLength<Cutoff]

ExcludeSubjects
ExcludeSubjectsFrames


### Here we will collect the data that we will use for the analysis in DAarray:
### rows are the frames of the presentation, layers are the subjects and
### col1: binary with 0=Neutral, 1=Emotion FACS for SUBJECT
### col2: binary with 0=Neutral, 1=Emotion FACS for committee, where to get
###       0 we need to have all three judges to be Neutral. If at least one
###       of the three judges is NA then we will have NA for the committee
### col3: binary with 0=Neutral, 1=Emotion SPEECH for SUBJECT
DAarray<-array(NA,dim=c(MPRmaxL,3,N))
for (k in 1:N){ ### here we loop over Subjects
  ### This is for the Subject's FACS
  DAarray[,1,k]<-DomEm[,1,k]
  DAarray[(DAarray[,1,k]!=7),1,k]<-1  ### Subject FACS Emotion
  DAarray[(DAarray[,1,k]==7),1,k]<-0  ### Subject FACS Neutral
  ### This is for the Committee's FACS
  tmpD<-DomEm[,2:4,k]
  tmpD[(tmpD!=7)]<-1  ### Judges FACS Emotion
  tmpD[(tmpD==7)]<-0  ### Judges FACS Neutral
  DAarray[,2,k]<-rowSums(tmpD)
  DAarray[(DAarray[,2,k]!=0),2,k]<-1  ### At least one Judge is non-Neutral and we have no NAs
  ### This is for the Subject's speech
  DAarray[,3,k]<-DomEm[,5,k]
  DAarray[(DAarray[,3,k]!=5),3,k]<-1  ### Subject Speech Emotion
  DAarray[(DAarray[,3,k]==5),3,k]<-0  ### Subject Speech Neutral
}


### Subject's FACS and Committee's FACS available data points
for (k in 1:N){
  print(paste("Subject",Sub[k]))
  print(sum(table(factor(DAarray[,1,k],levels = 0:1),factor(DAarray[,2,k],levels = 0:1))))
}

### Here we will run the mixed effects model using only the Subjects that
### has at least n=10 (for 1 Hz) data of FACS for Subject AND committee
ExcludeSubjectsBivariate<-NA
for (k in 1:N){ ### here we loop over Subjects
  ### Tables with BINARIZED FACS
  TablesubcommFB<-table(factor(DAarray[,1,k],levels = 0:1),factor(DAarray[,2,k],levels = 0:1))
  if (sum(TablesubcommFB)<10){    ### for 1Hz data
    ExcludeSubjectsBivariate<-c(ExcludeSubjectsBivariate,k)
  }  
}  
ExcludeSubjectsBivariate<-ExcludeSubjectsBivariate[-1]
Sub[ExcludeSubjectsBivariate]


### Here we will define the variables that will be used in the linear model
### (mixed effects logistic regression)

### We will merge the "BL" and "CL" into the class "L" (Low) while the 
### "BH" and "CH" will form class "H" (High)
tmpGr<-rep(NA,N)
tmpPPpres<-rep(NA,N) ### Perinasal Perspiration during the PRESENTATION segment
tmpPPbase<-rep(NA,N) ### Perinasal Perspiration during the BASELINE segment
tmpB5A<-rep(NA,N)  ### BFI_Agreeableness
tmpB5C<-rep(NA,N)  ### BFI_Conscientiousness
tmpB5O<-rep(NA,N)  ### BFI_Openness
tmpGEpW<-rep(NA,N) ### Grammar_Errors_WC
tmpMEpW<-rep(NA,N) ### Mechanic_Errors_WC

for (k in 1:N){ ### here we loop over Subjects
  if (sum((unique(Dpr[,34,k])=="BL"),na.rm=T)|sum((unique(Dpr[,34,k])=="CL"),na.rm=T)){
    tmpGr[k]<-"L"
  }
  if (sum((unique(Dpr[,34,k])=="BH"),na.rm=T)|sum((unique(Dpr[,34,k])=="CH"),na.rm=T)){
    tmpGr[k]<-"H"
  }
  if (sum(!is.na(Dpr[,72,k]))>0){
    tmpPPpres[k]<-mean(as.numeric(Dpr[,72,k]),na.rm=T)
  }
  if (sum(!is.na(Dpr[,167,k]))>0){
    tmpPPbase[k]<-as.numeric(names(table(Dpr[,167,k])))
  }
  if (sum(!is.na(Dpr[,144,k]))>0){
    tmpB5A[k]<-as.numeric(names(table(Dpr[,144,k])))
  }
  if (sum(!is.na(Dpr[,145,k]))>0){
    tmpB5C[k]<-as.numeric(names(table(Dpr[,145,k])))
  }
  if (sum(!is.na(Dpr[,148,k]))>0){
    tmpB5O[k]<-as.numeric(names(table(Dpr[,148,k])))
  }
  if (sum(!is.na(Dpr[,161,k]))>0){
    tmpGEpW[k]<-as.numeric(names(table(Dpr[,161,k])))
  }
  if (sum(!is.na(Dpr[,164,k]))>0){
    tmpMEpW[k]<-as.numeric(names(table(Dpr[,164,k])))
  }
}
tmpGr<-factor(tmpGr,levels = c("L","H"))  
tmpPP<-tmpPPpres-tmpPPbase  ### PP signal normalized index


tmpY<-c(DAarray[,1,1])  ### Subject's FACS (response)
tmpX<-c(DAarray[,2,1])  ### Committee's FACS (fixed effect)
S<-rep(Sub[1],MPRmaxL)  ### Subject ID (random effects)
Gr<-rep(tmpGr[1],MPRmaxL) ### L/H Group (fixed effect)
PPn<-rep(tmpPP[1],MPRmaxL) ### Normalized Perinasal Perspiration mean (fixed effect)
B5Agr<-rep(tmpB5A[1],MPRmaxL) ### BFI_Agreeableness (fixed effect)
B5C<-rep(tmpB5C[1],MPRmaxL) ### BFI_Conscientiousness (fixed effect)
B5O<-rep(tmpB5O[1],MPRmaxL) ### BFI_Openness (fixed effect)
GEpW<-rep(tmpGEpW[1],MPRmaxL) ### Grammar_Errors_WC (fixed effect)
MEpW<-rep(tmpMEpW[1],MPRmaxL) ### Mechanic_Errors_WC (fixed effect)
for (k in 2:N){ ### here we loop over Subjects
  if (sum(k==ExcludeSubjectsBivariate)==0){
    tmpY<-c(tmpY,DAarray[,1,k])
    tmpX<-c(tmpX,DAarray[,2,k])
    S<-c(S,rep(Sub[k],MPRmaxL))
    Gr<-c(Gr,rep(tmpGr[k],MPRmaxL))
    B5Agr<-c(B5Agr,rep(tmpB5A[k],MPRmaxL))
    PPn<-c(PPn,rep(tmpPP[k],MPRmaxL))
    B5C<-c(B5C,rep(tmpB5C[k],MPRmaxL))
    B5O<-c(B5O,rep(tmpB5O[k],MPRmaxL))
    GEpW<-c(GEpW,rep(tmpGEpW[k],MPRmaxL))
    MEpW<-c(MEpW,rep(tmpMEpW[k],MPRmaxL))
  }
}  

### paper notation
P<-tmpY
tmpX<-factor(tmpX,levels = c(0,1),labels = c("N","E"))
J<-tmpX
Gr<-factor(Gr,levels=c("1","2"),labels = c("NI","I"))


### Here we scale the continuous variables
sPP<-scale(PPn)
sB5A<-scale(B5Agr)
sB5C<-scale(B5C)
sB5O<-scale(B5O)
sGEpW<-scale(GEpW)
sMEpW<-scale(MEpW)

PP<-sPP
B5A<-sB5A


### Continuous variables and their scaling
cbind(Sub,tmpPP,scale(tmpPP),tmpB5A,scale(tmpB5A))



FM<-glmer(P~1+J+Gr+J:Gr+PP+J:PP+B5A+J:B5A+(1|S),family=binomial,control=glmerControl(optimizer="bobyqa"),nAGQ=10)
summary(FM)

### Here is the model's output
summary(FM)
anova(FM)
#confint(FM,level = 0.95)  ### this takes long time (~10 minutes)

# print the model results without correlations among fixed effects
print(FM, corr = FALSE)
se <- sqrt(diag(vcov(FM)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(FM), LL = fixef(FM) - 1.96 * se, UL = fixef(FM) + 1.96 * se))
### transform from log(odds) to odds
exp(tab)
### transform from odds to probabilities
exp(tab)[,1]/(1+exp(tab)[,1])




###setEPS()
###postscript("Figure_2_a.eps")
### Here we will plot the random effects
#plot_model(FM,"re") ### Plots the random effects using odds
plot_model(FM,"re")+font_size(labels.x=20,labels.y=14,title=0,axis_title.x = 20,axis_title.y = 15)
###dev.off()

###setEPS()
###postscript("Figure_2_b.eps")
### Here we will plot the fixed effects
#plot_model(FM,"est",sort=TRUE, show.values = TRUE, value.offset = .3) ### put labels
plot_model(FM,"est",sort=TRUE, show.values = TRUE, value.offset = .3,xlab="")+
  font_size(labels.x=20,labels.y=20,title = 0,axis_title.x =0)
###dev.off()

b1 = expression(italic("B5"[A]))
b2 = expression(italic("J[E]")~plain(x)~italic("Gr[I]"))
b3 = expression(bar(Delta*bold("PP")))
b4 = expression(italic("J[E]")~plain(x)~bar(Delta*bold("PP")))
b5 = expression(italic("J[E]")~plain(x)~italic("B5"[A]))
b6 = expression(italic("J[E]"))
b7 = expression(italic("Gr[I]"))

plot_model(FM,"est",sort=TRUE, show.values = TRUE, value.offset = .3,xlab="",
           axis.labels = c(b7, b6, b5, b4, b3, b2, b1), value.size = 7, dot.size = 5, line.size = 2)+
  font_size(labels.x=20,labels.y=20,title = 0,axis_title.x =0)


#plot_model(FM,"pred") ### Predicted values (marginal effects) for each of model terms
#plot_model(FM,"pred",terms = "PP[all]") ### Predicted values (marginal effects) for sPP
#plot_model(FM,"pred",terms = "B5A[all]") ### Predicted values (marginal effects) for sB5A

###setEPS()
###postscript("Figure_3_a.eps")
ws = expression(italic("J"))
ws1 = expression(italic("P"))
ws2 = expression(italic("NI"))
ws3 = expression(italic("I"))
plot_model(FM,"pred", dot.size = 6, line.size = 2, axis.title = c(ws,ws1), auto.label = FALSE ,axis.labels = c("A","B"))$J+
  font_size(labels.x=25,labels.y=25,title = 0, axis_title.x = 25,axis_title.y = 25)
###dev.off()
###setEPS()
###postscript("Figure_3_b.eps")
plot_model(FM,"pred", dot.size = 6, line.size = 2, axis.title = c(expression(italic("Gr")) ,ws1))$Gr+
  font_size(labels.x=25,labels.y=25,title = 0,axis_title.x = 25,axis_title.y = 0)
###dev.off()
###setEPS()
###postscript("Figure_3_c.eps")
xx1 = expression(bar(Delta*bold("PP")))
xx2 = expression(italic("P"))
plot_model(FM,"pred",terms = "PP[all]", axis.title = c(xx1,xx2), line.size = 2)+
  font_size(labels.x=25,labels.y=25,title = 0,axis_title.x = 25,axis_title.y = 25)
###dev.off()
###setEPS()
###postscript("Figure_3_d.eps")
x = expression(italic("B5"[A]))
y = expression(italic("P"))
plot_model(FM,"pred",terms = "B5A[all]", line.size = 2, axis.title = c(x,"")  )+
  font_size(labels.x=20,labels.y=25,title = 0,axis_title.x = 25,axis_title.y = 20)
###dev.off()


###setEPS()
###postscript("Figure_3_e.eps")
### Interaction plots
#plot_model(FM,"int")
par(mar = c(1,1,1,1))
sdf = c("x","y")
p = plot_model(FM,"int", axis.labels = c("A","B"), axis.title = c(expression(italic(J)),""), grid = F, line.size = 2,
           dot.size = 6, legend.title = expression(italic("Gr")))[[1]]+
  font_size(labels.x=20,labels.y=0,title = 0,axis_title.x = 25,axis_title.y = 0)+
  #xlab(ws)+
  #scale_x_discrete(breaks = c("N","E"), labels = c("N","E"))+
  set_theme(legend.size = 2,
            legend.title.size = 2,
            axis.textsize.x = 1.4,
            axis.textsize.y = 1.1,
            title.size = -1,
            #axis.title.y.vjust = 10,
            axis.title.size = 2.5)
###dev.off()
# 
# 
# ### Here we will provide the 2x2 tables regarding the Judged and Subject N/E levels
# ### per group of subjects (Gr = Uninformed or Informed)
# ### The Gr==U corresponds to the Uninformed group 
# GrU<-Gr=="U" 
# table(P[GrU],J[GrU])
# round(prop.table(table(P[GrU],J[GrU]),2),3)*100
# barplot(table(P[GrU],J[GrU]),beside=T,col=c("cyan","brown"),ylab="Counts",xlab="Judges",cex.names = 1.5,cex=1.5,cex.lab=1.5,main="Uninformed")
# legend("topleft",c("N","E"),fill=c("cyan","brown"),horiz=T,bty="n",title="Subjects",cex = 1.5)
# barplot(prop.table(table(P[GrU],J[GrU]),2),beside=T,ylim=c(0,1),col=c("cyan","brown"),ylab="Percentage",xlab="Judges",cex.names = 1.5,cex=1.5,cex.lab=1.5,main="Uninformed")
# legend("topright",c("N","E"),fill=c("cyan","brown"),horiz=T,bty="n",title="Subjects",cex = 1.5)
# ### The Gr==I corresponds to the Informed group 
# GrI<-Gr=="I" 
# table(P[GrI],J[GrI])
# round(prop.table(table(P[GrI],J[GrI]),2),3)*100
# barplot(table(P[GrI],J[GrI]),beside=T,col=c("cyan","brown"),ylab="Counts",xlab="Judges",cex.names = 1.5,cex=1.5,cex.lab=1.5,main="Informed")
# legend("topleft",c("N","E"),fill=c("cyan","brown"),horiz=T,bty="n",title="Subjects",cex = 1.5)
# barplot(prop.table(table(P[GrI],J[GrI]),2),beside=T,ylim=c(0,1),col=c("cyan","brown"),ylab="Percentage",xlab="Judges",cex.names = 1.5,cex=1.5,cex.lab=1.5,main="Informed")
# legend("topright",c("N","E"),fill=c("cyan","brown"),horiz=T,bty="n",title="Subjects",cex = 1.5)
# 


add_legend <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
              mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}

GrU<-Gr=="U" 
GrI<-Gr=="I" 

par(mfrow=c(1,2))
barplot(prop.table(table(P[GrU],J[GrU]),2),beside=F,ylim=c(0,1),col=c("cyan","brown"),ylab="Percentage",xlab="Judges",cex.names = 1.5,cex=1.5,cex.lab=1.5,main="Uninformed Subjects")
barplot(prop.table(table(P[GrI],J[GrI]),2),beside=F,ylim=c(0,1),col=c("cyan","brown"),ylab="",xlab="Judges",cex.names = 1.5,cex=1.5,cex.lab=1.5,main="Informed  Subjects",yaxt="n")
add_legend("top", legend=c("N","E"),fill=c("cyan","brown"),horiz=T,bty="n",cex = 1.3)


P1<-P
P1<-factor(P,levels = c(1,0),labels = c("E","N"))
par(mfrow=c(1,2))
barplot(prop.table(table(P1[GrU],J[GrU]),2),beside=F,ylim=c(0,1),col=c("brown","cyan"),ylab="Percentage",xlab="Judges",cex.names = 1.5,cex=1.5,cex.lab=1.5,main="Uninformed Subjects")
barplot(prop.table(table(P1[GrI],J[GrI]),2),beside=F,ylim=c(0,1),col=c("brown","cyan"),ylab="",xlab="Judges",cex.names = 1.5,cex=1.5,cex.lab=1.5,main="Informed Subjects",yaxt="n")
add_legend("top", legend=c("E","N"),fill=c("brown","cyan"),horiz=T,bty="n",cex = 1.3)






