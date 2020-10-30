pdata = read.csv("rawdatawithmissing2.csv",fill=TRUE,sep=",",header = TRUE)

#overall correction 
mean(pdata$overall.correction.rate,na.rm=TRUE)
mean(pdata$Age,na.rm=TRUE)
sd(pdata$Age,na.rm=TRUE)
options(digits=2)
prop.table(table(pdata$Occupation.Category),na.rm=TRUE)
prop.table(table(pdata$Occupation.Category),na.rm=TRUE)
round(2)
########## test carry vs not##########
carrykit=pdata[pdata$FA.Kit.=="yes",]
notcarry=pdata[pdata$FA.Kit.=="no",]
t.test(carrykit$overall.correction.rate,notcarry$overall.correction.rate)



chisq.test(pdata$FA.Kit.,pdata$overall.correction.rate)
with(pdata,addmargins(table(FA.Kit.,Know.FA,dnn=c("carry FA","knowing FA"))))
chisq.test(pdata$FA.Kit.,pdata$Know.FA)
summary(pdata$X1)
summary(pdata$X2)
summary(pdata$X3)
summary(pdata$X4)
summary(pdata$X5)
summary(pdata$X6)
summary(pdata$X7)
summary(pdata$X8)
summary(pdata$X9)
summary(pdata$X10)
options(digits = 4)

Healthcareset=pdata[pdata$Occupation.by.Healthcare=="H",]
others=pdata[pdata$Occupation.by.Healthcare=="others",]
mean(Healthcareset$overall.correction.rate)
mean(others$overall.correction.rate)
t.test(Healthcareset$overall.correction.rate,others$overall.correction.rate)
#chisq.test(pdata$Occupation.by.Healthcare,pdata$overall.correction.rate)
chisq.test(pdata$Occupation.by.Healthcare,pdata$X2)
length(which(Healthcareset$X2=="correct"))/length(Healthcareset$X2)
length(which(others$X2=="correct"))/length(others$X2)
table(pdata$Occupation.by.Healthcare,pdata$X2)
chisq.test(pdata$Occupation.by.Healthcare,pdata$X10)
length(which(Healthcareset$X10=="correct"))/length(Healthcareset$X10)
length(which(others$X10=="correct"))/length(others$X10)

HSO=pdata[pdata$Occupation.by.HSO=="HSO",]
others2=pdata[pdata$Occupation.by.HSO=="others",]
chisq.test(pdata$Occupation.by.HSO,pdata$X10)
length(which(HSO$X10=="correct"))/length(HSO$X10)
length(which(others2$X10=="correct"))/length(others2$X10)

########### medical knowledge vs non set ###########
Noneset=pdata[pdata$Level.of.Training=="none",]
CFB=pdata[pdata$Level.of.Training=="CPR/FA/basic",]
FAonlyset=pdata[pdata$level.of.training2=="FA only",]
othersmed=pdata[pdata$level.of.training2=="others",]
Advset=pdata[pdata$Level.of.Training=="Advanced",]
medknowset=rbind(CFB,FAonlyset,othersmed,Advset)
someset=rbind(CFB,FAonlyset,othersmed)
####### on average med knowledge vs non####
t.test(Noneset$overall.correction.rate,medknowset$overall.correction.rate)
t.test(someset$overall.correction.rate,medknowset$overall.correction.rate)
######## each question########
quecol=c(13,17,19,21,23,25,27,29,31)
for (i in quecol){
 pval_1= t.test(Noneset[,i],medknowset[,i])$p.value
 pval_2 = t.test(someset[,i],medknowset[,i])$p.value
 result=cbind(pval_1,pval_2)
 print(result)
}

for (i in quecol){
  mean1=mean(Noneset[,i],na.rm=T)*100
  mean2=mean(medknowset[,i],na.rm=T)*100
  mean3=mean(someset[,i],na.rm=T)*100
  mean4=mean(Advset[,i],na.rm=T)*100
  rest=cbind(mean1,mean2,mean3,mean4)
  print(rest)
}

mean(Noneset$overall.correction.rate,na.rm=TRUE)
mean(FAonlyset$overall.correction.rate,na.rm=TRUE)
mean(othersmed$overall.correction.rate,na.rm=TRUE)
mean(CFB$overall.correction.rate)
mean(Advset$overall.correction.rate)
#run the overall ANOVA among medical training groups
Medtrainaov = aov(pdata$overall.correction.rate~pdata$level.of.training2)
summary(Medtrainaov)
# separate comparasion
TukeyHSD(Medtrainaov)
chisq.test(pdata$level.of.training2,pdata$overall.correction.rate)


chisq.test(pdata$Level.of.Training,pdata$X10)
length(which(Noneset$X10=="correct"))/length(Noneset$X10)
length(which(CFB$X10=="correct"))/length(CFB$X10)
length(which(Advset$X10=="correct"))/length(Advset$X10)



