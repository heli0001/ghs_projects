setwd("C:/Users/emmal/Desktop/GHS/Dr pittman project")
pdata = read.csv("rawdataedited2.csv",fill=TRUE,sep=",",header = TRUE)
###table 1 ####
mean(pdata$Age,na.rm=T)
sd(pdata$Age,na.rm=T)
table(pdata$Sex)
table(pdata$Occupation.Category)

####table 2####
table(pdata$Medical.Training,pdata$Sex)
table(pdata$FA.Kit.,pdata$Sex)
table(pdata$Know.FA,pdata$Sex)

### table 3 ######
dimnames(pdata)
corre=matrix(-99,nr=10,nc=2)
for (i in que) {
  t1=table(pdata[,i])
  print (t1)
}

### table 4 #####
mean(pdata$overall.correction.rate,na.rm=T)
carrykit=pdata[pdata$FA.Kit.=="yes",]
notcarry=pdata[pdata$FA.Kit.=="no",]
t.test(carrykit$overall.correction.rate,notcarry$overall.correction.rate)
chisq.test(pdata$overall.correction.rate,pdata$FA.Kit.)

### table 5 ####
Noneset=pdata[pdata$Level.of.Training=="none",]
CFB=pdata[pdata$Level.of.Training=="CPR/FA/basic",]
Advset=pdata[pdata$Level.of.Training=="Advanced",]

AdvNoneset=pdata[pdata$Level.of.Training=="Advanced" | pdata$Level.of.Training=="none",]
CFBNonset=pdata[pdata$Level.of.Training=="CPR/FA/basic" | pdata$Level.of.Training=="none",]
AdvCFBset=pdata[pdata$Level.of.Training=="Advanced" | pdata$Level.of.Training=="CPR/FA/basic",]
quecode=c(13,17, 19, 21, 23, 25, 27, 29 ,31)

for (i in quecode){
  n1=length(Noneset[,i])
  n2=sum(Noneset[,i],na.rm=T)
  n=cbind(n1,n2)
  print (n)
}

for (i in quecode){
  n1=length(Advset[,i])
  n2=sum(Advset[,i])
  n=cbind(n1,n2)
  print (n)
}

for (i in quecode){
  n1=length(CFB[,i])
  n2=sum(CFB[,i],na.rm=T)
  n=cbind(n1,n2)
  print (n)
}

for (i in quecode){
  mean1=mean(Noneset[,i],na.rm=T)*100
  mean2=mean(CFB[,i],na.rm=T)*100
  mean3=mean(Advset[,i],na.rm=T)*100
  rest=cbind(mean1,mean3,mean1,mean2,mean2,mean3)
  print(rest)
}
for (i in quecode){
r1=chisq.test(AdvNoneset$Level.of.Training,AdvNoneset[,i])$p.value
r2=chisq.test(CFBNonset$Level.of.Training,CFBNonset[,i])$p.value
r3=chisq.test(AdvCFBset$Level.of.Training,AdvCFBset[,i])$p.value
rcom=cbind(r1,r2,r3)
print (rcom)
}

mean(Noneset$overall.correction.rate,na.rm=T)
mean(CFB$overall.correction.rate,na.rm=T)
mean(Advset$overall.correction.rate,na.rm=T)
t.test(Noneset$overall.correction.rate,CFB$overall.correction.rate)$p.value
t.test(Noneset$overall.correction.rate,Advset$overall.correction.rate)$p.value
t.test(Advset$overall.correction.rate,CFB$overall.correction.rate)$p.value
chisq.test(AdvNoneset$overall.correction.rate,AdvNoneset$Medical.Training)
chisq.test(AdvCFBset$overall.correction.rate,AdvCFBset$Medical.Training)
chisq.test(CFBNonset$overall.correction.rate,CFBNonset$Medical.Training)
#### table 6 ####
HSO=pdata[pdata$Occupation.by.HSO=="HSO",]
others2=pdata[pdata$Occupation.by.HSO=="others",]

for (i in quecode) {
  n1=length(HSO[,i])
  n2=sum(HSO[,i],na.rm=T)
  n=cbind(n1,n2)
  print (n)
}

for (i in quecode) {
  n1=length(others2[,i])
  n2=sum(others2[,i],na.rm=T)
  n=cbind(n1,n2)
  print (n)
}

for (i in quecode){
  mean1=mean(HSO[,i],na.rm=T)*100
  mean2=mean(others2[,i],na.rm=T)*100 
  meancom=cbind(mean1,mean2)
  print (meancom)
}

for (i in quecode){
  r1=chisq.test(pdata$Occupation.by.HSO,pdata[,i])$p.value
  print (r1)
}

t.test(HSO$overall.correction.rate,others2$overall.correction.rate)
chisq.test(pdata$Occupation.by.HSO,pdata$overall.correction.rate)
