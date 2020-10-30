cprdata=read.csv("CPR Training in Middle Schools recode.csv",header=TRUE,sep=",")

################ frequency analysis #############
sum(cprdata$has_responded == "True")
summary(cprdata$has_responded)
options(digits=4)
prop.table(table(cprdata$I.am))
## demographic descriptive
with(cprdata,addmargins(table(cprdata$My.age.is.)))
with(cprdata,addmargins(table(cprdata$What.is.the.primary.language.spoken.in.your.home..)))
prop.table(table(cprdata$What.is.the.primary.language.spoken.in.your.home..))
with(cprdata,addmargins(table(cprdata$Is.there.anyone.living.in.your.home.older.than.60.years.of.age.)))
prop.table(table(cprdata$Is.there.anyone.living.in.your.home.older.than.60.years.of.age.))

## general background information
with(cprdata,addmargins(table(cprdata$Which.device.did.you.use.to.learn.CPR.)))
prop.table(table(cprdata$Which.device.did.you.use.to.learn.CPR.))
with(cprdata,addmargins(table(cprdata$Before.today..have.you.ever.taken.a.class.or.been.taught.to.perform.CPR.)))
prop.table(table(cprdata$Before.today..have.you.ever.taken.a.class.or.been.taught.to.perform.CPR.))
with(cprdata,addmargins(table(cprdata$Have.you.ever.performed.CPR.on.a.person.in.an.emergency..)))
prop.table(table(cprdata$Have.you.ever.performed.CPR.on.a.person.in.an.emergency..))
with(cprdata,addmargins(table(cprdata$Before.today..have.you.ever.taken.a.class.or.been.taught.to.use.an.Automated.External.Defibrillator..AED...)))
with(cprdata,addmargins(table(cprdata$Have.you.ever.used.an.AED.on.a.person.in.an.emergency.)))

## pre and post summary
mean(cprdata$before.I.believe.my.family.should.be.trained.in.hands.only.CPR.and.AED.use,na.rm=TRUE)
sd(cprdata$before.I.believe.my.family.should.be.trained.in.hands.only.CPR.and.AED.use,na.rm=TRUE)
mean(cprdata$after.I.believe.my.family.should.be.trained.in.hands.only.CPR.and.AED.use,na.rm=TRUE)
sd(cprdata$after.I.believe.my.family.should.be.trained.in.hands.only.CPR.and.AED.use,na.rm=TRUE)
mean(cprdata$before.How.interested.are.you.in.a.career.in.healthcare.,na.rm=TRUE)
sd(cprdata$before.How.interested.are.you.in.a.career.in.healthcare.,na.rm=TRUE)
mean(cprdata$before.Knowledge.of.hands.only.CPR,na.rm=TRUE)
sd(cprdata$before.Knowledge.of.hands.only.CPR,na.rm=TRUE)
mean(cprdata$after.How.interested.are.you.in.a.career.in.healthcare.,na.rm=TRUE)
sd(cprdata$after.How.interested.are.you.in.a.career.in.healthcare.,na.rm=TRUE)
mean(cprdata$after.Knowledge.of.hands.only.CPR,na.rm=TRUE)
sd(cprdata$after.Knowledge.of.hands.only.CPR,na.rm=TRUE)
mean(cprdata$before.Knowledge.of.AED.use,na.rm=TRUE)
sd(cprdata$before.Knowledge.of.AED.use,na.rm=TRUE)
mean(cprdata$after.Knowledge.of.AED.use,na.rm=TRUE)
sd(cprdata$after.Knowledge.of.AED.use,na.rm=TRUE)

with(cprdata,addmargins(table(cprdata$Where.should.you.put.your.hands.to.press.on.the.chest..)))
prop.table(table(cprdata$Where.should.you.put.your.hands.to.press.on.the.chest..))
mean(cprdata$Where.should.you.put.your.hands.to.press.on.the.chest..,na.rm=TRUE)
sd(cprdata$Where.should.you.put.your.hands.to.press.on.the.chest..,na.rm=TRUE)
mean(cprdata$What.are.the.correct.steps.for.providing.Adult.Hands.Only.CPR.,na.rm=TRUE)
sd(cprdata$What.are.the.correct.steps.for.providing.Adult.Hands.Only.CPR.,na.rm=TRUE)
with(cprdata,addmargins(table(cprdata$What.are.the.correct.steps.for.providing.Adult.Hands.Only.CPR.)))
mean(cprdata$When.using.an.AED.you.should.,na.rm=TRUE)
sd(cprdata$When.using.an.AED.you.should.,na.rm=TRUE)
table(cprdata$Is.it.better.to.do.any.CPR.than.to.do.no.CPR..)
mean(cprdata$How.do.you.check.a.person.for.a.response..,na.rm=TRUE)
sd(cprdata$How.do.you.check.a.person.for.a.response..,na.rm=TRUE)
table(cprdata$I.might.do.it.wrong....BEFORE.training.today.,cprdata$I.might.do.it.wrong....AFTER.training.today.)

############### concerns#############
table(cprdata$I.should.just.wait.for.EMS....BEFORE.training.today.)
table(cprdata$I.should.just.wait.for.EMS....AFTER.training.today.)
both1ems=cprdata[cprdata$I.should.just.wait.for.EMS....BEFORE.training.today.==1 & cprdata$I.should.just.wait.for.EMS....AFTER.training.today.==2,]
table(both1$I.should.just.wait.for.EMS....BEFORE.training.today.)
both2=cprdata[cprdata$I.might.do.it.wrong....BEFORE.training.today.&cprdata$I.might.do.it.wrong....AFTER.training.today.,]
table(both2$I.might.do.it.wrong....BEFORE.training.today.)

############ Analysis###########
manne=cprdata[cprdata$Which.device.did.you.use.to.learn.CPR.==1,]
pump=cprdata[cprdata$Which.device.did.you.use.to.learn.CPR.==2,]
### prior ######
priorcol=c(12,13,14,15,61,62)
for (i in priorcol){
  mannetab=with(manne,addmargins(table(manne[,i])))
  pumptab=with(pump,addmargins(table(pump[,i])))
  result=cbind(mannetab,pumptab)
  print(result)
}
with(manne,addmargins(table(manne$Before.today..have.you.ever.taken.a.class.or.been.taught.to.perform.CPR.)))
with(pump,addmargins(table(pump$Before.today..have.you.ever.taken.a.class.or.been.taught.to.perform.CPR.)))
with(manne,addmargins(table(manne$Have.you.ever.performed.CPR.on.a.person.in.an.emergency..)))
with(pump,addmargins(table(pump$Have.you.ever.performed.CPR.on.a.person.in.an.emergency..)))
with(manne,addmargins(table(manne$Before.today..have.you.ever.taken.a.class.or.been.taught.to.use.an.Automated.External.Defibrillator..AED...)))
with(pump,addmargins(table(pump$Before.today..have.you.ever.taken.a.class.or.been.taught.to.use.an.Automated.External.Defibrillator..AED...)))
with(manne,addmargins(table(manne$Have.you.ever.used.an.AED.on.a.person.in.an.emergency.)))
with(pump,addmargins(table(pump$Have.you.ever.used.an.AED.on.a.person.in.an.emergency.)))
with(manne,addmargins(table(manne$I.am)))
with(pump,addmargins(table(pump$I.am)))
with(manne,addmargins(table(manne$My.age.is.)))
with(pump,addmargins(table(pump$My.age.is.)))
###### quiz q8-q12 summary##

## recode as true or false###
cprdata$newq8=NA
cprdata$newq9=NA
cprdata$newq10=NA
cprdata$newq11=NA
cprdata$newq12=NA
cprdata$newq8[cprdata$Where.should.you.put.your.hands.to.press.on.the.chest..==4]="correct"
cprdata$newq8[cprdata$Where.should.you.put.your.hands.to.press.on.the.chest..<4]="wrong"
cprdata$newq9[cprdata$What.are.the.correct.steps.for.providing.Adult.Hands.Only.CPR.==1]="correct"
cprdata$newq9[cprdata$What.are.the.correct.steps.for.providing.Adult.Hands.Only.CPR.>1]="wrong"
cprdata$newq10[cprdata$When.using.an.AED.you.should.==1]="correct"
cprdata$newq10[cprdata$When.using.an.AED.you.should.>1]="wrong"
cprdata$newq11[cprdata$Is.it.better.to.do.any.CPR.than.to.do.no.CPR..==1]="correct"
cprdata$newq11[cprdata$Is.it.better.to.do.any.CPR.than.to.do.no.CPR..>1]="wrong"
cprdata$newq12[cprdata$How.do.you.check.a.person.for.a.response..==3]="correct"
cprdata$newq12[cprdata$How.do.you.check.a.person.for.a.response..==c(1,2)]="wrong"
########## summary for two divices##########
testcol=c(65,66,67,68,69)
for (i in testcol){
  mannetab=with(manne,addmargins(table(manne[,i])))
  pumptab=with(pump,addmargins(table(pump[,i])))
  print.table(mannetab)
  print.table(pumptab)
}
with(manne,addmargins(table(manne$Where.should.you.put.your.hands.to.press.on.the.chest..)))
with(pump,addmargins(table(pump$Where.should.you.put.your.hands.to.press.on.the.chest..)))
with(manne,addmargins(table(manne$What.are.the.correct.steps.for.providing.Adult.Hands.Only.CPR.)))
with(pump,addmargins(table(pump$What.are.the.correct.steps.for.providing.Adult.Hands.Only.CPR.)))
with(manne,addmargins(table(manne$When.using.an.AED.you.should.)))
with(pump,addmargins(table(pump$When.using.an.AED.you.should.)))
with(manne,addmargins(table(manne$Is.it.better.to.do.any.CPR.than.to.do.no.CPR..)))
with(pump,addmargins(table(pump$Is.it.better.to.do.any.CPR.than.to.do.no.CPR..)))
with(manne,addmargins(table(manne$How.do.you.check.a.person.for.a.response..)))
with(pump,addmargins(table(pump$How.do.you.check.a.person.for.a.response..)))
#########chisq.test q8-q12 #####
for (i in testcol){
  pvalues=chisq.test(cprdata[,i],cprdata$Which.device.did.you.use.to.learn.CPR.)$p.value
  print (pvalues)
}

############ comfort of CPR/AED###
knowcol=c(25,26,27,28)
for (i in knowcol){
  pumptab=with(pump,addmargins(table(pump[,i])))
  mannetab=with(manne,addmargins(table(manne[,i])))
  pval=t.test(pump[,i],manne[,i])$p.value
  print(pumptab)
  print(mannetab)
  print (pval)
  
}
#### comfort test before and after###
t.test(pump[,25],pump[,26])$p.value
t.test(manne[,25],manne[,26])$p.value
t.test(pump[,27],pump[,28])$p.value
t.test(manne[,27],manne[,28])$p.value
######## compute the difference of before and after#####
cprdata$diffcpr=cprdata[,26]-cprdata[,25]
cprdata$diffAED=cprdata[,28]-cprdata[,27]
