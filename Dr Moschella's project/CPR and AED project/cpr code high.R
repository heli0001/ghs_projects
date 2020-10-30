cprhigh=read.csv("CPR Survey high school recoded.csv",header=TRUE,sep=",")
######## descriptive tables ###########
options(digits=3)
descol=c(36,37,38,39)
for (i in descol){
  destab=(table(cprhigh[,i]))
  print(destab)
}

desbackcol=c(2,3,4,5)
for (i in desbackcol){
  destab=(table(cprhigh[,i]))
  print(destab)
}

############ concerns before and after #############
concol=c(15,17,19,21,23,25,27,29,31,33,35)
for (i in concol){
  destab=with(cprhigh,addmargins(table(cprhigh[,i])))
  print(destab)
}

cprhigh$knowledgecprbefore=NA
cprhigh$knowledgecprafter=NA
cprhigh$knowledgeAEDbefore=NA
cprhigh$knowledgeAEDafter=NA
cprhigh$knowledgecprbefore[cprhigh$Knowledge.of.hands.only.CPR..BEFORE.training.today.=="I could perform hands-only CPR CORRECTLY."]=6
cprhigh$knowledgecprbefore[cprhigh$Knowledge.of.hands.only.CPR..BEFORE.training.today.=="I could perform hands-only CPR PRETTY WELL."]=5
cprhigh$knowledgecprbefore[cprhigh$Knowledge.of.hands.only.CPR..BEFORE.training.today.=="I could PROBABLY perform hands-only CPR."]=4
cprhigh$knowledgecprbefore[cprhigh$Knowledge.of.hands.only.CPR..BEFORE.training.today.=="I need A BRIEF REVIEW of the training."]=3
cprhigh$knowledgecprbefore[cprhigh$Knowledge.of.hands.only.CPR..BEFORE.training.today.=="I need A LITTLE more training."]=2
cprhigh$knowledgecprbefore[cprhigh$Knowledge.of.hands.only.CPR..BEFORE.training.today.=="I need A LOT more training."]=1

cprhigh$knowledgecprafter[cprhigh$Knowledge.of.hands.only.CPR..AFTER.training.today.=="I could perform hands-only CPR CORRECTLY."]=6
cprhigh$knowledgecprafter[cprhigh$Knowledge.of.hands.only.CPR..AFTER.training.today.=="I could perform hands-only CPR PRETTY WELL."]=5
cprhigh$knowledgecprafter[cprhigh$Knowledge.of.hands.only.CPR..AFTER.training.today.=="I could PROBABLY perform hands-only CPR."]=4
cprhigh$knowledgecprafter[cprhigh$Knowledge.of.hands.only.CPR..AFTER.training.today.=="I need A BRIEF REVIEW of the training."]=3
cprhigh$knowledgecprafter[cprhigh$Knowledge.of.hands.only.CPR..AFTER.training.today.=="I need A LITTLE more training."]=2
cprhigh$knowledgecprafter[cprhigh$Knowledge.of.hands.only.CPR..AFTER.training.today.=="I need A LOT more training."]=1

cprhigh$knowledgeAEDbefore[cprhigh$Knowledge.of.AED.use..BEFORE.training.today.=="I could use an AED CORRECTLY."]=6
cprhigh$knowledgeAEDbefore[cprhigh$Knowledge.of.AED.use..BEFORE.training.today.=="I could use an AED PRETTY WELL."]=5
cprhigh$knowledgeAEDbefore[cprhigh$Knowledge.of.AED.use..BEFORE.training.today.=="I could PROBABLY use an AED."]=4
cprhigh$knowledgeAEDbefore[cprhigh$Knowledge.of.AED.use..BEFORE.training.today.=="I need a BRIEF REVIEW of the training."]=3
cprhigh$knowledgeAEDbefore[cprhigh$Knowledge.of.AED.use..BEFORE.training.today.=="I need A LITTLE more training."]=2
cprhigh$knowledgeAEDbefore[cprhigh$Knowledge.of.AED.use..BEFORE.training.today.=="I need A LOT more training."]=1

cprhigh$knowledgeAEDafter[cprhigh$Knowledge.of.AED.use..AFTER.training.today.=="I could use an AED CORRECTLY."]=6
cprhigh$knowledgeAEDafter[cprhigh$Knowledge.of.AED.use..AFTER.training.today.=="I could use an AED PRETTY WELL."]=5
cprhigh$knowledgeAEDafter[cprhigh$Knowledge.of.AED.use..AFTER.training.today.=="I could PROBABLY use an AED."]=4
cprhigh$knowledgeAEDafter[cprhigh$Knowledge.of.AED.use..AFTER.training.today.=="I need a BRIEF REVIEW of the training."]=3
cprhigh$knowledgeAEDafter[cprhigh$Knowledge.of.AED.use..AFTER.training.today.=="I need A LITTLE more training."]=2
cprhigh$knowledgeAEDafter[cprhigh$Knowledge.of.AED.use..AFTER.training.today.=="I need A LOT more training."]=1

######### summary knowledge of training####
knowcol=c(46,47,48,49)
for (i in knowcol){
  hightab=with(cprhigh,addmargins(table(cprhigh[,i])))
  print (hightab)
}

###### test between middle and high#####
for (i in knowcol){
  j=i-21
  pval1=t.test(manne[,j],cprhigh[,i])$p.value
  tab=table(cprdata[,j])
  pval2=t.test(cprdata[,j],cprhigh[,i])$p.value
  resu=cbind(pval1,pval2)
  print(resu)
  print(tab)
}
####### test before and after ########
t.test(cprhigh[,46],cprhigh[,47])$p.value
t.test(cprhigh[,48],cprhigh[,49])$p.value

########### difference between before and after########
cprhigh$diffcpr=cprhigh[,47]-cprhigh[,46]
cprhigh$diffAED=cprhigh[,49]-cprhigh[,48]

#### compare difference middle vs high#######
t.test(cprdata$diffcpr,cprhigh$diffcpr)
t.test(cprdata$diffAED,cprhigh$diffAED)

sd(cprdata$diffcpr,na.rm=T)

sd(cprdata$diffAED,na.rm=T)

sd(cprhigh$diffcpr,na.rm=T)

sd(cprhigh$diffAED,na.rm=T)