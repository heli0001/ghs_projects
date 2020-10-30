rm(list=ls())
setwd("C:/Users/emmal/Desktop/2018 GHS/1-Patient observer project")
jobsa_all=as.data.frame(read.csv("Updated patient sitter data.csv",header=TRUE,sep=","))
jobsapos=jobsa_all[jobsa_all$groups=="post",] 
jobsapre=jobsa_all[jobsa_all$groups=="pre",]
jobdanew_pos = jobsapos[jobsapos$my_first_instrument_complete==2,]
jobdanew_pre = jobsapre[jobsapre$my_first_instrument_complete==2,]
jobsanew_whole=list(jobdanew_pre,jobdanew_pos)
######################################
######## Table 1 descriptive #########
######################################
jobsanew = jobsanew_whole[[2]][,-4] #pre and post seperately
options(digits=4)
for (i in c(4,5,6,7)){
  re1=table(jobsanew[,i])
  re2=prop.table(table(jobsanew[,i]))
  su=rbind(re1,re2)
  print (su)
}

# mean and sd for each question
mean_summary=as.matrix(apply(jobsanew[,8:28],2,mean,na.rm=TRUE),byrow=FALSE)
sd_summary=as.matrix(apply(jobsanew[,8:28],2,sd,na.rm=TRUE),byrow=FALSE)
min_summary=as.matrix(apply(jobsanew[,8:28],2,min,na.rm=TRUE),byrow=FALSE)
max_summary=as.matrix(apply(jobsanew[,8:28],2,max,na.rm=TRUE),byrow=FALSE)
cbind(mean_summary,sd_summary,min_summary,max_summary)

### Male with Female###
mdata=jobsanew[jobsanew$gender==2,]
fdata=jobsanew[jobsanew$gender==1,]

#####################################
######## Table 2 ####################
######################################
library(psych)
library(coefficientalpha)
library(psy)
#### find the Cronbach's alpha under psy
cronbach(jobsanew[,8:28])
cronbach(jobsanew[,cow])
cronbach(jobsanew[,wplace])
cronbach(jobsanew[,wc])
cronbach(jobsanew[,ws])
cronbach(jobsanew[,tra])
cronbach(jobsanew[,re])
cronbach(jobsanew[,qoc])
cronbach(jobsanew[,gr])

cow=c(10,12,13) #coworkers subscale
wplace=c(15,16,21) #work place support
wc=c(17,18,19) #work content
ws=c(11,20,22) #work schedule
tra=c(23,14,24) #training
re=c(8,9) #rewards
xr=jobsanew[,8]+jobsanew[,9]
qoc=c(25,26) #quality of care
xq=jobsanew[,25]+jobsanew[,26]
gr=c(27,28) #global rating
xg=jobsanew[,27]+jobsanew[,28]

# get warnings when under psych use alpha 
# using the formula for calculation 
alpha6=2/1*(1-(var(jobsanew[,8],na.rm=T)+var(jobsanew[,9],na.rm=T))/var(xr,na.rm=T))
alpha7=2/1*(1-(var(jobsanew[,25],na.rm=T)+var(jobsanew[,26],na.rm=T))/var(xq,na.rm=T))
alpha8=2/1*(1-(var(jobsanew[,27],na.rm=T)+var(jobsanew[,28],na.rm=T))/var(xg,na.rm=T))

#### find subscale means ######
subavg1=sum(jobsanew[,cow],na.rm=T)/sum(!is.na(jobsanew[,cow])) 
subavg2=sum(jobsanew[,wplace],na.rm=T)/sum(!is.na(jobsanew[,wplace]))
subavg3=sum(jobsanew[,wc],na.rm=T)/sum(!is.na(jobsanew[,wc]))
subavg4=sum(jobsanew[,ws],na.rm=T)/sum(!is.na(jobsanew[,ws]))
subavg5=sum(jobsanew[,tra],na.rm=T)/sum(!is.na(jobsanew[,tra]))
subavg6=sum(jobsanew[,re],na.rm=T)/sum(!is.na(jobsanew[,re]))
subavg7=sum(jobsanew[,qoc],na.rm=T)/sum(!is.na(jobsanew[,qoc]))
subavg8=sum(jobsanew[,gr],na.rm=T)/sum(!is.na(jobsanew[,gr]))
rbind(subavg1,subavg2,subavg3,subavg4,subavg5,subavg6,subavg7,subavg8)
#### find each question average ####
subcode=seq(from=8,to=28,by=1)
sum_sub=matrix(-99,nrow=21,ncol=4)
for (i in subcode) {
  j=i-7
  avg=mean(jobsanew[,i],na.rm=T)
  sd_sub=sd(jobsanew[,i],na.rm=T)
  min_sub=min(jobsanew[,i],na.rm=T)
  max_sub=max(jobsanew[,i],na.rm=T)
  sum_sub[j,]=cbind(avg,sd_sub,min_sub,max_sub)
}
print (sum_sub)

############## table 3 ###########
##################################
### for role ###
ques=seq(8,28,by=1)
res1=matrix(-99,nrow=21,ncol=3)
for (i in ques){
  j=i-7
  mean1=t.test(jobsanew[jobsanew$role==1,i],jobsanew[jobsanew$role==2,i])$estimate[1]
  mean2=t.test(jobsanew[jobsanew$role==1,i],jobsanew[jobsanew$role==2,i])$estimate[2]
  pvalue=t.test(jobsanew[jobsanew$role==1,i],jobsanew[jobsanew$role==2,i])$p.value
  res1[j,]=cbind(mean1,mean2,pvalue)
}
### for shift ###
res2=matrix(-99,nrow=21,ncol=3)
for (i in ques){
  j=i-7
  mean1=t.test(jobsanew[jobsanew$shif==1,i],jobsanew[jobsanew$shif==2 |jobsanew$shif==3,i])$estimate[1]
  mean2=t.test(jobsanew[jobsanew$shif==1,i],jobsanew[jobsanew$shif==2 |jobsanew$shif==3,i])$estimate[2]
  pvalue=t.test(jobsanew[jobsanew$shif==1,i],jobsanew[jobsanew$shif==2 |jobsanew$shif==3,i])$p.value
  res2[j,]=cbind(mean1,mean2,pvalue)
}
res2
### for age ###
res=matrix(-99,nrow=21,ncol=3)
for (i in ques){
  j=i-7
  mean1=t.test(jobsanew[jobsanew$age==1|jobsanew$age==2,i],jobsanew[jobsanew$age==3 |jobsanew$age==4,i])$estimate[1]
  mean2=t.test(jobsanew[jobsanew$age==1|jobsanew$age==2,i],jobsanew[jobsanew$age==3 |jobsanew$age==4,i])$estimate[2]
  pvalue=t.test(jobsanew[jobsanew$age==1|jobsanew$age==2,i],jobsanew[jobsanew$age==3 |jobsanew$age==4,i])$p.value
  res[j,]=cbind(mean1,mean2,pvalue)
}
print (res)

#####################################################################
################ comparison within pre and post #####################
#####################################################################
### each question mean
res = matrix(-99,nrow=21,ncol=3)
for (i in 9:29){
  j=i-8
  res[j,1] = t.test(jobdanew_pre[,i],jobdanew_pos[,i])$estimate[1]
  res[j,2] = t.test(jobdanew_pre[,i],jobdanew_pos[,i])$estimate[2]
  res[j,3] = t.test(jobdanew_pre[,i],jobdanew_pos[,i])$p.value
  
}






jobsanew$prepost=2
jobsanew$prepost[1:57]='pre'
jobsanew$prepost[58:126]='post'
### each question mean
res = matrix(-99,nrow=21,ncol=3)
for (i in 8:28){
  j=i-7
  res[j,1] = t.test(jobsanew[jobsanew$prepost=='pre',i],jobsanew[jobsanew$prepost=='post',i])$estimate[1]
  res[j,2] = t.test(jobsanew[jobsanew$prepost=='pre',i],jobsanew[jobsanew$prepost=='post',i])$estimate[2]
  res[j,3] = t.test(jobsanew[jobsanew$prepost=='pre',i],jobsanew[jobsanew$prepost=='post',i])$p.value
  
}
### subscale mean 
t.test(jobsanew[jobsanew$prepost=='pre',cow],jobsanew[jobsanew$prepost=='post',cow])
t.test(jobsanew[jobsanew$prepost=='pre',wplace],jobsanew[jobsanew$prepost=='post',wplace])
t.test(jobsanew[jobsanew$prepost=='pre',wc],jobsanew[jobsanew$prepost=='post',wc])
t.test(jobsanew[jobsanew$prepost=='pre',ws],jobsanew[jobsanew$prepost=='post',ws])
t.test(jobsanew[jobsanew$prepost=='pre',tra],jobsanew[jobsanew$prepost=='post',tra])
t.test(jobsanew[jobsanew$prepost=='pre',re],jobsanew[jobsanew$prepost=='post',re])
t.test(jobsanew[jobsanew$prepost=='pre',qoc],jobsanew[jobsanew$prepost=='post',qoc])
t.test(jobsanew[jobsanew$prepost=='pre',gr],jobsanew[jobsanew$prepost=='post',gr])
