jobsa1=as.data.frame(read.csv("PatientSitterData.csv",header=TRUE,sep=","))
jobsa=jobsa1[jobsa1$my_first_instrument_complete==2,]
######################################
######## Table 1 descriptive #########
######################################
options(digits=4)
for (i in c(4,5,6,7)){
  re1=table(jobsa[,i])
  re2=prop.table(table(jobsa[,i]))
  su=rbind(re1,re2)
  print (su)
}

# mean and sd for each question
mean_summary=as.matrix(apply(jobsa[,8:28],2,mean,na.rm=TRUE),byrow=FALSE)
sd_summary=as.matrix(apply(jobsa[,8:28],2,sd,na.rm=TRUE),byrow=FALSE)
min_summary=as.matrix(apply(jobsa[,8:28],2,min,na.rm=TRUE),byrow=FALSE)
max_summary=as.matrix(apply(jobsa[,8:28],2,max,na.rm=TRUE),byrow=FALSE)
cbind(mean_summary,sd_summary,min_summary,max_summary)

## for eac question
q1age=matrix(-99,nrow=4,ncol=1)
q1role=matrix(-99,nrow=4,ncol=1)
q1shift=matrix(-99,nrow=4,ncol=1)
for (i in 1:4){
q1age[i,]=mean(jobsa[jobsa$age==i,]$Q3,na.rm=TRUE)
q1role[i,]=mean(jobsa[jobsa$role==i,]$Q3,na.rm=TRUE)
q1shift[i,]=mean(jobsa[jobsa$shif==i,]$Q3,na.rm=TRUE)
}
q1age
q1role
q1shift

### Male with Female###
mdata=jobsa[jobsa$gender==2,]
fdata=jobsa[jobsa$gender==1,]

#####################################
######## Table 2 ####################
######################################
library(psych)
library(coefficientalpha)
library(psy)
#### find the Cronbach's alpha under psy
cronbach(jobsa[,8:28])
cronbach(jobsa[,cow])
cronbach(jobsa[,wplace])
cronbach(jobsa[,wc])
cronbach(jobsa[,ws])
cronbach(jobsa[,tra])
cronbach(jobsa[,re])
cronbach(jobsa[,qoc])
cronbach(jobsa[,gr])

cow=c(10,12,13) #coworkers subscale
wplace=c(15,16,21) #work place support
wc=c(17,18,19) #work content
ws=c(11,20,22) #work schedule
tra=c(23,14,24) #training
re=c(8,9) #rewards
xr=jobsa[,8]+jobsa[,9]
qoc=c(25,26) #quality of care
xq=jobsa[,25]+jobsa[,26]
gr=c(27,28) #global rating
xg=jobsa[,27]+jobsa[,28]

# get warnings when under psych use alpha 
# using the formula for calculation 
alpha6=2/1*(1-(var(jobsa[,8],na.rm=T)+var(jobsa[,9],na.rm=T))/var(xr,na.rm=T))
alpha7=2/1*(1-(var(jobsa[,25],na.rm=T)+var(jobsa[,26],na.rm=T))/var(xq,na.rm=T))
alpha8=2/1*(1-(var(jobsa[,27],na.rm=T)+var(jobsa[,28],na.rm=T))/var(xg,na.rm=T))

#### find subscale means ######
subavg1=sum(jobsa[,cow],na.rm=T)/sum(!is.na(jobsa[,cow])) 
subavg2=sum(jobsa[,wplace],na.rm=T)/sum(!is.na(jobsa[,wplace]))
subavg3=sum(jobsa[,wc],na.rm=T)/sum(!is.na(jobsa[,wc]))
subavg4=sum(jobsa[,ws],na.rm=T)/sum(!is.na(jobsa[,ws]))
subavg5=sum(jobsa[,tra],na.rm=T)/sum(!is.na(jobsa[,tra]))
subavg6=sum(jobsa[,re],na.rm=T)/sum(!is.na(jobsa[,re]))
subavg7=sum(jobsa[,qoc],na.rm=T)/sum(!is.na(jobsa[,qoc]))
subavg8=sum(jobsa[,gr],na.rm=T)/sum(!is.na(jobsa[,gr]))
rbind(subavg1,subavg2,subavg3,subavg4,subavg5,subavg6,subavg7,subavg8)
#### find each question average ####
subcode=seq(from=8,to=28,by=1)
sum_sub=matrix(-99,nrow=21,ncol=4)
for (i in subcode) {
  j=i-7
 avg=mean(jobsa[,i],na.rm=T)
 sd_sub=sd(jobsa[,i],na.rm=T)
 min_sub=min(jobsa[,i],na.rm=T)
 max_sub=max(jobsa[,i],na.rm=T)
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
  mean1=t.test(jobsa[jobsa$role==1,i],jobsa[jobsa$role==2,i])$estimate[1]
  mean2=t.test(jobsa[jobsa$role==1,i],jobsa[jobsa$role==2,i])$estimate[2]
  pvalue=t.test(jobsa[jobsa$role==1,i],jobsa[jobsa$role==2,i])$p.value
  res1[j,]=cbind(mean1,mean2,pvalue)
}
### for shift ###
res2=matrix(-99,nrow=21,ncol=3)
for (i in ques){
  j=i-7
  mean1=t.test(jobsa[jobsa$shif==1,i],jobsa[jobsa$shif==2 |jobsa$shif==3,i])$estimate[1]
  mean2=t.test(jobsa[jobsa$shif==1,i],jobsa[jobsa$shif==2 |jobsa$shif==3,i])$estimate[2]
  pvalue=t.test(jobsa[jobsa$shif==1,i],jobsa[jobsa$shif==2 |jobsa$shif==3,i])$p.value
  res2[j,]=cbind(mean1,mean2,pvalue)
}
res2
### for age ###
res=matrix(-99,nrow=21,ncol=3)
for (i in ques){
  j=i-7
  mean1=t.test(jobsa[jobsa$age==1|jobsa$age==2,i],jobsa[jobsa$age==3 |jobsa$age==4,i])$estimate[1]
  mean2=t.test(jobsa[jobsa$age==1|jobsa$age==2,i],jobsa[jobsa$age==3 |jobsa$age==4,i])$estimate[2]
  pvalue=t.test(jobsa[jobsa$age==1|jobsa$age==2,i],jobsa[jobsa$age==3 |jobsa$age==4,i])$p.value
  res[j,]=cbind(mean1,mean2,pvalue)
}
print (res)
