#################################################################
#################################################################
############### original work back to 2018 ######################   
#################################################################
stu = read.csv("LET BUMS study data recode.csv",fill=TRUE,sep=",",header = TRUE)
total=matrix(-99,nrow=dim(stu)[1],ncol=14)
total=cbind(stu[,5],stu[,8],stu[,11],stu[,14],stu[,17],stu[,20],stu[,23],stu[,26],stu[,29],stu[,32],stu[,35],stu[,38],stu[,41],stu[,44])
stu$totalcorrection=as.vector(apply(total,1,mean))
totaldvt=matrix(-99,nrow=dim(stu)[1],ncol=15)
totaldvt=cbind(stu[,47],stu[,50],stu[,53],stu[,56],stu[,59],stu[,62],stu[,65],stu[,68],stu[,71],stu[,74],stu[,77],stu[,80],stu[,83],stu[,86],stu[,89])
stu$totalDVTcorrection=as.vector(apply(totaldvt,1,mean))
medstu=stu[stu$What.is.your.level.of.training.=="Medical student",]
resident=stu[stu$What.is.your.level.of.training.=="Emergency medicine resident",]
mr=rbind(medstu,resident)
physi=stu[stu$What.is.your.level.of.training.=="Emergency Medicine Attending",]

##### descriptive summary##############
despcol=c(2,90,91,92,93,94,95,96)
for (i in despcol){
  r1=table(stu[,i])
  print(r1)
}
############# test for each question of GB and DVT#############
GBqcol=c(5,8,11,14,17,20,23,26,29,32,35,38,41,44)
DVTqcol=c(47,50,56,59,62,65,68,71,74,77,80,83,86) ## without Q3 and Q15
for (i in GBqcol){
  mean1=mean(medstu[,i],na.rm=T)
  mean2=mean(resident[,i],na.rm=T)
  mean3=mean(physi[,i],na.rm=T)
  pval1=t.test(medstu[,i],resident[,i])$p.value
  pval2=t.test(medstu[,i],physi[,i])$p.value
  results=cbind(mean1,mean2,mean1,mean3,pval1,pval2)
  print(results)
}
for (i in DVTqcol){
  mean1=mean(medstu[,i],na.rm=T)
  mean2=mean(resident[,i],na.rm=T)
  mean3=mean(physi[,i],na.rm=T)
  pval1=t.test(medstu[,i],resident[,i])$p.value
  pval2=t.test(medstu[,i],physi[,i])$p.value
  results=cbind(mean1,mean2,mean1,mean3,pval1,pval2)
  print(results)
}

########### overall correction ###############
#######GB##########
  allmean1=mean(medstu[,97],na.rm=T)
  allmean2=mean(resident[,97],na.rm=T)
  allmean3=mean(physi[,97],na.rm=T)
  pval1=t.test(medstu[,97],resident[,97])$p.value
  pval2=t.test(medstu[,97],physi[,97])$p.value
  results1=cbind(allmean1,allmean2,allmean1,allmean3,pval1,pval2)
  print(results1)
#########DVT###########
  allmean1=mean(medstu[,98],na.rm=T)
  allmean2=mean(resident[,98],na.rm=T)
  allmean3=mean(physi[,98],na.rm=T)
  pval1=t.test(medstu[,98],resident[,98])$p.value
  pval2=t.test(medstu[,98],physi[,98])$p.value
  results2=cbind(allmean1,allmean2,allmean1,allmean3,pval1,pval2)
  print(results2)
  
##########################################################
####################### updated work #####################
##########################################################
setwd("C:/Users/emmal/Desktop/2018 GHS/Dr Moschella's project")
ult = read.csv("LET BUMS study Responses.csv",fill=TRUE,sep=",",header = TRUE)
ult$medstu = as.numeric(ifelse(ult[,62]=='First year',-4,ifelse(ult[,62]=='Second year',-3,ifelse(ult[,62]=='Third year',-2,ifelse(ult[,62]=='Fourth year',-1,0)))))
ult$resi = as.numeric(ifelse(ult[,66]=='PGY 1',1,ifelse(ult[,66]=='PGY 2',2,0)))
ult$atten = ifelse(ult[,68]=='<3 years',5,ifelse(ult[,68]=='3-5 years',6,ifelse(ult[,68]=='5-10 years',9,ifelse(ult[,68]=='>10 years',14,0))))
ult$PGY = ult$medstu+ult$resi+ult$atten
ult$conf = (ult[,63]-min(ult[,63]))/diff(range(ult[,63]))

####### each question correct percentage for GB and DVT
GBdata = ult[,c(1:31,62:78)]
cw_ind = ult[,seq(5,31,2)]
correct_anw = sapply(cw_ind,unique)[1,]
for (i in 1:14){
  j=2*i+2
  GBdata[[paste("gb",i)]]=ifelse(GBdata[,j]==correct_anw[i],1,0)
}
for (i in 49:62){
m1=mean(GBdata[GBdata$PGY==-4,i])
m2=mean(GBdata[GBdata$PGY==-3,i])
m3=mean(GBdata[GBdata$PGY==-2,i])
m4=mean(GBdata[GBdata$PGY==-1,i])
p1=mean(GBdata[GBdata$PGY==1,i])
p2=mean(GBdata[GBdata$PGY==2,i])
less3=mean(GBdata[GBdata$PGY==5,i])
three5=mean(GBdata[GBdata$PGY==6,i])
five10=mean(GBdata[GBdata$PGY==9,i])
ge10=mean(GBdata[GBdata$PGY==14,i])
cf1=mean(GBdata[GBdata$PGY==-4,48])
cf2=mean(GBdata[GBdata$PGY==-3,48])
cf3=mean(GBdata[GBdata$PGY==-2,48])
cf4=mean(GBdata[GBdata$PGY==-1,48])
cf5=mean(GBdata[GBdata$PGY==1,48])
cf6=mean(GBdata[GBdata$PGY==2,48])
cf7=mean(GBdata[GBdata$PGY==5,48])
cf8=mean(GBdata[GBdata$PGY==6,48])
cf9=mean(GBdata[GBdata$PGY==9,48])
cf10=mean(GBdata[GBdata$PGY==14,48])
png(filename=paste("C:/Users/emmal/Desktop/2018 GHS/Dr Moschella's project/LET plots/","GB",i-48,".png"))
br = barplot(names.arg=c("MS1","MS2","MS3","MS4","PGY1","PGY2","<3yrs","3-5yrs","5-10yrs",">10yrs"),c(m1,m2,m3,m4,p1,p2,less3,three5,five10,ge10),ylim=c(0,1),xlab="PGY",ylab=paste("correct percent for GB",i-48))
points(x=br, y=c(cf1,cf2,cf3,cf4,cf5,cf6,cf7,cf8,cf9,cf10))
dev.off()
}


DVTdata = ult[,c(1:3,32:78)]
dvt_ind = ult[,seq(33,61,2)]
correct_dvt = sapply(dvt_ind,unique)[1,]
for (i in 1:15){
  j=2*i+2
  DVTdata[[paste("dvt",i)]]=ifelse(DVTdata[,j]==correct_dvt[i],1,0)
}
for (i in 51:65){
  m1=mean(DVTdata[DVTdata$PGY==-4,i])
  m2=mean(DVTdata[DVTdata$PGY==-3,i])
  m3=mean(DVTdata[DVTdata$PGY==-2,i])
  m4=mean(DVTdata[DVTdata$PGY==-1,i])
  p1=mean(DVTdata[DVTdata$PGY==1,i])
  p2=mean(DVTdata[DVTdata$PGY==2,i])
  less3=mean(DVTdata[DVTdata$PGY==5,i])
  three5=mean(DVTdata[DVTdata$PGY==6,i])
  five10=mean(DVTdata[DVTdata$PGY==9,i])
  ge10=mean(DVTdata[DVTdata$PGY==14,i])
  cf1=mean(DVTdata[DVTdata$PGY==-4,50])
  cf2=mean(DVTdata[DVTdata$PGY==-3,50])
  cf3=mean(DVTdata[DVTdata$PGY==-2,50])
  cf4=mean(DVTdata[DVTdata$PGY==-1,50])
  cf5=mean(DVTdata[DVTdata$PGY==1,50])
  cf6=mean(DVTdata[DVTdata$PGY==2,50])
  cf7=mean(DVTdata[DVTdata$PGY==5,50])
  cf8=mean(DVTdata[DVTdata$PGY==6,50])
  cf9=mean(DVTdata[DVTdata$PGY==9,50])
  cf10=mean(DVTdata[DVTdata$PGY==14,50])
  png(filename=paste("C:/Users/emmal/Desktop/2018 GHS/Dr Moschella's project/LET plots/","DVT",i-50,".png"))
  br = barplot(names.arg=c("MS1","MS2","MS3","MS4","PGY1","PGY2","<3yrs","3-5yrs","5-10yrs",">10yrs"),c(m1,m2,m3,m4,p1,p2,less3,three5,five10,ge10),ylim=c(0,1),xlab="PGY",ylab=paste("correct percent for DVT",i-50))
  points(x=br, y=c(cf1,cf2,cf3,cf4,cf5,cf6,cf7,cf8,cf9,cf10))
  dev.off()
}

######## overall correct for GB and DVT
GBdata$overallcorrect = rowSums(GBdata[,49:62])/14
  m1=mean(GBdata[GBdata$PGY==-4,63])
  m2=mean(GBdata[GBdata$PGY==-3,63])
  m3=mean(GBdata[GBdata$PGY==-2,63])
  m4=mean(GBdata[GBdata$PGY==-1,63])
  p1=mean(GBdata[GBdata$PGY==1,63])
  p2=mean(GBdata[GBdata$PGY==2,63])
  less3=mean(GBdata[GBdata$PGY==5,63])
  three5=mean(GBdata[GBdata$PGY==6,63])
  five10=mean(GBdata[GBdata$PGY==9,63])
  ge10=mean(GBdata[GBdata$PGY==14,63])
  png(filename=paste("C:/Users/emmal/Desktop/2018 GHS/Dr Moschella's project/LET plots/","overall_GB",".png"))
  br = barplot(names.arg=c("MS1","MS2","MS3","MS4","PGY1","PGY2","<3yrs","3-5yrs","5-10yrs",">10yrs"),c(m1,m2,m3,m4,p1,p2,less3,three5,five10,ge10),ylim=c(0,1),xlab="PGY",ylab="overall correct percent for GB")
  dev.off()
  
DVTdata$overallcorrect = rowSums(DVTdata[,51:65])/15
m1=mean(DVTdata[DVTdata$PGY==-4,66])
m2=mean(DVTdata[DVTdata$PGY==-3,66])
m3=mean(DVTdata[DVTdata$PGY==-2,66])
m4=mean(DVTdata[DVTdata$PGY==-1,66])
p1=mean(DVTdata[DVTdata$PGY==1,66])
p2=mean(DVTdata[DVTdata$PGY==2,66])
less3=mean(DVTdata[DVTdata$PGY==5,66])
three5=mean(DVTdata[DVTdata$PGY==6,66])
five10=mean(DVTdata[DVTdata$PGY==9,66])
ge10=mean(DVTdata[DVTdata$PGY==14,66])
png(filename=paste("C:/Users/emmal/Desktop/2018 GHS/Dr Moschella's project/LET plots/","overall_DVT",".png"))
br = barplot(names.arg=c("MS1","MS2","MS3","MS4","PGY1","PGY2","<3yrs","3-5yrs","5-10yrs",">10yrs"),c(m1,m2,m3,m4,p1,p2,less3,three5,five10,ge10),ylim=c(0,1),xlab="PGY",ylab="overall correct percent for DVT")
dev.off()