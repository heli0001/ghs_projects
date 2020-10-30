hiv=read.csv("HIV1.csv",header=T,sep=",")
hivdata=as.data.frame(hiv)
#### not considering positive row########
newdata=hiv[c(-1,-2,-13),]
newdatamodel=as.data.frame(newdata)

### question 1
plot(newdata$Jackie.Ordered.Test,newdata$ED.Providers..Ordered.Test)
chisq.test(newdata$Jackie.Ordered.Test,newdata$ED.Providers..Ordered.Test)
### question 1
reslm=lm(newdata$ED.Providers..Ordered.Test~newdata$Jackie.Ordered.Test,data=newdatamodel)
summary(reslm)

## not significant show that there is a relationship between J and ED.
