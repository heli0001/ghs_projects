# for TC
x=c(1,1,1,2,2,2,3,3,3,4,4,4)
y=c(155.9,153.3,158.6,168.8,167.4,170.1,167.6,165,170.2,174.5,173.1,175.9)
plot(x,y,xlim=c(1,8),xaxt="n")
axis(1,at=1:4,labels=c("m","m","f","f"))
segments(x0=1, x1=1, y0=y[2],y1=y[3], col='blue')
for (i in c(2,3,4)){
  j=3*i-1
  if (i%%2==0){
    segments(x0=i, x1=i, y0=y[j],y1=y[j+1], col='red')
  } 
  else {
    segments(x0=i, x1=i, y0=y[j],y1=y[j+1], col='blue')
  }
}