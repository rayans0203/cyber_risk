library(hawkes)
library(stats)

lambda0<-0.2
alpha<-0.2
beta<-0.7
history<-simulateHawkes(lambda0,alpha,beta,3600)
l<-likelihoodHawkes(lambda0,0.5,beta,history[[1]])
print(l);
LKHawkes<-function(alpha){
  return(likelihoodHawkes(lambda0,alpha,beta,history[[1]]));
}

#print(l)
#history<-simulateHawkes(0.3,0.6,0.7,3600);

alp=optimize(LKHawkes, c(0, 1), maximum = FALSE);

#print(LKHawkes(0.6));
#print(LKHawkes(0.4));
print(alp)

