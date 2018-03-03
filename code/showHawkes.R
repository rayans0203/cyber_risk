lambda0=1;
alpha=1;
beta=3;
horizon=10;
a=simulateHawkes(lambda0,alpha,beta,horizon);
P=3; # nombre de pays

N_step=10000;
dt=horizon/N_step;

plotHawkes<-function(N,dt,a){
  n=length(a[[1]]);
  lambda=c();
  times=c();
  for (i in 1:N_step){
    t=i*dt;
    times[i]=t;
    j=1;
    intensity=0;
    while(a[[1]][j]<t && j<=n){
      intensity=intensity+alpha*exp(-beta*(t-a[[1]][j]));
      j=j+1;}
    lambda[[i]]=lambda0 + intensity;
  }
  plot(times,lambda,type="l",col="red")
}

plotHawkes(N_step,dt,a);

## Algo de mont??e de gradient stochastique

SGA<-function(){
  
}
