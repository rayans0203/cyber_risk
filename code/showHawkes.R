library(hawkes)
# param??tres arbitrairement fix??s
lambda0=c(0.2,0.2);
alpha=matrix(c(0.5,0,0,0.5),byrow=TRUE,nrow=2);
beta=c(0.7,0.7);
horizon=30;

# simulation d'un processus de hawkes
a=simulateHawkes(lambda0,alpha,beta,horizon);
P=length(alpha[1,]); # nombre de pays

N_step=10000;
dt=horizon/N_step;

# affichage du processus
plotHawkes<-function(N,dt,a){
  for (k in 1:P){
    n_k=length(a[[k]]);
    lambda_k=c();
    times_k=c();
    for (i in 1:N_step){
      t=i*dt;
      times_k[i]=t;
      j=1;
      intensity_k=0;
      while(a[[k]][j]<t && j<=n_k){
        intensity_k=intensity_k+sum(alpha[k,]*exp(-beta*(t-a[[k]][j])));
        j=j+1;}
      lambda_k[[i]]=lambda0[k] + intensity_k;
    }
    if (k==1){
      plot(times_k,lambda_k,type="l",xlab="time",ylab="intensity",col="red",main="Intensity of Hawkes processes for 2 groups")
    }
    else {
      lines(times_k,lambda_k,type="l",col="blue",lty=2)
    }
  }
}

plotHawkes(N_step,dt,a);

## Algo de montee de gradient stochastique, en cours


SGA<-function(a,lambda0,alpha0,beta0,eps){
  
  P=length(a[1,]);
  
  lambda=lambda0;
  alpha=alpha0;
  beta=beta0[1];
  
  grad_alpha=Inf;
  grad_beta=Inf;

  while(grad_beta^2+grad_alpha^2>eps){
    i=sample(1:P,1);
    k=sample(1:length(a[[i]])-1,1);
    s=lambda[[i]];
    g_ij=c();
    for (j in 1:P){
      k_=0;
      while(a[[j]][k_+1] <a[[i]][k]){
        
      }
      
      while(a[[j]][k_]<a[[i]][k]){
        s=s+alpha[i,j]*beta*exp(-beta*(a[[i]][k]-a[[j]][k_]));
        g_ij[j]=g_ij[j]+alpha[i,j]*beta*exp(-beta*(a[[i]][k]-a[[j]][k_]));
        k_=k_+1;
      }
    }
    
    for (j in 1:P){
      alpha[i,j]=alpha[i,j]-(g_ij[j]/s - exp(-beta*(a[[i]][k]-a[[i]][k-1])));
    }
    
    grad_beta=0;
    for (ii in 1:P){
      for (kk in 1:length(a[[ii]])){
        for (j in 1:P){
          # mettre tous les tj tels que ti_(k-1) < tj < ti_k
          
          grad_beta=grad_beta+(alpha[ii,j]*g_ij[kk]/beta -(a[[ii]][k]-a[[j]][k-1])*exp(beta*(a[[j]][k-1]-a[[ii]][k])))/s;
        }
      }
    }
    
  }
}
