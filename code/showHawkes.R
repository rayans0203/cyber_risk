library(hawkes)
# param??tres arbitrairement fix??s
lambda=c(0.2,0.2);
alpha=matrix(c(0.,0.8,0.,0.2),byrow=TRUE,nrow=2);
beta=c(0.7,0.7);
horizon=30;
epsilon=1e-10;

# simulation d'un processus de hawkes
a=simulateHawkes(lambda,alpha,beta,horizon);
print(jumpMean(lambda,alpha,beta,horizon));
N_step=10000;
dt=horizon/N_step;

# affichage du processus
plotHawkes<-function(N,dt,a,graph="plot"){
  P=length(a); # nombre de pays
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
      lambda_k[[i]]=lambda[k] + intensity_k;
    }
    if (k==1 & graph=="plot"){
      plot(times_k,lambda_k,type="l",xlab="t",ylab="intensity",col="red",main="Intensity of Hawkes processes for 2 groups")
    }
    else {
      lines(times_k,lambda_k,type="l",col="blue",lty=2)
    }
    legend(0.2, 2.55, legend=c("Group 1", "Group 2"),
           col=c("red", "blue"), lty=1:2, cex=0.8);
  }
}
plotHawkes(N_step,dt,a);
print(alpha);
# g_j<-function(a,k,j,i,beta){
#   g_jj=0;
#   k__=1;
#   n=length(a[[j]]);
#   while(a[[j]][k__]<a[[i]][k] && k__ <= n){
#     g_jj=g_jj+beta*exp(-beta*(a[[i]][k]-a[[j]][k__]));
#     k__=k__+1;
#     print(a[[j]][k__]<a[[i]][k] && k__ <= n);
#   }
#   return(g_jj);
# }
# 
# update_alpha<-function(a,k,i,beta,alpha,lambda){
#   alpha_new=alpha;
#   s=lambda[i];
#   for (j in 1:P){
#     s=s+alpha[i,j]*g_j(a,k,j,i,beta);
#   }
#   P=length(alpha[1,]);
#   for (j in 1:P){
#     g_jj=g_j(a,k,j,i,beta);
#     alpha_new[i,j]=alpha[i,j]-(g_jj/s - g_jj*(1-exp(-beta*(a[[i]][k+1]-a[[i]][k]))));
#     k_=1;
#     while(a[[j]][k_]<=a[[i]][k]){
#       k_=k_+1;
#     }
#     while(a[[j]][k_]<a[[i]][k+1]){
#       alpha_new[i,j]=alpha[i,j]+1-exp(-beta*(a[[i]][k+1]-a[[j]][k_]));
#       k_=k_+1;
#     }
#   }
#   return(alpha_new);
# }
# 
# update_beta<-function(a){
# }

## Algo de montee de gradient stochastique, en cours
# SGA<-function(a,lambda0,alpha0,beta0,eps){
#   lambda=lambda0;
#   alpha=alpha0;
#   beta=beta0[1];
#   
#   grad_alpha=Inf;
#   grad_beta=0;
# 
#   while(grad_beta^2+grad_alpha^2>eps){
#     i=sample(1:P,1);
#     k=sample(1:length(a[[i]])-1,1);
#     alpha_old=alpha;
#     alpha=update_alpha(a,k,i,beta,alpha,lambda);
#     grad_alpha=sum((alpha_old-alpha)^2);
#     # print(grad_alpha^2);
#   }
#   return(alpha);
# }

# LKHawkes<-function(X){
#   #lambda0=X[1];
#   alpha0=X[1];
#   #beta0=X[3];
#   history=simulateHawkes(0.3,0.6,7,horizon);
#   -likelihoodHawkes(0.3,alpha0,7,history[[1]])
# }
# 
# #alpha0=matrix(c(0.5,0.,0.,0.5),byrow=TRUE,nrow=2);
# alpha0=0.1;
# beta0=0.9;
# lambda0=0.3;
# 
# #alp=SGA(a,lambda,alpha0,beta,epsilon);
# X=c(alpha0);
# alp=optim(X,LKHawkes,method="Brent",lower=0,upper=1)
# print(alp)
