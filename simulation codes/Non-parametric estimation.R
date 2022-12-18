library(lmtest)
library(skedastic)
library(parallel)
library(foreach)
library(iterators)
library(doParallel)
library(mvtnorm)
library(PLRModels)
library(readr)
library(MASS)
library(pps)
library(dplyr)
library(isotone)
library(modi)

#detectCores()
myCluster<-makeCluster(64)
registerDoParallel(myCluster)

it<-100
# true value of 2*eta
eta_1<-0



me<-500
va<-100^2
N<-5000

X_1<-rgamma(N,(me)^2/va,me/va)
c<-100
t<-seq(1/c,1,1/c)

T<-matrix(0,length(t),length(t))
for(i in 1:length(t))
{
  for(j in 1:length(t))
  {
    T[i,j]=min(t[i],t[j])
  }
}

beta<-rep(0,length(t))
for(j in 1:length(t))
{
  beta[j]=1-(t[j]-1/2)^2
  #t[j]
  #1
  
}


Y_1<-matrix(0,N,length(t))

for(i in 1:N)
{
  Y_1[i,]<-as.vector(rmvnorm(1, 1000+beta*X_1[i], (X_1[i]^eta_1)*T))
}

system.time({
  output<-foreach(1:it,.combine=rbind,.packages=c("pps","dplyr","isotone","mvtnorm","readr","MASS", "PLRModels","modi","lmtest","skedastic"))%dopar%{
    
n<-500
Q<-seq(1,N,1)
z<-rep(0,n)

X<-rep(0,n)
Y<-matrix(0,n,length(t))



z<-sample(Q,n,replace = FALSE)
X<-X_1[z]
Y<-Y_1[z,]



mNW <- function(x, Z, Y, h) {
  
  Kx <- sapply(Z, function(Xi) dunif((x - Xi) / h,-1,1))
  
  # Weights
  W <- Kx / sum(Kx) 
  
  # Means at x 
  return(sum(W*Y))
  
}




d<-c
NW_1<-matrix(0,length(X),d)
NW_2<-matrix(0,length(X),d)
NW_3<-matrix(0,length(X),d)
NW<-rep(0,length(X))


h_0<-rep(0,d)
h_1<-rep(0,d)
h_2<-rep(0,d)
h_3<-rep(0,d)
h_4<-rep(0,d)
h_5<-rep(0,d)
h_6<-rep(0,d)
h_7<-rep(0,d)
a<-rep(1/d,d)



for(i in 1:d)
{
  h_0[i]=np.cv(cbind( Y[,i], X),ln.0=0, kernel="uniform")$h.opt[2,1]
  NW_1[,i]=sapply(X, function(z) mNW( z,X,Y[,i],h_0[i]))
  NW_2[,i]=( Y[,i]-NW_1[,i])^2
  h_1[i]=np.cv(cbind(NW_2[,i], X ),ln.0=0,kernel="uniform")$h.opt[2,1]
  NW_3[,i]=sapply(X, function(z) mNW( z,X, NW_2[,i], h_1[i]) )
}

NW=NW_3%*%a


df_1<-data.frame(log(NW),log(X))
colnames(df_1)<-c("resp_1","ind_1")
df_1[is.na(df_1) | df_1=="-Inf"] = NA
#reg_1<-lm(resp_1~ind_1,df_1)
#plot(log(X),log(NW_1))
#abline(reg_1,lty="dotted")





cov(df_1$resp_1,df_1$ind_1, "complete")/cov(df_1$ind_1,df_1$ind_1, "complete") 

}})

stopCluster(myCluster)


P<-rep(0,it)



for(i in 1:it)
{
  if(output[i,1]<=1)
  P[i]=1

}
mean(P)


