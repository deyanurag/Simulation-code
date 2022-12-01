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
#true value of 2*eta
eta_0<-0

#a set 2*eta's

eta_1<-0
eta_2<-0.2
eta_3<-0.4
eta_4<-0.6
eta_5<-0.8
eta_6<-1
eta_7<-1.2
eta_8<-1.4
eta_9<-1.6
eta_10<-1.8
eta_11<-2

se<-seq(1,it,1)

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
  beta[j]=t[j]
  #1-(t[j]-1/2)^2
  #1
  
}
beta_1<-rep(1,c)

Y_1<-matrix(0,N,length(t))


for(i in 1:N)
{
  Y_1[i,]<-as.vector(rmvnorm(1, 1000+beta*X_1[i], (X_1[i]^eta_0)*T))
}

system.time({
  output<-foreach(1:it,.combine=rbind,.packages=c("pps","dplyr","isotone","mvtnorm","readr","MASS", "PLRModels","modi","lmtest","skedastic"))%dopar%{
    
    n<-500
    Q<-seq(1,N,1)
    z<-rep(0,n)
    
    X<-rep(0,n)
    Y<-matrix(0,n,length(t))

    Z<-rep(0,n)

    
    z<-sample(Q,n,replace = FALSE)
    X<-X_1[z]
    Y<-Y_1[z,]

    Z<-rowSums(Y)/c

    
    
    wh_1<-0
    wh_2<-0
    wh_3<-0
    wh_4<-0
    wh_5<-0
    wh_6<-0
    wh_7<-0
    wh_8<-0
    wh_9<-0
    wh_10<-0
    wh_11<-0
    
    
    df_1<-data.frame(Z/(X^(0.5*eta_1) ),X/(X^(0.5*eta_1) ),1/(X^(0.5*eta_1) ))
    df_2<-data.frame(Z/(X^(0.5*eta_2) ),X/(X^(0.5*eta_2) ),1/(X^(0.5*eta_2) ))
    df_3<-data.frame(Z/(X^(0.5*eta_3) ),X/(X^(0.5*eta_3) ),1/(X^(0.5*eta_3) ))
    df_4<-data.frame(Z/(X^(0.5*eta_4) ),X/(X^(0.5*eta_4) ),1/(X^(0.5*eta_4) ))
    df_5<-data.frame(Z/(X^(0.5*eta_5) ),X/(X^(0.5*eta_5) ),1/(X^(0.5*eta_5) ))
    df_6<-data.frame(Z/(X^(0.5*eta_6) ),X/(X^(0.5*eta_6) ),1/(X^(0.5*eta_6) ))
    df_7<-data.frame(Z/(X^(0.5*eta_7) ),X/(X^(0.5*eta_7) ),1/(X^(0.5*eta_7) ))
    df_8<-data.frame(Z/(X^(0.5*eta_8) ),X/(X^(0.5*eta_8) ),1/(X^(0.5*eta_8) ))
    df_9<-data.frame(Z/(X^(0.5*eta_9) ),X/(X^(0.5*eta_9) ),1/(X^(0.5*eta_9) ))
    df_10<-data.frame(Z/(X^(0.5*eta_10) ),X/(X^(0.5*eta_10) ),1/(X^(0.5*eta_10) ))
    df_11<-data.frame(Z/(X^(0.5*eta_11) ),X/(X^(0.5*eta_11) ),1/(X^(0.5*eta_11) ))
    
    colnames(df_1)<-c("study","var_1", "var_2")
    colnames(df_2)<-c("study","var_1", "var_2")
    colnames(df_3)<-c("study","var_1", "var_2")
    colnames(df_4)<-c("study","var_1", "var_2")
    colnames(df_5)<-c("study","var_1", "var_2")
    colnames(df_6)<-c("study","var_1", "var_2")
    colnames(df_7)<-c("study","var_1", "var_2")
    colnames(df_8)<-c("study","var_1", "var_2")
    colnames(df_9)<-c("study","var_1", "var_2")
    colnames(df_10)<-c("study","var_1", "var_2")
    colnames(df_11)<-c("study","var_1", "var_2")
    
    
    model_1<-lm(study~0+var_2+var_1,df_1)
    model_2<-lm(study~0+var_2+var_1,df_2)
    model_3<-lm(study~0+var_2+var_1,df_3)
    model_4<-lm(study~0+var_2+var_1,df_4)
    model_5<-lm(study~0+var_2+var_1,df_5)
    model_6<-lm(study~0+var_2+var_1,df_6)
    model_7<-lm(study~0+var_2+var_1,df_7)
    model_8<-lm(study~0+var_2+var_1,df_8)
    model_9<-lm(study~0+var_2+var_1,df_9)
    model_10<-lm(study~0+var_2+var_1,df_10)
    model_11<-lm(study~0+var_2+var_1,df_11)
    
    
    wh_1<-bptest(model_1,~var_2+var_1+var_2*var_1+I(var_2^2)+I(var_1^2),data=df_1)$p.value
    wh_2<-bptest(model_2,~var_2+var_1+var_2*var_1+I(var_2^2)+I(var_1^2),data=df_2)$p.value
    wh_3<-bptest(model_3,~var_2+var_1+var_2*var_1+I(var_2^2)+I(var_1^2),data=df_3)$p.value
    wh_4<-bptest(model_4,~var_2+var_1+var_2*var_1+I(var_2^2)+I(var_1^2),data=df_4)$p.value
    wh_5<-bptest(model_5,~var_2+var_1+var_2*var_1+I(var_2^2)+I(var_1^2),data=df_5)$p.value
    wh_6<-bptest(model_6,~var_2+var_1+var_2*var_1+I(var_2^2)+I(var_1^2),data=df_6)$p.value
    wh_7<-bptest(model_7,~var_2+var_1+var_2*var_1+I(var_2^2)+I(var_1^2),data=df_7)$p.value
    wh_8<-bptest(model_8,~var_2+var_1+var_2*var_1+I(var_2^2)+I(var_1^2),data=df_8)$p.value
    wh_9<-bptest(model_9,~var_2+var_1+var_2*var_1+I(var_2^2)+I(var_1^2),data=df_9)$p.value
    wh_10<-bptest(model_10,~var_2+var_1+var_2*var_1+I(var_2^2)+I(var_1^2),data=df_10)$p.value
    wh_11<-bptest(model_11,~var_2+var_1+var_2*var_1+I(var_2^2)+I(var_1^2),data=df_11)$p.value
    
    p<-0
    
  
    maximum=max(wh_1, wh_2, wh_3, wh_4, wh_5,wh_6,wh_7,wh_8,wh_9,wh_10,wh_11)
    if(maximum%in% c(wh_1, wh_2, wh_3, wh_4, wh_5,wh_6))
    p=1/it
    p
  }})

stopCluster(myCluster)

sum(output[,1])




