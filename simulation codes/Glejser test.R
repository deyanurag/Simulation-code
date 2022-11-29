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
  beta[j]=1-(t[j]-1/2)^2
  #1
  #t[j]
  
}
beta_1<-rep(1,c)

Y_1<-matrix(0,N,length(t))
#Y_21<-matrix(0,N,length(t))
#Y_31<-matrix(0,N,length(t))
#Y_41<-matrix(0,N,length(t))

for(i in 1:N)
{
  Y_1[i,]<-as.vector(rmvnorm(1, 1000+beta*X_1[i], (X_1[i]^eta_6)*T))
  #Y_21[i,]<-as.vector(rmvnorm(1, 1000+beta*X_1[i], (X_1[i]^eta_2)*T))
  #Y_31[i,]<-as.vector(rmvnorm(1, 1000+beta*X_1[i], (X_1[i]^eta_3)*T))
  #Y_41[i,]<-as.vector(rmvnorm(1, 1000+beta*X_1[i], (X_1[i]^eta_4)*T))
}

system.time({
  output<-foreach(1:it,.combine=rbind,.packages=c("pps","dplyr","isotone","mvtnorm","readr","MASS", "PLRModels","modi","lmtest","skedastic"))%dopar%{
    
    n<-500
    Q<-seq(1,N,1)
    z<-rep(0,n)
    
    X<-rep(0,n)
    Y<-matrix(0,n,length(t))
    #Y_2<-matrix(0,n,length(t))
    #Y_3<-matrix(0,n,length(t))
    #Y_4<-matrix(0,n,length(t))
    Z<-rep(0,n)
    #Z_2<-rep(0,n)
    #Z_3<-rep(0,n)
    #Z_4<-rep(0,n)
    
    z<-sample(Q,n,replace = FALSE)
    X<-X_1[z]
    Y<-Y_1[z,]
    #Y_2<-Y_21[z,]
    #Y_3<-Y_31[z,]
    #Y_4<-Y_41[z,]
    Z<-rowSums(Y)/c
    #Z_2<-rowSums(Y_2)/c
    #Z_3<-rowSums(Y_3)/c
    #Z_4<-rowSums(Y_4)/c
    
    
    gl_1<-0
    gl_2<-0
    gl_3<-0
    gl_4<-0
    gl_5<-0
    gl_6<-0
    gl_7<-0
    gl_8<-0
    gl_9<-0
    gl_10<-0
    gl_11<-0
    
    
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
    
    
    gl_1<-glejser(model_1)$p.value
    gl_2<-glejser(model_2)$p.value
    gl_3<-glejser(model_3)$p.value
    gl_4<-glejser(model_4)$p.value
    gl_5<-glejser(model_5)$p.value
    gl_6<-glejser(model_6)$p.value
    gl_7<-glejser(model_7)$p.value
    gl_8<-glejser(model_8)$p.value
    gl_9<-glejser(model_9)$p.value
    gl_10<-glejser(model_10)$p.value
    gl_11<-glejser(model_11)$p.value
    
    
    c(gl_1, gl_2, gl_3, gl_4, gl_5,gl_6,gl_7,gl_8,gl_9,gl_10,gl_11)
    
  }})

stopCluster(myCluster)


p_1<-rep(0,it)
p_2<-rep(0,it)
p_3<-rep(0,it)
p_4<-rep(0,it)
p_5<-rep(0,it)
p_6<-rep(0,it)
p_7<-rep(0,it)
p_8<-rep(0,it)
p_9<-rep(0,it)
p_10<-rep(0,it)
p_11<-rep(0,it)

for(i in 1:it)
{
  if(output[i,1]>=0.05)
    p_1[i]=1
  if(output[i,2]>=0.05)
    p_2[i]=1
  if(output[i,3]>=0.05)
    p_3[i]=1
  if(output[i,4]>=0.05)
    p_4[i]=1
  if(output[i,5]>=0.05)
    p_5[i]=1
  if(output[i,6]>=0.05)
    p_6[i]=1
  if(output[i,7]>=0.05)
    p_7[i]=1
  if(output[i,8]>=0.05)
    p_8[i]=1
  if(output[i,9]>=0.05)
    p_9[i]=1
  if(output[i,10]>=0.05)
    p_10[i]=1
  if(output[i,11]>=0.05)
    p_11[i]=1
}

mean(p_1)
mean(p_2)
mean(p_3)
mean(p_4)
mean(p_5)
mean(p_6)
mean(p_7)
mean(p_8)
mean(p_9)
mean(p_10)
mean(p_11)



