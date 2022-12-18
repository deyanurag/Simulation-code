library(readr)
library(lubridate)
library(MASS)
#library(fda.usc)
library(pps)
library(dplyr)
library(isotone)
library(modi)

mean<-500
sd<-100


c<-100

it<-1000

N<-1000

Z<-matrix(0,N,2)
X<-rep(0,N)
for(i in 1:N)
{
Z[i,]<-mvrnorm(1, c(0,0), matrix(c(1,0,0,1),2,2), tol = 1e-6, empirical = FALSE)
}

X<-rgamma(N,(mean/sd)^2,mean/(sd^2))

a<-0
b<-1
n<-100

t<-seq(1/c,1,1/c)
length(t)

T<-matrix(0,length(t),length(t))
for(i in 1:length(t))
{
for(j in 1:length(t))
{
T[i,j]=min(t[i],t[j])
}
}

# True value of 2*eta
eta<-2*(0)

Y<-matrix(0,N,length(t))

beta<-rep(0,length(t))
for(j in 1:length(t))
{
beta[j]=1-(t[j]-1/2)^2
#t[j]
#1
}


for(i in 1:N)
{
Y[i,]<-mvrnorm(1, 1000+beta*X[i], (X[i]^eta)*T, tol = 1e-6, empirical = FALSE)
}



Q<-seq(1,N,1)

N/n
m<-N/n

R<-NULL
R_1<-rep(0,N-1)
H_1<-rep(0,m)
H_2<-rep(0,m+1)
q<-matrix(0,n,it)

pi_3<-(n*X)/sum(X)


z_1<-matrix(0,n,it)
z_2<-matrix(0,n,it)
z_3<-matrix(0,n,it)
z_4<-matrix(0,n,it)


Z_1<-matrix(0,c,N)
Z_2<-matrix(0,c,N)
Z_3<-matrix(0,c,N)
Z_4<-matrix(0,c,N)

Z_11<-matrix(0,c-1,N)
Z_12<-matrix(0,c-1,N)
Z_13<-matrix(0,c-1,N)
Z_21<-matrix(0,c-1,N)
Z_22<-matrix(0,c-1,N)
Z_23<-matrix(0,c-1,N)
Z_31<-matrix(0,c-1,N)
Z_32<-matrix(0,c-1,N)
Z_33<-matrix(0,c-1,N)
Z_41<-matrix(0,c-1,N)
Z_42<-matrix(0,c-1,N)
Z_43<-matrix(0,c-1,N)

D_1<-rep(0,N)
D_2<-rep(0,N)
D_3<-rep(0,N)
D_4<-rep(0,N)




Y_bar<-rep(0,c)
for(i in 1:c)
{
Y_bar[i]=mean(Y[,i])
}
Y_1<-matrix(0,c,it)
Y_2<-matrix(0,c,it)
Y_3<-matrix(0,c,it)
Y_4<-matrix(0,c,it)
Y_5<-matrix(0,c,it)
Y_6<-matrix(0,c,it)
Y_7<-matrix(0,c,it)
Y_8<-matrix(0,c,it)

X_1<-rep(0,it)
X_3<-rep(0,it)
X_4<-rep(0,it)

W_1<-matrix(0,c,it)
W_2<-matrix(0,c,it)
W_3<-matrix(0,c,it)
W_4<-matrix(0,c,it)
W_5<-matrix(0,c,it)
W_6<-matrix(0,c,it)
W_7<-matrix(0,c,it)
W_8<-matrix(0,c,it)
W_11<-matrix(0,c-1,it)
W_12<-matrix(0,c-1,it)
W_13<-matrix(0,c-1,it)
W_21<-matrix(0,c-1,it)
W_22<-matrix(0,c-1,it)
W_23<-matrix(0,c-1,it)
W_31<-matrix(0,c-1,it)
W_32<-matrix(0,c-1,it)
W_33<-matrix(0,c-1,it)
W_41<-matrix(0,c-1,it)
W_42<-matrix(0,c-1,it)
W_43<-matrix(0,c-1,it)
W_51<-matrix(0,c-1,it)
W_52<-matrix(0,c-1,it)
W_53<-matrix(0,c-1,it)
W_61<-matrix(0,c-1,it)
W_62<-matrix(0,c-1,it)
W_63<-matrix(0,c-1,it)
W_71<-matrix(0,c-1,it)
W_72<-matrix(0,c-1,it)
W_73<-matrix(0,c-1,it)
W_81<-matrix(0,c-1,it)
W_82<-matrix(0,c-1,it)
W_83<-matrix(0,c-1,it)

y_1<-array(0,dim=c(n,it,c))
y_2<-array(0,dim=c(n,it,c))
y_3<-array(0,dim=c(n,it,c))
y_4<-array(0,dim=c(n,it,c))

t_1<-array(0,dim=c(n,it,c))
t_3<-array(0,dim=c(n,it,c))
t_4<-array(0,dim=c(n,it,c))

A_1<-rep(0,N)

u_1<-matrix(0,n,it)
u_2<-matrix(0,n,it)
u_3<-matrix(0,n,it)
u_4<-matrix(0,n,it)

b_1<-matrix(0,it,c)
b_3<-matrix(0,it,c)
b_4<-matrix(0,it,c)

c_1<-matrix(0,it,c)
c_3<-matrix(0,it,c)
c_4<-matrix(0,it,c)

d_1<-matrix(0,it,c)
d_3<-matrix(0,it,c)
d_4<-matrix(0,it,c)

v_1<-rep(0,it)
v_3<-rep(0,it)
v_4<-rep(0,it)

e_1<-rep(0,it)
e_3<-rep(0,it)
e_4<-rep(0,it)

f_1<-rep(0,it)
f_3<-rep(0,it)
f_4<-rep(0,it)

x_1<-matrix(0,n,it)
x_2<-matrix(0,n,it)
x_3<-matrix(0,n,it)
x_4<-matrix(0,n,it)

S_1<-rep(0,it)
S_2<-rep(0,it)
S_3<-rep(0,it)
S_4<-rep(0,it)
S_5<-rep(0,it)
S_6<-rep(0,it)
S_7<-rep(0,it)
S_8<-rep(0,it)


#sampling without replacement

#SRSWOR
for(k in 1:it)
{
z_2[,k]<-sample(Q,n,replace=FALSE)
}



#Sampford
for(k in 1:it)
{
z_3[,k]<-sampford(X,n)
}

#RHC design
for(k in 1:it)
{
R<-Q
for(i in 1:(n-1))
{
H_1<-sample(R,m,replace=FALSE)
q[i,k]<-sum(X[H_1])
z_4[i,k]=H_1[ppswr(X[H_1],1)]
R<-setdiff(R,H_1)
}
q[n,k]<-sum(X[R])
z_4[n,k]=R[ppswr(X[R],1)]
}

for(k in 1:it)
{

x_2[,k]=X[z_2[,k]]
x_3[,k]=X[z_3[,k]]
x_4[,k]=X[z_4[,k]]

u_3[,k]=(1/N)*(1/pi_3[z_3[,k]])
u_4[,k]=(1/N)*(q[,k]/x_4[,k])

v_3[k]=N*(sum(u_3[,k]))
v_4[k]=N*(sum(u_4[,k]))

e_3[k]=sum((x_3[,k])*(x_3[,k])*(1/pi_3[z_3[,k]]))
e_4[k]=sum(x_4[,k]*q[,k])

f_3[k]=(sum((x_3[,k])*(1/pi_3[z_3[,k]])))^2
f_4[k]=(sum(X))^2

for(i in 1:c)
{
y_2[,k,i]=Y[z_2[,k],i]
y_3[,k,i]=Y[z_3[,k],i]
y_4[,k,i]=Y[z_4[,k],i]

t_3[,k,i]=(1/N)*((y_3[,k,i])*(1/pi_3[z_3[,k]]))
t_4[,k,i]=(1/N)*(y_4[,k,i])*(q[,k]/x_4[,k])

c_3[k,i]=sum((y_3[,k,i])*(x_3[,k])*(1/pi_3[z_3[,k]]))
c_4[k,i]=sum(y_4[,k,i]*q[,k])

d_3[k,i]=(sum((y_3[,k,i])*(1/pi_3[z_3[,k]])))*(sum((x_3[,k])*(1/pi_3[z_3[,k]])))
d_4[k,i]=(sum(y_4[,k,i]*(q[,k]/x_4[,k])))*(sum(X))

b_3[k,i]=(c_3[k,i]-(d_3[k,i]/v_3[k]))/(e_3[k]-(f_3[k]/v_3[k]))
b_4[k,i]=(c_4[k,i]-(d_4[k,i]/v_4[k]))/(e_4[k]-(f_4[k]/v_4[k]))
}
}

for(k in 1:it)
{
X_3[k]=sum((1/N)*(1/pi_3[z_3[,k]])*x_3[,k])/sum(u_3[,k])
X_4[k]=(mean(X))/sum(u_4[,k])

for(i in 1:c)
{
Y_1[i,k]=mean(Y[z_2[,k],i])
Y_2[i,k]=Y_1[i,k]+(cov(X[z_2[,k]],Y[z_2[,k],i])/var(X[z_2[,k]]))*(mean(X)-mean(X[z_2[,k]]))
Y_3[i,k]=sum(t_3[,k,i])
Y_5[i,k]=(Y_3[i,k]/sum(u_3[,k]))+(b_3[k,i])*(mean(X)-X_3[k])
Y_4[i,k]=sum(t_4[,k,i])
Y_7[i,k]=(Y_4[i,k]/sum(u_4[,k]))+(b_4[k,i])*(mean(X)-X_4[k])


W_1[i,k]=Y_1[i,k]-Y_bar[i]
W_2[i,k]=Y_2[i,k]-Y_bar[i]
W_3[i,k]=Y_3[i,k]-Y_bar[i]
W_4[i,k]=Y_4[i,k]-Y_bar[i]
W_5[i,k]=Y_5[i,k]-Y_bar[i]
W_7[i,k]=Y_7[i,k]-Y_bar[i]
}
}

#HT_SRS
mean(W_1^2)
#GREG_SRS
mean(W_2^2)

#HT_RS
mean(W_3^2)
#GREG_RS
mean(W_5^2)

#RHC
mean(W_4^2)
#GREG_RHC
mean(W_7^2)

vec<-c(mean(W_1^2), mean(W_2^2), mean(W_3^2), mean(W_5^2), mean(W_4^2), mean(W_7^2))

du<-data.frame(vec)
rownames(du)<-c("HT_SRS", "GREG_SRS", "HT_RS", "GREG_RS", "RHC", "GREG_RHC")
colnames(du)<-c("eta_0")
write.csv(du, "F:/comparison/FDA/File_simulation_1.csv")


#du<-read.csv("F:/comparison/FDA/File_simulation_1.csv")
#du["eta_1"]<-vec
#du
#write.csv(du, "F:/comparison/FDA/File_simulation_1.csv")


# After 'File_simulation_1.csv' is constructed, go to the file 'Curves of relative efficiencies' and run the codes in this file
