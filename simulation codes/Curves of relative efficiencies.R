library(usethis)
library(base)
library(pps)
library(dplyr)
library(isotone)
library(MASS)

library(isotone)

#beta(t)=t
#beta(t)=1-(t-1/2)^2
a<-c(0,0.1, 0.2, 0.3, 0.4,0.5, 0.6, 0.7, 0.8, 0.9 ,1)

du<-read.csv("D:/comparison/FDA/File_simulation_1.csv")

dv<-t(du)

b<-rep(1,11)


RE_1<-expression(bold(RE(bolditalic(hat(bar(Y))[HT]),~SRSWOR~"|"~bolditalic(hat(bar(Y))[GREG]),~SRSWOR)))
RE_2<-expression(bold(RE(bolditalic(hat(bar(Y))[HT]),~PPSWOR~"|"~bolditalic(hat(bar(Y))[GREG]),~PPSWOR)))
RE_3<-expression(bold(RE(bolditalic(hat(bar(Y))[HT]),~RS~"|"~bolditalic(hat(bar(Y))[GREG]),~RS)))
RE_4<-expression(bold(RE(bolditalic(hat(bar(Y))[RHC]),~RHC~"|"~bolditalic(hat(bar(Y))[GREG]),~RHC)))

RE_5<-expression(bold(RE(bolditalic(hat(bar(Y))[GREG]),~SRSWOR~"|"~bolditalic(hat(bar(Y))[GREG]),~PPSWOR)))
RE_6<-expression(bold(RE(bolditalic(hat(bar(Y))[GREG]),~SRSWOR~"|"~bolditalic(hat(bar(Y))[GREG]),~RS)))
RE_7<-expression(bold(RE(bolditalic(hat(bar(Y))[GREG]),~SRSWOR~"|"~bolditalic(hat(bar(Y))[GREG]),~RHC)))

du[1,2:12]

plot(a,as.numeric(du[4,-1])/as.numeric(du[2,-1]) ,ylim=c(0.8,1.2),type="l", lty="solid", xlab="Degree of heteroscedasticity",ylab="Relative efficiency"
     ,cex.lab=1.5,cex.axis=1.5,cex.main=1.5)
#lines(a,as.numeric(du[6,-1])/as.numeric(du[2,-1]) ,type="l",lty="longdash" )
lines(a,as.numeric(du[8,-1])/as.numeric(du[2,-1]) ,type="l",lty="dotted" )
lines(a, b,type="l", lty="dashed")
legend(0.54,1.23,legend=c(RE_6, RE_7), lty=c("solid","dotted"), cex=1.2, box.col=c("white"))

as.numeric(du[1,-1])
as.numeric(du[2,-1])
par(mfrow=c(1,3),mar=c(5.1,4.8,4.1,2.1))

plot(a,as.numeric(du[2,-1])/as.numeric(du[1,-1]) , main=RE_1,
     ylim=c(0,1.5),type="l", lty="solid", xlab="Degree of heteroscedasticity",ylab="Relative efficiency",cex.lab=2,cex.axis=2,cex.main=1.5)
lines(a, b,type="l",ylim=c(0,1 ), lty="dashed")


#plot(a,as.numeric(du[4,-1])/as.numeric(du[3,-1]), main=RE_2,
#ylim=c(0,1.5),type="l", lty="solid", xlab="Degree of heteroscedasticity",ylab="Relative efficiency",cex.lab=1.5,cex.axis=1.5,cex.main=1.5)
#lines(a, b,type="l",ylim=c(0,1 ), lty="dashed")


plot(a,as.numeric(du[6,-1])/as.numeric(du[5,-1]), main=RE_3,
     ylim=c(0,1.5),type="l", lty="solid", xlab="Degree of heteroscedasticity",ylab="Relative efficiency",cex.lab=2,cex.axis=2,cex.main=1.5)
lines(a, b,type="l",ylim=c(0,1 ), lty="dashed")


plot(a,as.numeric(du[8,-1])/as.numeric(du[7,-1]),main=RE_4,
     ylim=c(0,1.5),type="l", lty="solid", xlab="Degree of heteroscedasticity",ylab="Relative efficiency",cex.lab=2,cex.axis=2,cex.main=1.5)
lines(a, b,type="l",ylim=c(0,1 ), lty="dashed")


#################################
