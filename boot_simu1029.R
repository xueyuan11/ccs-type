setwd("C:/Users/o0/Desktop/ordinal data/模拟")
source("statistic1029.R")
require(rms)
ptm<-proc.time()
ordsim<-function(alphax, betax, alphay,betay,eta,N,Nemp,Nrep){
  
  pval.emp = matrix(,Nrep,4)
  colnames(pval.emp) = c("T1", "T2", "T3", "T4")
  pval.con = pval.dis = pval.spl  = pval.chisq=rep(0,Nrep)
  
  for (k in 1:Nrep){ 
  data = generate.data(alphax, betax, alphay, betay, eta, N)
  pval.emp[k,] = OC_p(data, Nemp)
  modyx.con = lrm(y ~ x, data=data)
  modyx.dis = lrm(y ~ x,data=list(y=data$y,x=as.factor(data$x)))
  modspline = lrm(y ~ rcs(x, 3), data=data)
  pval.con[k] = anova(modyx.con)[1,3]
  pval.dis[k] = anova(modyx.dis)[1,3]
  pval.spl[k] = anova(modspline)[1,3]
  pval.chisq[k]=chisq.test(data$x,data$y)$p.value
  }
  param = list(alpha.y=alphay, beta.y=betay,
               alpha.x=alphax, beta.x=betax, eta=eta,
               N=N, Nemp=Nemp, Nrep=Nrep)
  list(par= param, pval=data.frame(pval.emp,pval.con, pval.dis, pval.spl,pval.chisq))
}
N = 500
Nemp = 1000
Nrep = 1000

alphay = c(-1, 0, 1)
betay = -.5
alphax = c(-1, 0, 1, 2)
betax = c(0,1,2,3)

eta = rep(0,5)
sim0s = ordsim(alphax, betax, alphay, betay, eta, N, Nemp, Nrep)

eta = 0.2 * (-2:2)
sim1s = ordsim(alphax, betax, alphay, betay, eta, N, Nemp, Nrep)

eta = c(-.3, .18, .20, .22, .24)
sim2s = ordsim(alphax, betax, alphay, betay, eta, N, Nemp, Nrep)

eta = 0.1 * c(-2,0,2,0,-2)
sim3s = ordsim(alphax, betax, alphay, betay, eta, N, Nemp, Nrep)

proc.time()-ptm


##Computer Type I error and power
library(xlsx)
alpha=NULL
p=0.05
alpha[1]=sum(sim0s$pval$T1<p)/Nrep
alpha[2]=sum(sim0s$pval$T2<p)/Nrep
alpha[3]=sum(sim0s$pval$T3<p)/Nrep
alpha[4]=sum(sim0s$pval$T4<p)/Nrep
alpha[5]=sum(sim0s$pval$pval.con<p)/Nrep
alpha[6]=sum(sim0s$pval$pval.dis<p)/Nrep
#save.image()
alpha[7]=sum(sim0s$pval$pval.spl<p)/Nrep
alpha[8]=sum(sim0s$pval$pval.chisq<p)/Nrep

beta=matrix(0,8,3)
# for (i in 1:7){
#   beta[i,1]=sum(sim1s$pval[i,]<p)/Nrep
# }
  
beta[1,1]=sum(sim1s$pval$T1<p)/Nrep
beta[2,1]=sum(sim1s$pval$T2<p)/Nrep
beta[3,1]=sum(sim1s$pval$T3<p)/Nrep
beta[4,1]=sum(sim1s$pval$T4<p)/Nrep
beta[5,1]=sum(sim1s$pval$pval.con<p)/Nrep
beta[6,1]=sum(sim1s$pval$pval.dis<p)/Nrep
beta[7,1]=sum(sim1s$pval$pval.spl<p)/Nrep
beta[8,1]=sum(sim1s$pval$pval.chisq<p)/Nrep
#save.image()
#

beta[1,2]=sum(sim2s$pval$T1<p)/Nrep
beta[2,2]=sum(sim2s$pval$T2<p)/Nrep
beta[3,2]=sum(sim2s$pval$T3<p)/Nrep
beta[4,2]=sum(sim2s$pval$T4<p)/Nrep
beta[5,2]=sum(sim2s$pval$pval.con<p)/Nrep
beta[6,2]=sum(sim2s$pval$pval.dis<p)/Nrep
beta[7,2]=sum(sim2s$pval$pval.spl<p)/Nrep
beta[8,2]=sum(sim2s$pval$pval.chisq<p)/Nrep
#save.image()
#

beta[1,3]=sum(sim3s$pval$T1<p)/Nrep
# beta[4]=sum(sim3s$pval$T1b<p)/Nrep
# beta[5]=sum(sim3s$pval$T2b<p)/Nrep
# beta[6]=sum(sim3s$pval$T3b<p)/Nrep
beta[2,3]=sum(sim3s$pval$T2<p)/Nrep
beta[3,3]=sum(sim3s$pval$T3<p)/Nrep
beta[4,3]=sum(sim3s$pval$T4<p)/Nrep
beta[5,3]=sum(sim3s$pval$pval.con<p)/Nrep
beta[6,3]=sum(sim3s$pval$pval.dis<p)/Nrep
beta[7,3]=sum(sim3s$pval$pval.spl<p)/Nrep
beta[8,3]=sum(sim3s$pval$pval.chisq<p)/Nrep
#save.image()
result<-cbind(alpha,beta)

rownames(result)=c("T1", "T2", "T3", "T4", "X linear","X catego","Spline","Chisq")
colnames(result)=c("Null","Linear","Nonlinear","Nonmontonic")


write.xlsx(x = result, file = "result.xlsx",
           sheetName = "mine", row.names =TRUE,col.names = TRUE)