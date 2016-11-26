
#### Function for simulating data
generate.data = function(alphax, betax, alphay, betay, eta, N) {
 
  x = y = numeric(N)
  
  ## px is an N x length(alphax) matrix.
  ## Each row has the TRUE cummulative probabilities for each subject.
  px = (1 + exp(- alphax) )^ (-1)
  aa = runif(N)
  for(i in 1:N)
    x[i] = sum(aa[i] > px)
  x = as.numeric(as.factor(x))
  ## x = x+1 may have category gaps if there are small probability categories.
  py = (1 + exp(- outer(alphay, eta[x], "+"))) ^ (-1)
  aa = runif(N)
  for(i in 1:N)
    y[i] = sum(aa[i] > py[,i])
  y = as.numeric(as.factor(y))
  ## y = y+1 may have category gaps if there are small probability categories.
  
  return(list(x=x, y=y))
}
OC_stat<-function(data){
x <-data$x
y <-data$y
N<-length(x)
tab_n<-as.matrix(table(x,y))
R<-rowSums(tab_n)#X频数
C<-colSums(tab_n)#Y频数
Rp_cum<-cumsum(R)/N
Cp_cum<-cumsum(C)/N

#频率
nx<-dim(tab_n)[1]#x的类个数
ny<-dim(tab_n)[2]##y的类个数
#计算累计频数
Rz<-t(apply(tab_n,1,cumsum))#行x
Cz<-apply(tab_n,2,cumsum)#列y
RCz<-apply(Rz,2,cumsum)#联合累计
T1<-0
T2<-0
T3<-0
#F(y|x)
for (j in 1:(ny-1)){
  for (i in 1:nx){
    T1<-T1+ R[i]*(Rz[i,j]/R[i]-Cp_cum[j])^2/ny
  }
} 
#F(x|y)
for (i in 1:(nx-1)){
  for (j in 1:ny){
    T2<- T2+C[j]*(Cz[i,j]/C[j]-Rp_cum[i])^2/nx
  }
}
#F(x,y)
for (i in 1:(nx-1)){
  for (j in 1:(ny-1)){
    T3<- T3+N*(RCz[i,j]/N-Rp_cum[i]*Cp_cum[j])^2/(nx*ny)
  }
}
T4<-mean(c(T1,T2))
list(stat=c(T1,T2,T3,T4),probx=R/N,proby=C/N)
}
OC_p<-function(data,Nemp=1000){
  OC.stat = OC_stat(data)
  N = length(data$y)
  nx = length(table(data$x))
  p<-outer(OC.stat$probx, OC.stat$proby)
  test.stat.emp = matrix(,4,Nemp)
  for(j in 1:Nemp){
  xyemp<-sample(0:19,N,replace = T,p)
  yemp = xyemp %/% nx + 1
  xemp = xyemp %% nx + 1
  test.stat.emp[,j]=OC_stat(list(x=xemp, y=yemp))$stat
  }
  c(apply(OC.stat$stat <= test.stat.emp, 1, mean, na.rm=T))
}
