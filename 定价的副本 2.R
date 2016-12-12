library(quantmod)
library(reshape2)
library(ggplot2)
getSymbols("601288.ss")
ZGYH=as.matrix(`601288.SS`) #将中国银行表格数据转化为矩阵
Daily=as.matrix(dailyReturn(`601288.SS`)) #计算日收益率
sigma=sd(Daily)*(252)^0.5#求波动率
S0=2.6;#初始价格
r=0.0284;#无风险收益率
T=5/12;#设定时间
NSteps=700; #每支模拟步数
NReps=20;#平行支数 
Recycles=15;#重复模拟次数
sb=1;#障碍水平
K=3;#敲定价格
Vector=matrix(0,1,Recycles)
for (n in 1:Recycles){
  SPaths = matrix(NA,NReps,NSteps+1);
  SPaths[,1] = S0;
  dt = T/NSteps
  nudt = (r-0.5*sigma^2)*dt
  sidt = sigma*sqrt(dt)
  for (i in 1:NReps){
    for (j in 1:NSteps){
      SPaths[i,j+1] = SPaths[i,j]*exp(nudt + sidt*rnorm(1))
    }
  }#到这步数值模拟已经结束
  step=c(1:(NSteps+1))
  data=data.frame(step)#创建data数据
  for (k in 1:NReps){
    data=data.frame(data,SPaths[k,])
  }
  dfidfm=melt(data,id.vars="step")#熔开data
  ggplot(dfidfm,aes(x=step,y=value))+geom_line(aes(color=variable))#绘图
  payoff=matrix(0,NReps,1)
  for (m in 1:NReps){
    ax=SPaths[m,];
    if (min(ax)<sb)
      payoff[m]=0
    else 
      payoff[m]=max(0,K-ax[NSteps])
  }
  P=mean(exp(-r*T)*payoff)
  Vector[1,n]=P
}
Average=mean(Vector)
#Average即是定价。