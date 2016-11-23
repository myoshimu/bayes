#Predict winning rate with random walk mMetropolis-Hastings methods
set.seed(1234)
qsd <- sqrt(0.1)   #Proposal distribution ＳＤ
Nsi <- 10^5    #sample size
Bin <- 10^3        #burn-in period
x<-numeric(Nsi)
x[1]<-4.0
co<-0
for (t in 2:Nsi){
    a<-runif(1)
    if (runif(1) < dbeta(a,10.2,5.8)/
                   dbeta(x[t-1],10.2,5.8)) {
       x[t]<-a; co<-co+1
    }else{
       x[t]<-x[t-1]
    }
}

#Acceptance rate
print(round(co/Nsi,2))

#trace line
par(mfrow=c(2,1))
plot(x[1:1000],type="l",ylab='',xlab='',lwd=1,ylim=c(0,4.0),cex.axis=2.0)
plot(x,type="l",ylab='',xlab='',lwd=0.5,ylim=c(0,4.0),cex.axis=2.0)
par(mfrow=c(1,1))

#EAP
eap=round(mean(x[(Bin+1):Nsi]),3)

#Winning rate(Binomial distribution, n=5,x=3)
choose(5, 3)*eap^3*(1-eap)^2

#Evaluate parameters
#theta  : Parameter values including buring-in period
#ff     : Function
#pre    : description
#b      : burn-in
#yu     : significant figure
genqua<- function(theta,ff,pre,b,yu=3) {
   lx <- length(theta)
   sx <- sort(ff(theta[(b+1):lx]))
   yx <- lx-b
   xxx <- matrix(c(round(mean(sx),yu),round(sd(sx),yu),round(sx[yx%/%40],yu),round(sx[yx-yx%/%40],yu)),1,4)
   colnames(xxx) <- c("mean","SD","95b","95u")
   rownames(xxx) <- pre
   return(xxx)
}

f1<-function(x){x}
f2<-function(x){sqrt(x-x^2)}
fB<-function(x){dbinom(3,5,x)}

#Estimate standard deviation
genqua(theta=x,ff=f2,pre='SD',b=1000)

#Winning rate(n=5,x=3) using　posterior distribution
genqua(theta=x,ff=fB,pre='3勝',b=1000)
