
setwd("E:/2018_2019/SEMESTER 6/AD")
result100non1new <- matrix(0,ncol = 11,nrow = 10000)
colnames(result100non1new) <- c("regresssor^2", "regresssor^3", "regresssor^23",
                            "fitted^2", "fitted^3", "fitted^23",
                            "princomp^2", "princomp^3", "princomp^23",
                            "white", "terasvirta")
library(lmtest)
library(tseries)

for (i in 1:10000)
{
  n <- 100
  sigma <- matrix(c(1,0.9,0.8,0.9,1,0.7,0.8,0.7,1), ncol=3)
  x <- rmvnorm(n=100, mean=c(1,1,1), sigma=sigma)
  e <- rnorm(100)
  x1 <- x[,1]
  x2 <- x[,2]
  x3 <- x[,3]
  y <- log(x1+4) - sqrt(x2+4) + 0.3*x3 + 0.1*e
  
  prr2 <- resettest(y ~ 1 + x1 + x2 + x3, power=2, type="regressor")
  prr3 <- resettest(y ~ 1 + x1 + x2 + x3, power=3, type="regressor")
  prr23 <- resettest(y ~ 1 + x1 + x2 + x3, power=2:3, type="regressor")
  
  prf2 <- resettest(y ~ 1 + x1 + x2 + x3, power=2, type="fitted")
  prf3 <- resettest(y ~ 1 + x1 + x2 + x3, power=3, type="fitted")
  prf23 <- resettest(y ~ 1 + x1 + x2 + x3, power=2:3, type="fitted")
  
  prp2 <- resettest(y ~ 1 + x1 + x2 + x3, power=2, type="princomp")
  prp3 <- resettest(y ~ 1 + x1 + x2 + x3, power=3, type="princomp")
  prp23 <- resettest(y ~ 1 + x1 + x2 + x3, power=2:3, type="princomp")
  
  pw <- white.test(cbind(x1,x2,x3), y, type="F")
  pt <- terasvirta.test(cbind(x1,x2,x3), y, type="F")
  
  result100non1new[i,1] <- prr2$p.value
  result100non1new[i,2] <- prr3$p.value
  result100non1new[i,3] <- prr23$p.value
  result100non1new[i,4] <- prf2$p.value
  result100non1new[i,5] <- prf3$p.value
  result100non1new[i,6] <- prf23$p.value
  result100non1new[i,7] <- prp2$p.value
  result100non1new[i,8] <- prp3$p.value
  result100non1new[i,9] <- prp23$p.value
  result100non1new[i,10] <- pw$p.value
  result100non1new[i,11] <- pt$p.value
}

write.csv(result100non1new, "hasil10010000non1new.csv")

# Mohon dilakukan dengan n berbeda-beda, yaitu 50, 100, dan 200.
# Juga dengan 2 skenario model nonlinear seperti paper Anders & Korn (1999)
x=0
persen_benar=0
uji_statistik <- c("regresssor^2", "regresssor^3", "regresssor^23",
                   "fitted^2", "fitted^3", "fitted^23",
                   "princomp^2", "princomp^3", "princomp^23",
                   "white", "terasvirta")

for (i in 1:11)
{
  x[i]=length(result100non1new[,i][result100non1new[,i]<0.05])
  persen_benar[i]=x[i]/10000*100
}

z100non1new=data.frame(uji_statistik, persen_benar,row.names = NULL)
z100non1new
hasi100nonlinier1new=write.csv(z100non1new,"hasil100nonlinier1new.csv")