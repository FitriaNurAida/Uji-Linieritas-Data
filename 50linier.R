#MODEL LINIER
#membangkitkan data
e<-rnorm(50)
x1<-runif(50,0,1)
x2<-runif(50,0,1)
x3<-runif(50,0,1)
y<-1+0.5*x1-0.2*x2+0.3*x3+0.1*e
#plot
par(mfrow=c(1,3))
plot(x1,y)
plot(x2,y)
plot(x3,y)
#uji linieritas
resettest(y~1+x1+x2+x3,power=2, type="regressor")
resettest(y~1+x1+x2+x3,power=3, type="regressor")
resettest(y~1+x1+x2+x3,power=2:3, type="regressor")

resettest(y~1+x1+x2+x3,power=2, type="fitted")
resettest(y~1+x1+x2+x3,power=3, type="fitted")
resettest(y~1+x1+x2+x3,power=2:3, type="fitted")

resettest(y~1+x1+x2+x3,power=2, type="princomp")
resettest(y~1+x1+x2+x3,power=3, type="princomp")
resettest(y~1+x1+x2+x3,power=2:3, type="princomp")

white.test(cbind(x1,x2,x3),y,type="F")
terasvirta.test(cbind(x1,x2,x3),y,type="F")

#####################################
#SIMULASI100data
# Mohon diperhatikan directory yang digunakan
# Replikasi 10000 kali dan hasil p-value disimpan di "hasil10000.csv" atau "hasil10000.xlsx"
# Contoh Model Linier --> Ganti random datanya untuk model non linier

setwd("E:/2018_2019/SEMESTER 6/AD")
result50 <- matrix(0,ncol = 11,nrow = 10000)
colnames(result50) <- c("regresssor^2", "regresssor^3", "regresssor^23",
                      "fitted^2", "fitted^3", "fitted^23",
                      "princomp^2", "princomp^3", "princomp^23",
                      "white", "terasvirta")
for (i in 1:10000)
{
  n <- 50
  e <- rnorm(50)
  x1 <- runif(50, 0, 1)  
  x2 <- runif(50, 0, 1) 
  x3 <- runif(50, 0, 1) 
  y <- 1 + 0.5*x1 - 0.2*x2 + 0.3*x3 + 0.1*e
  
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
  
  result50[i,1] <- prr2$p.value
  result50[i,2] <- prr3$p.value
  result50[i,3] <- prr23$p.value
  result50[i,4] <- prf2$p.value
  result50[i,5] <- prf3$p.value
  result50[i,6] <- prf23$p.value
  result50[i,7] <- prp2$p.value
  result50[i,8] <- prp3$p.value
  result50[i,9] <- prp23$p.value
  result50[i,10] <- pw$p.value
  result50[i,11] <- pt$p.value
}

write.csv(result50, "hasil5010000.csv")

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
  x[i]=length(result50[,i][result50[,i]>0.05])
  persen_benar[i]=x[i]/10000*100
}

z50=data.frame(uji_statistik, persen_benar,row.names = NULL)
z50
hasi50linier=write.csv(z50,"hasil50linier.csv")


grafikz100=ggplot(z100,aes(z100$uji_statistik,z100$persen_benar))+geom_col(aes(color="blue"))
grafikz100
