#SIMULASI100data
# Mohon diperhatikan directory yang digunakan
# Replikasi 10000 kali dan hasil p-value disimpan di "hasil10000.csv" atau "hasil10000.xlsx"
# Contoh Model Linier --> Ganti random datanya untuk model non linier

setwd("E:/2018_2019/SEMESTER 6/AD")
result200 <- matrix(0,ncol = 11,nrow = 10000)
colnames(result200) <- c("regresssor^2", "regresssor^3", "regresssor^23",
                        "fitted^2", "fitted^3", "fitted^23",
                        "princomp^2", "princomp^3", "princomp^23",
                        "white", "terasvirta")
for (i in 1:10000)
{
  n <- 200
  e <- rnorm(200)
  x1 <- runif(200, 0, 1)  
  x2 <- runif(200, 0, 1) 
  x3 <- runif(200, 0, 1) 
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
  
  result200[i,1] <- prr2$p.value
  result200[i,2] <- prr3$p.value
  result200[i,3] <- prr23$p.value
  result200[i,4] <- prf2$p.value
  result200[i,5] <- prf3$p.value
  result200[i,6] <- prf23$p.value
  result200[i,7] <- prp2$p.value
  result200[i,8] <- prp3$p.value
  result200[i,9] <- prp23$p.value
  result200[i,10] <- pw$p.value
  result200[i,11] <- pt$p.value
}

write.csv(result200, "hasil20010000.csv")

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
  x[i]=length(result200[,i][result200[,i]>0.05])
  persen_benar[i]=x[i]/10000*100
}

z200=data.frame(uji_statistik, persen_benar,row.names = NULL)
z200
hasi200linier=write.csv(z200,"hasil200linier.csv")


grafikz100=ggplot(z100,aes(z100$uji_statistik,z100$persen_benar))+geom_col(aes(color="blue"))
grafikz100
