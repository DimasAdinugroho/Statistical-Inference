exp.distr = function(x){
  simulasi <- 1000 #banyaknya simulasi yang dilakukan
  n <- 40 #banyaknya sample per simulasi
  lambda <- 0.2 #disetting untuk semua simulasi 
  set.seed(1112) #random number generator
  sim_exp <- matrix(data = rexp(simulasi*n,lambda), ncol = n, nrow = simulasi, byrow = TRUE)
  sim_mean <- rowMeans(sim_exp) #mencari nilai rata rata dari tiap simulasi
  
  mean_theory <- 1/lambda
  var_theory <- 1/(lambda^2*n) 
  
  #menggambar dengan normal distribution secara theory
  x <- seq(min(sim_mean), max(sim_mean), length.out = 100)
  y <- dnorm(x, mean = mean_theory, sd = sqrt(var_theory))
  hist(sim_mean, probability = T)
  lines(x,y, pch=22, col="black", lty=5)
}

tootgroth = function(x) {
  #perbedaan antara VC dan OJ 
  p <- ggplot(ToothGrowth, aes(factor(supp), len, fill = supp))
  p + geom_boxplot()
  
  #perbedaan antara VC dan OJ per dosis 
  q <- ggplot(ToothGrowth, aes(factor(dose), len, fill = supp))
  q + geom_bar(stat = 'identity') + facet_grid(. ~ supp)
  
}
library(datasets)
data(ToothGrowth)
OJ.len<- ToothGrowth[ToothGrowth$supp == 'OJ',]$len
VC.len<- ToothGrowth[ToothGrowth$supp == 'VC',]$len
n <- 30
sOJ <- sd(OJ$len)
mOJ <- mean(OJ$len)
sVC <- sd(VC$len)"conf.int"
mVC <- mean(VC$len)
ttest <- t.test(OJ.len,VC.len, alternative = "two.sided")
SE <- sqrt((sOJ^2/n)+(sVC^2/n))
difference <- OJ$len - VC$len #estimasi dari difference of populasi

