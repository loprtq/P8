# Downloading "python"
dir.create("/srv/scratch/shsa19")
keras::install_keras(envname = "/srv/scratch/shsa19/r-miniconda")

# Packages(You need your ovn path to python)
library(tensorflow)
use_python("/Users/sandrasorensen/miniconda3/bin/python")
library(keras)
library(dplyr)
library(scatterplot3d)
library(plotly)
library(rgl)

# Get chosen data
nydata_mimp2 <- readRDS("data_w_2implied_volatility") 

#Sort data 
nydata_mimp2 <- filter(nydata_mimp2, as.numeric(nydata_mimp2$`Stock Price`) >= 99)
nydata_mimp2 <- filter(nydata_mimp2, nydata_mimp2$`Time to Maturity` <= "0.1 days")
nydata_mimp2 <- filter(nydata_mimp2, as.numeric(nydata_mimp2$`Stock Price`) <= 101)

set.seed(13)

# Splitting the data into test and training set
trainset <- as.data.frame(lapply(nydata_mimp2[,c(2,5,8, 4)], as.numeric))[c(sample(length(nydata_mimp2[,1]),8000)), ]
testset <- as.data.frame(lapply(nydata_mimp2[-c(as.numeric(rownames(trainset))), ][, c(2,5,8, 4)], as.numeric))

# Normalizing
x_train <- trainset[,1:2]
max_train_K = max(x_train[,1])
max_train_T = max(x_train[,2])
min_train_K = min(x_train[,1])
min_train_T = min(x_train[,2])

x_test_i <-  testset[,1:2]
y_train <- trainset[,3]
y_test_i <- testset[,3]

#Normalized inputs
Strike.Price = (x_train[,1] - min_train_K)/(max_train_K - min_train_K)
Time.to.Maturity =  (x_train[,2] - min_train_T)/(max_train_T - min_train_T)

x_train <- as.data.frame(cbind(Strike.Price, Time.to.Maturity))

Strike.Price = (x_test_i[,1] - min_train_K)/(max_train_K - min_train_K)
Time.to.Maturity =  (x_test_i[,2] - min_train_T)/(max_train_T - min_train_T)

x_test <- as.data.frame(cbind(Strike.Price, Time.to.Maturity))

# Loading the chosen neural network
mymodel = load_model_hdf5("Færdig2.h5")
mymodelmetrics = readRDS("Færdig2RDS")

#Evaluate model on test set
mymodel %>% evaluate(cbind(x_test$Strike.Price, x_test$Time.to.Maturity),  y_test_i)

# Function needed to determine the risk neutral density (BS derivatives)
d1 = function(S, K, r, v, tau) {(log(S/K) + tau*(r+v^2/2)) / (v*sqrt(tau))}
d2 = function(S, K, r, v, tau) {d1(S, K, r, v, tau) - v*sqrt(tau)}

dCBS_dK1 = D(expression(S * pnorm((log(S/K) + tau*(r+v^2/2)) / (v*sqrt(tau))) - K * exp(-r * tau) * pnorm((log(S/K) + tau*(r+v^2/2)) / (v*sqrt(tau)) - v*sqrt(tau))), "K")
dCBS_dv1 = D(expression(S * pnorm((log(S/K) + tau*(r+v^2/2)) / (v*sqrt(tau))) - K * exp(-r * tau) * pnorm((log(S/K) + tau*(r+v^2/2)) / (v*sqrt(tau)) - v*sqrt(tau))), "v")
dCBS_dK2 <- deriv(dCBS_dK1, c("S", "K", "r", "v", "tau"), func = T)
dCBS_dv2 <- deriv(dCBS_dv1, c("S", "K", "r", "v", "tau"), func = T)
dCBS_dK <- function(S, K, r, v, tau){
  a <- dCBS_dK2(S, K, r, v, tau)[1]
  return(a)
}
dCBS_dv <- function(S, K, r, v, tau){
  b <- dCBS_dv2(S, K, r, v, tau)[1]
  return(b)
}
d2CBS_dK2 <- function(S, K, r, v, tau){
  c <- as.numeric(attr(dCBS_dK2(S, K, r, v, tau), "gradient")[1,2])
  return(c)
}
d2CBS_dv2 <- function(S, K, r, v, tau){
  d <- as.numeric(attr(dCBS_dv2(S, K, r, v, tau), "gradient")[1,4])
  return(d)
}
d2CBS_dKdv <- function(S, K, r, v, tau){
  e <- as.numeric(attr(dCBS_dK2(S, K, r, v, tau), "gradient")[1,4])
  return(e)
}

# Testing if the functions work
d2CBS_dK2(100, 110, 0.0386, 0.2, 0.01)
dCBS_dK(100, 110, 0.0386, 0.2, 0.01)
d2CBS_dv2(100, 110, 0.0386, 0.2, 0.01)
dCBS_dv(100, 110, 0.0386, 0.2, 0.01)
d2CBS_dKdv(100, 110, 0.0386, 0.2, 0.01)

# Defining functions for the implied volatility and its partial derivatives
delta = 1/1000
dv_dK = function(K, tau){
  Kplus <- K + delta
  Kminus <- K - delta
  Knormplus <- (Kplus - min_train_K)/(max_train_K - min_train_K)
  Knormminus <- (Kminus - min_train_K)/(max_train_K - min_train_K)
  taunorm <- (tau - min_train_T)/(max_train_T - min_train_T)
  plus1 <- predict(mymodel, cbind(Knormplus,taunorm))
  minus1 <- predict(mymodel, cbind(Knormminus,taunorm))
  return((plus1-minus1)/(2*delta))
}

d2v_dK2 = function(K, tau){
  f_mærkeplus = dv_dK(K+delta, tau)
  f_mærkeminus = dv_dK(K-delta, tau)
  f_mærkemærke = (f_mærkeplus-f_mærkeminus)/(2*delta)
  return(f_mærkemærke)
}

v <- function(K, tau){
  Knorm <- (K - min_train_K)/(max_train_K - min_train_K)
  taunorm <- (tau - min_train_T)/(max_train_T - min_train_T)
  nul <- predict(mymodel, cbind(Knorm,taunorm))
  return(nul)
}

# Using all the functions above to define the partial derivative of C
d2C_dK2 = function(S, K, r, v, tau){
  du <- d2CBS_dK2(S, K, r, v, tau) + 2*d2CBS_dKdv(S, K, r, v, tau)*dv_dK(K,tau)
  + d2CBS_dv2(S, K, r, v, tau)*(dv_dK(K, tau))^2 + dCBS_dv(S, K, r, v, tau)*d2v_dK2(K, tau)
  return(du)
}


# Defining the risk neutral density and integrating+plotting in different ways
# For the two examples presented in the project.
dd6 = function(S){(d2C_dK2(100, S, 0.0386, v(S, 0.07), 0.07)*exp(0.0386*0.07))}
cunint6 = cubintegrate(dd6, lower = 0, upper = 200, method = "hcubature", maxEval = 500)

s6 = seq(0, 200, 1/2)
t6 = c()
for (i in s6) {
  t6 = cbind(t6,dd6(i))
}
plot(s6,t6, type = "l", xlim = c(0, 200), xlab = "Stock Price", ylab = "Value", col = "steelblue1")
abline(v=100, col = "springgreen3")
legend("topright", lwd=1,legend=c("Risk neutral density",
                                  "Strike Price"), col=c("steelblue1", "springgreen3")  ,cex = 1)

dd5 = function(S){(d2C_dK2(100, S, 0.0386, v(S, 0.02), 0.02)*exp(0.0386*0.02))}
cunint5 = cubintegrate(dd5, lower = 0, upper = 200, method = "hcubature", maxEval = 500)

s = seq(0, 200, 1/2)
t5 = c()
for (i in s) {
  t5 = cbind(t5,dd5(i))
}
plot(s,t5, type = "l", xlim = c(0, 200), xlab = "Stock Price", ylab = "Value", col = "steelblue1")
abline(v=100, col = "springgreen3")
legend("topright", lwd=1,legend=c("Risk neutral density",
                                  "Strike Price"), col=c("steelblue1", "springgreen3")  ,cex = 1)

set.seed(2)
priskt <- testset[c(sample(length(x_test_i[,1]),100)), ]

prisen = c()
for (i in 1:length(priskt[,1])) {
  print(i)
  CKtau <- function(S){
    pris <- (S-priskt$Strike.Price[i])*(d2C_dK2(165, S, 0.0386, v(S,priskt$Time.to.Maturity[i]),  priskt$Time.to.Maturity[i])*exp(0.0386*priskt$Time.to.Maturity[i]))
    return(pris)
  }
  s = seq(priskt$Strike.Price[i], priskt$Strike.Price[i]+200, 1)
  t5 = c()
  for (j in s) {
    t5 = cbind(t5,CKtau(j))
  }
  summen = sum(t5)
  prisen = cbind(prisen, summen)
}

par(mfrow=c(1,2))
plot(priskt$Strike.Price, col = "brown1", ylab = "Strike Price")
plot(priskt$Time.to.Maturity, col ="brown1", ylab = "Time to Maturity")

par(mfrow=c(1,2))
plot(simplify2array(priskt$Option.Price), ylab = "Price")
lines(simplify2array(exp(-0.0386*priskt$Time.to.Maturity)*prisen), col = "brown1", type = "p")
legend("topright", lwd=1,legend=c("Real Price",
                                  "Calculated Price"), col=c("black", "brown1")  ,cex = 1, pch = 1, lty = FALSE)
error = priskt$Option.Price-(exp(-0.0386*priskt$Time.to.Maturity)*prisen)
plot(error, type = "l", ylab = "Error", col = "brown1")
#means
mean(error)
mean(abs(error)/(simplify2array(prisen))*100)
mean(abs(error)/(simplify2array(priskt$Option.Price))*100)

par(mfrow=c(1,2))
prisen = cbind(priskt$Strike.Price, prisen)
plot(priskt$Strike.Price, priskt$Option.Price, ylab = "Price", xlab = "Strike Price", ylim = c(-5,100))
lines(priskt$Strike.Price, prisen[,2], col = "brown1", type = "p")
legend("topright", lwd=1,legend=c("Real Price",
                                  "Calculated Price"), col=c("black", "brown1")  ,cex = 1, pch = 1, lty = FALSE)
plot(priskt$Time.to.Maturity, priskt$Option.Price)
lines(priskt$Time.to.Maturity, prisen[,2], col = "red", type = "p")
legend("topleft", lwd=1,legend=c("Real Price",
                                 "Calculated Price"), col=c("black", "brown1")  ,cex = 1, pch = 1, lty = FALSE)
plot(priskt$Strike.Price, error, type = "p", ylab = "Error", xlab = "Strike Price", col = "brown1")
plot(priskt$Time.to.Maturity, error, type = "p", ylab = "Error", xlab = "Time to Maturity", col = "brown1")

