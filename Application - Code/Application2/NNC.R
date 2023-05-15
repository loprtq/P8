# Downloading "python"
dir.create("/srv/scratch/shsa19")
keras::install_keras(envname = "/srv/scratch/shsa19/r-miniconda")

# Packages(You need your ovn path to python)
library(tensorflow)
use_python("/srv/scratch/shsa19/r-miniconda/bin/python")
library(keras)
library(dplyr)
library(scatterplot3d)
library(plotly)
library(rgl)

# Get data and its the data for when S is input
nydata_mimp2 <- readRDS("data_w_2implied_volatility") 

#Sort data 
nydata_mimp2 <- filter(nydata_mimp2, as.numeric(nydata_mimp2$`Stock Price`) >= 99)
nydata_mimp2 <- filter(nydata_mimp2, nydata_mimp2$`Time to Maturity` <= "0.1 days")
nydata_mimp2 <- filter(nydata_mimp2, as.numeric(nydata_mimp2$`Stock Price`) <= 101)

#Sort data no restraint on time to maturity
nydata_mimp2 <- filter(nydata_mimp2, as.numeric(nydata_mimp2$`Stock Price`) >= 99)
nydata_mimp2 <- filter(nydata_mimp2, as.numeric(nydata_mimp2$`Stock Price`) <= 101)

# Plot data
plotdata <- as.data.frame(lapply(nydata_mimp2[,c(2,5,4,6)], as.numeric))
fig2 <- plot_ly() %>% 
  add_markers(x=plotdata$Time.to.Maturity, y=plotdata$Strike.Price, z=plotdata$Option.Price, marker=list(size=3, color=plotdata$Option.Price, colorscale=list(c(0, 0.32,1), c("black", "red", "pink")))) %>%
  layout(title = "", scene = list(xaxis=list(title="Time to Maturity"), yaxis=list(title="Strike Price"), zaxis=list(title="Option Price")))
fig2

set.seed(13)

# Splitting the data into test and training set
trainset <- as.data.frame(lapply(nydata_mimp2[,c(2,5,4, 6)], as.numeric))[c(sample(length(nydata_mimp2[,1]), 145000)), ]
testset <- as.data.frame(lapply(nydata_mimp2[-c(as.numeric(rownames(trainset))), ][, c(2,5,4, 6)], as.numeric))

# Normalizing without S as input
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


# Normalizing with S as input
x_train <- trainset[,c(1:2,4)]
max_train_K = max(x_train[,1])
max_train_T = max(x_train[,2])
max_train_S = max(x_train[,3])
min_train_K = min(x_train[,1])
min_train_T = min(x_train[,2])
min_train_S = min(x_train[,3])

x_test_i <-  testset[,c(1:2,4)]
y_train <- trainset[,3]
y_test_i <- testset[,3]

#Normalized inputs
Strike.Price = (x_train[,1] - min_train_K)/(max_train_K - min_train_K)
Time.to.Maturity =  (x_train[,2] - min_train_T)/(max_train_T - min_train_T)
Stock.Price =  (x_train[,3] - min_train_S)/(max_train_S - min_train_S)

x_train <- as.data.frame(cbind(Strike.Price, Time.to.Maturity, Stock.Price))

Strike.Price = (x_test_i[,1] - min_train_K)/(max_train_K - min_train_K)
Time.to.Maturity =  (x_test_i[,2] - min_train_T)/(max_train_T - min_train_T)
Stock.Price =  (x_test_i[,3] - min_train_S)/(max_train_S - min_train_S)

x_test <- as.data.frame(cbind(Strike.Price, Time.to.Maturity, Stock.Price))


# Create Neural Network
tensorflow::set_random_seed(65)
model2 <- keras_model_sequential()
model2 %>% 
  layer_dense(units = 110, activation = "relu", input_shape = c(3)) %>%
  layer_dropout(rate = 0.0) %>%
  layer_dense(units = 110, activation = "relu") %>%
  layer_dropout(rate = 0.0) %>%
  layer_dense(units = 110, activation = "relu") %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1)

opt_adam = optimizer_adam(learning_rate = 0.0005, beta_1 = 0.80, beta_2 = 0.9)
model2 %>% compile(optimizer = opt_adam, 
                   loss = 'mse', 
                   metrics = list('mae', 'mape'))
models <- model2 %>% 
  fit(cbind(x_train$Strike.Price, x_train$Time.to.Maturity, x_train$Stock.Price), 
      y_train, epochs =1200, batch_size = 32, validation_split = 0.2)

# Saving the Network or loading
#save_model_hdf5(model2, "NNC32.h5")
#saveRDS(models, "NNC32RDS")
mymodel = load_model_hdf5("NNC.h5")
models = readRDS("NNCRDS")

# Plotting the loss, mae and mape 
par(mfrow=c(1,3))
plot(models$metrics$val_loss, col="springgreen3", type="l", ylab='Value',
     xlab='Epoch')
lines(models$metrics$loss, type="l", col="steelblue1")
legend("topright", lwd=1,legend=c("MSE",
                                  "MSE on validation set"), col=c("steelblue1", "springgreen3") ,cex = 1)
plot(models$metrics$val_mae, col="springgreen3", type="l", ylab='Value',
     xlab='Epoch')
lines(models$metrics$mae, type="l", col="steelblue1")
legend("topright", lwd=1,legend=c("MAE",
                                  "MAE on validation set"), col=c("steelblue1", "springgreen3")  ,cex = 1)


plot(models$metrics$val_mape, col="springgreen3", type="l", ylab='Value',
     xlab='Epoch', ylim = c(8,50))
lines(models$metrics$mape, type="l", col="steelblue1")
legend("topright", lwd=1,legend=c("MAPE",
                                  "MAPE on validation set"), col=c("steelblue1", "springgreen3")  ,cex = 1)

#Evaluate model on test set
mymodel %>% evaluate(cbind(x_test$Strike.Price, x_test$Time.to.Maturity, x_test$Stock.Price),  y_test_i)

# Plot prediction wrt real values
predictions <- predict(mymodel, cbind(x_test$Strike.Price, x_test$Time.to.Maturity, x_test$Stock.Price))
plot(y_test_i, predictions, ylab = "Predictions", xlab = "Real Value",
     pch = 1, cex = 0.7, col = "brown1")
abline(0,1)
abline(lm(predictions~y_test_i), col="blue")
summary(lm(predictions~y_test_i))

#Creating and plotting volatility surface
mymodel = model2
Tfrom = 0
Tto = 1
Tantal = 100
delta_n = (Tto-Tfrom)/(Tantal-1)
T = seq(Tfrom,Tto,delta_n)

Kfrom = 0
Kto = 1
Kantal = 100
delta_m = (Kto-Kfrom)/(Kantal-1)
K = seq(Kfrom,Kto,delta_m)

Tvec <- c()
for (i in 1:Tantal) {
  Tvec <- append(Tvec,c(rep(T[i], Kantal)))
}

Strike <- c(rep(K, Tantal))
hej3 <- as.data.frame(cbind(Tvec*(max_train_T - min_train_T)+min_train_T,Strike*(max_train_K - min_train_K)+min_train_K))

nul <- predict(mymodel, cbind(Strike,Tvec))
hej7 <- as.data.frame(cbind(T*(max_train_T - min_train_T)+min_train_T,K*(max_train_K - min_train_K)+min_train_K))

# Plot surface with test points
fig <- plot_ly() %>% 
  add_surface(x=hej7$V1, y=hej7$V2, z=matrix(nul, length(hej7$V1), length(hej7$V2)), colorscale = list(c(0, 1), c("black", "red")), showscale=FALSE, showlegend = FALSE) %>%
  add_markers(x=x_test_i$Time.to.Maturity, y=x_test_i$Strike.Price, z=y_test_i, marker=list(size=1, color="orange")) %>%
  layout(scene = list(xaxis=list(title="Time to Maturity"), yaxis=list(title="Strike Price"), zaxis=list(title="Option price")))
fig


# Evaluation of 100 random points
set.seed(2)
priskt <- testset[c(sample(length(x_test_i[,1]),100)), ]
prisen <- predict(mymodel, cbind(((priskt2$Strike.Price- min_train_K)/(max_train_K - min_train_K)), ((priskt2$Time.to.Maturity- min_train_T)/(max_train_T - min_train_T)), ((priskt2$Stock.Price- min_train_S)/(max_train_S - min_train_S))))

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




