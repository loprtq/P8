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

# Get data
nydata_mimp2 <- readRDS("data_w_2implied_volatility") 

#Sort data 
nydata_mimp2 <- filter(nydata_mimp2, as.numeric(nydata_mimp2$`Stock Price`) >= 99)
nydata_mimp2 <- filter(nydata_mimp2, nydata_mimp2$`Time to Maturity` <= "0.1 days")
nydata_mimp2 <- filter(nydata_mimp2, as.numeric(nydata_mimp2$`Stock Price`) <= 101)

# Plot data
plotdata <- as.data.frame(lapply(nydata_mimp2[,c(2,5,8)], as.numeric))
fig2 <- plot_ly() %>% 
  add_markers(x=plotdata$Time.to.Maturity, y=plotdata$Strike.Price, z=plotdata$Implied.Volatility.fkt, marker=list(size=3, color=plotdata$Implied.Volatility.fkt, colorscale=list(c(0, 0.32,1), c("black", "red", "pink")))) %>%
  layout(title = "", scene = list(xaxis=list(title="Time to Maturity"), yaxis=list(title="Strike Price"), zaxis=list(title="Implied Volatility")))
fig2

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

# Create Neural Network
tensorflow::set_random_seed(65)
model2 <- keras_model_sequential()
model2 %>% 
  layer_dense(units = 110, activation = "relu", input_shape = c(2)) %>%
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
  fit(cbind(x_train$Strike.Price, x_train$Time.to.Maturity), 
      y_train, epochs =1200, batch_size = 32, validation_split = 0.2)

# Saving the Network or loading
#save_model_hdf5(model2, "Færdig2.h5")
#saveRDS(models, "Færdig2RDS")
mymodel = load_model_hdf5("Færdig2.h5")
mymodelmetrics = readRDS("Færdig2RDS")

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
     xlab='Epoch')
lines(models$metrics$mape, type="l", col="steelblue1")
legend("topright", lwd=1,legend=c("MAPE",
                                  "MAPE on validation set"), col=c("steelblue1", "springgreen3")  ,cex = 1)

#Evaluate model on test set
model2 %>% evaluate(cbind(x_test$Strike.Price, x_test$Time.to.Maturity),  y_test_i)

# Plot prediction wrt real values
predictions <- predict(mymodel, cbind(x_test$Strike.Price, x_test$Time.to.Maturity))
plot(y_test_i, predictions, ylab = "Predictions", xlab = "Real Value",
     pch = 1, cex = 0.7, col = "brown1", xlim = c(0.3,1.4), ylim = c(0.3,1.4))
abline(0,1)
abline(lm(predictions~y_test_i), col="blue")
summary(lm(predictions~y_test_i))

## Trying exp decay i Adam - optimizer 
# Define a function that returns the learning rate for each epoch
lr_decay <- function(epoch, lr) {
  lr * exp(-0.1)
} 

# Define the LearningRateScheduler callback
lr_scheduler <- callback_learning_rate_scheduler(lr_decay)

tensorflow::set_random_seed(65)
model2 <- keras_model_sequential()
model2 %>% 
  layer_dense(units = 110, activation = "relu", input_shape = c(2)) %>%
  layer_dense(units = 110, activation = "relu") %>%
  layer_dense(units = 110, activation = "relu") %>%
  layer_dense(units = 1)
summary(model2)

model2 %>% compile(optimizer = optimizer_adam(learning_rate = 0.001), 
                   loss = 'mse', 
                   metrics = list('mae'))

mymodel2 <- model2 %>% 
  fit(cbind(x_train$Strike.Price, x_train$Time.to.Maturity), 
      y_train, epochs = 200, batch_size = 32, validation_split = 0.2, callbacks = list(lr_scheduler))

# Evaluate and plot result
model2 %>% evaluate(cbind(x_test$Strike.Price, x_test$Time.to.Maturity),  y_test_i)
predictions <- predict(model2, cbind(x_test$Strike.Price, x_test$Time.to.Maturity))
plot(y_test_i, predictions)


#RUNS - using tuning_run to choose best model
# Need the file test.R in your working directory
library(tfruns)
runsa <- tuning_run("test.R", sample = 0.3, flags = list(
  dropout1 = c(0, 0.2, 0.5),
  dropout2 = c(0, 0.2, 0.5),
  dropout3 = c(0, 0.2, 0.5),
  learningrate = c(0.0001, 0.0005, 0.001),
  b1 = c(0.8, 0.85, 0.9),
  b2 = c(0.9, 0.95, 0.99)
))
# Find the best evaluation accuracy and saving results
dub = runsa[order(runsa$eval_, decreasing = TRUE), ]
saveRDS(dub, "nytuninga")


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
  layout(scene = list(xaxis=list(title="Time to Maturity"), yaxis=list(title="Strike Price"), zaxis=list(title="Implied Volatility")))
fig

# Plot surface with all points
fig2 <- plot_ly() %>% 
  add_surface(x=hej7$V1, y=hej7$V2, z=matrix(nul, length(hej7$V1), length(hej7$V2)), colorscale = list(c(0, 1), c("black", "red")), showscale=FALSE, showlegend = FALSE) %>%
  add_markers(x=plotdata$Time.to.Maturity, y=plotdata$Strike.Price, z=plotdata$Implied.Volatility.fkt, marker=list(size=1, color="orange")) %>%
  layout(title = "", scene = list(xaxis=list(title="Time to Maturity"), yaxis=list(title="Strike Price"), zaxis=list(title="Implied Volatility")))
fig2

# Determining the partial derivatives of the implied volatility for plotting(Delete maybe?)
delta = 1/1000
Tfrom = 0
Tto = 1
Tantal = 100
delta_n = (Tto-Tfrom)/(Tantal-1)
T = seq(Tfrom,Tto,delta_n)

Kfrom = 0 + delta
Kto = 1 - delta
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
minus1 = predict(mymodel, cbind(Strike-delta,Tvec))
plus1 = predict(mymodel, cbind(Strike+delta,Tvec))

f_mærke =(plus1-minus1)/(2*delta)
fig <- plot_ly() %>% 
  add_surface(x=hej7$V1, y=hej7$V2, z=matrix(f_mærke, length(hej7$V1), length(hej7$V2)), colorscale = list(c(0, 1), c("black", "red")), showscale=FALSE, showlegend = FALSE) %>%
  layout(scene = list(xaxis=list(title="Time to Maturity"), yaxis=list(title="Strike Price"), zaxis=list(title="Implied Volatility")))
fig


f_mærkemærke = c()
for (i in 2:(length(f_mærke)-1)) {
  f_mærkemærke = cbind(f_mærkemærke, (f_mærke[i+1]-f_mærke[i-1])/(2*delta))
}

f_mærkemærke = cbind(1.24, f_mærkemærke, -0.005)
fig <- plot_ly() %>% 
  add_surface(x=hej7$V1, y=hej7$V2, z=matrix(f_mærkemærke, length(hej7$V1), length(hej7$V2)), colorscale = list(c(0, 1), c("black", "red")), showscale=FALSE, showlegend = FALSE) %>%
  layout(scene = list(xaxis=list(title="Time to Maturity"), yaxis=list(title="Strike Price"), zaxis=list(title="Implied Volatility")))
fig