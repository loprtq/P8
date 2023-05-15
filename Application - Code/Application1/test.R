FLAGS <- flags(
  flag_numeric("dropout1", 0.3),
  flag_numeric("dropout2", 0.3),
  flag_numeric("dropout3", 0.2),
  flag_numeric("learningrate", 0.00008),
  flag_numeric("b1", 0.9),
  flag_numeric("b2", 0.99)
)

tensorflow::set_random_seed(65)
model <- keras_model_sequential()
model %>% 
  layer_dense(name = "First_hidden_layer", units = 110, activation = "relu", input_shape = c(2)) %>%
  layer_dropout(rate = FLAGS$dropout1) %>%
  layer_dense(name = "Second_hidden_layer", units = 110, activation = "relu") %>%
  layer_dropout(rate = FLAGS$dropout2) %>%
  layer_dense(name = "Third_hidden_layer", units = 110, activation = "relu") %>%
  layer_dropout(name = "Dropout_layer", rate = FLAGS$dropout3) %>%
  layer_dense(name = "Output_layer", units = 1)

model %>% compile(optimizer = optimizer_adam(learning_rate = FLAGS$learningrate, beta_1 = FLAGS$b1, beta_2 = FLAGS$b2), 
                  loss = 'mse', 
                  metrics = list('mae'))
mymodel <- model %>% 
  fit(cbind(x_train$Strike.Price, x_train$Time.to.Maturity), 
      y_train, epochs = 200, batch_size = 32, validation_split = 0.2)

model2 <- model %>% evaluate(cbind(x_test$Strike.Price, x_test$Time.to.Maturity),  y_test_i)

cat(model2)

