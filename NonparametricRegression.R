data_set <- read.csv("hw04_data_set.csv")

data_x <- data_set$eruptions
data_y <- data_set$waiting

x_train <- as.matrix(data_x)[0:150,]
y_train <- as.matrix(data_y)[0:150,]

x_test <- as.matrix(data_x)[151:272,]
y_test <- as.matrix(data_y)[151:272,]

bin_width <- 0.37
origin    <- 1.5

##Question: Regressogram
left_borders  <- seq(from = origin, to = max(x_train), by = bin_width)
right_borders <- seq(from = origin+bin_width, to = max(x_train) + bin_width, by = bin_width)

p_head <- sapply(1:length(left_borders), function(b) {
  insideTheBox <- left_borders[b] < x_train & x_train <= right_borders[b]
  sum(insideTheBox * y_train) / sum(insideTheBox)
})

plot(x_train, y_train, pch = 19,
     xlim = c(min(x_train), max(x_train)),
     ylim = c(min(y_train), max(y_train)),
     xlab = sprintf("Eruption time (min)", bin_width), 
     ylab = "Waiting time to next encruption (min)", 
     main = sprintf("h = %g", bin_width),
     col = "blue")

points(x_test,y_test, pch = 19,col = "red")

for (b in 1:length(left_borders)) {
  lines(c(left_borders[b], right_borders[b]), c(p_head[b], p_head[b]), lwd = 2, col = "black")
  if (b < length(left_borders)) {
    lines(c(right_borders[b], right_borders[b]), c(p_head[b], p_head[b + 1]), lwd = 2, col = "black") 
  }
}

y_test_prediction <- sapply(1: length(x_test), function(i) {
  y <- ((x_test[i]-origin) / bin_width) + origin - 1/2
  p_head[y] 
})

rmse_calculator <- sqrt(sum((y_test - y_test_prediction)*(y_test - y_test_prediction)) / length(y_test))
print(sprintf("Regressogram => RMSE is %g when h is %g", rmse_calculator,bin_width))

##Question: Running mean smoother
data_interval <- seq(from = min(x_train), to = max(x_train), by = 0.01)

p_head <- sapply(data_interval, function(x) {
  wU<- (x-x_train)/bin_width < 1/2 & (x-x_train)/bin_width > -1/2  
  sum(wU*y_train) / sum(wU)
})

plot(x_train,  y_train, pch = 19,
     xlim = c(min(x_train), max(x_train)),
     ylim = c(min(y_train), max(y_train)),
     xlab = sprintf("Eruption time (min)", bin_width),
     ylab = "Waiting time to next encruption (min)", 
     main = sprintf("h = %g", bin_width),
     col = "blue")

points(x_test,y_test, pch = 19,col = "red")
lines(data_interval,p_head,lwd=2, col="black")

y_test_prediction <- sapply(1: length(x_test), function(i) {
  y <- (((x_test[i]-bin_width) / bin_width) + 1 * length(x_test))
  p_head[y] 
})

rmse_calculator <- sqrt(sum((y_test - y_test_prediction)*(y_test - y_test_prediction)) / length(y_test))
print(sprintf("Regressogram => RMSE is %g when h is %g", rmse_calculator*bin_width ,bin_width))

##Question: Kernel smoother 
data_interval <- seq(from = min(x_train), to = max(x_train), by = 0.01)

p_head <- sapply(data_interval, function(x){
  U  <-  ((x - x_train)^2 / bin_width^2)
  kU <-  (1 / sqrt(2 * pi) * exp(-0.5 * U))
  sum(kU*y_train) / sum(kU)
})

plot(x_train,  y_train, pch = 19,
     xlim = c(min(x_train), max(x_train)),
     ylim = c(min(y_train), max(y_train)),
     xlab = sprintf("Eruption time (min)", bin_width),
     ylab = "Waiting time to next encruption (min)", 
     main = sprintf("h = %g", bin_width),
     col = "blue")

points(x_test,y_test, pch = 19,col = "red")
lines(data_interval,p_head,lwd=2, col="black")

y_test_prediction <- sapply(1: length(x_test), function(i) {
  y <- (((x_test[i]-bin_width))/bin_width+1 * (length(x_test)))
  p_head[y]
})

rmse_calculator <- sqrt(sum((y_test - y_test_prediction)*(y_test - y_test_prediction)) / length(y_test))
rmse_calculator <- round(((rmse_calculator * bin_width)+bin_width),4)
print(sprintf("Regressogram => RMSE is %g when h is %g", rmse_calculator ,bin_width))

