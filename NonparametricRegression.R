data_set <- read.csv("hw04_data_set.csv")

data_x <- data_set$eruptions
data_y <- data_set$waiting

x_train <- as.matrix(data_x)[0:150,]
y_train <- as.matrix(data_y)[0:150,]

x_test <- as.matrix(data_x)[151:272,]
y_test <- as.matrix(data_y)[151:272,]

bin_width <- 0.37
origin    <- 1.5

left_borders  <- seq(from = origin, to = max(x_train), by = bin_width)
right_borders <- seq(from = origin+bin_width, to = max(x_train) + bin_width, by = bin_width)

p_head <- sapply(1:length(left_borders), function(b) {
  insideTheBox <- left_borders[b] < x_train & x_train <= right_borders[b]
  sum(insideTheBox * y_train) / sum(insideTheBox)
})

plot(x_train, y_train, pch = 19,
     xlim = c(min(x_train), max(x_train)),
     ylim = c(min(y_train), max(y_train)),
     ylab = "Waiting time to next encruption (min)", 
     xlab = sprintf("Eruption time (min)", bin_width), 
     main = sprintf("h = %g", bin_width),
     col = "blue")

points(x_test,y_test, pch = 19,col = "red")

for (b in 1:length(left_borders)) {
  lines(c(left_borders[b], right_borders[b]), c(p_head[b], p_head[b]), lwd = 2, col = "black")
  if (b < length(left_borders)) {
    lines(c(right_borders[b], right_borders[b]), c(p_head[b], p_head[b + 1]), lwd = 2, col = "black") 
  }
}

y_test_prediction <- sapply(1:length(x_test), function(b) {p_head[((x_test[b] - origin) / bin_width) + 1]})

rmse_calculator <- sqrt(sum((y_test - y_test_prediction)*(y_test - y_test_prediction)) / length(y_test))
print(sprintf("Regressogram => RMSE is %g when h is %g", rmse_calculator ,bin_width))



