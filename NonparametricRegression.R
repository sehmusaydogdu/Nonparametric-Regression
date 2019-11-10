data_set <- read.csv("hw04_data_set.csv")

data_x <- data_set$eruptions
data_y <- data_set$waiting

x_train <- as.matrix(data_x)[0:150,]
y_train <- as.matrix(data_y)[0:150,]

x_test <- as.matrix(data_x)[151:272,]
y_test <- as.matrix(data_y)[151:272,]

N         <- length(y_train)
bin_width <- 0.37
origin    <- 1.5

point_colors <- c("blue", "red")

minimum_value <- -8
maximum_value <- +8

data_interval <- seq(from = minimum_value, to = maximum_value, by = 0.01)

left_borders  <- seq(from = minimum_value, to = maximum_value - bin_width, by = bin_width)
right_borders <- seq(from = minimum_value + bin_width, to = maximum_value, by = bin_width)

p_head <- sapply(1:length(left_borders), function(b) {sum(left_borders[b] < x_train & x_train <= right_borders[b])}) / (N * bin_width)


plot(x_train, y_train,  pch = 19,lwd=1,
     xlim = c(min(x_train), max(x_train)),
     ylim = c(min(y_train), max(y_train)),
     ylab = "Waiting time to next encruption (min)", 
     xlab = sprintf("Eruption time (min)", bin_width), 
     main = sprintf("h = %g", bin_width), col = c("red", "blue"))

for (b in 1:length(left_borders)) {
  lines(c(left_borders[b], right_borders[b]), c(p_head[b], p_head[b]), lwd = 2, col = "black")
  if (b < length(left_borders)) {
    lines(c(right_borders[b], right_borders[b]), c(p_head[b], p_head[b + 1]), lwd = 2, col = "black") 
  }
}

