data1 <- read.table('data.dat.txt', header = TRUE)
print(data1)
plot(data1)
cov(data1$x, data1$y)
cor(data1$x, data1$y)


generate_data <- function(nsamples, intercept, slope, noise) {
  x <- rnorm(nsamples)
  y <- x*slope + rnorm(nsamples)*noise + intercept
  return(data.frame(x, y))
}

dev.off()
library(stats)
noise_levels <- c(0.1, 1, 10, 100)
par(mfrow=c(2,2))
for (noise in noise_levels) {
  data <- generate_data(1000, 0, 1, noise)
  print(paste("Noise Level:", noise))
  plot(ccf(data$x, data$y), main=paste("Noise Level:", noise))
}


Regression_data <- generate_data(1000, 0, 1, 0.5)
lm_result <- lm(Regression_data)
summary(lm_result)
plot(Regression_data)
abline(lm_result)

install.packages('car')
library('car')
qqPlot(lm_result$residuals, main=paste('Regression Residuals'))

predicted_values <- predict(lm_result)
plot(predicted_values)
plot(Regression_data, main = "Scatter Plot of Data with Predicted Line", 
     xlab = "Predictor", ylab = "Response")
lines(Regression_data[,1], predicted_values, col = "red")
legend("topright", legend = c("Predicted"), col = "red", lty = 1)


generate_data_with_nonlinearity <- function(nsamples, intercept, slope, slope2, noise) {
  x <- rnorm(nsamples)
  x2 <- x^2
  y <- intercept + slope * x + slope2 * x2 + rnorm(nsamples, mean = 0, sd = noise)  
  return(data.frame(x, x2, y))
}

data_nonlinear <- generate_data_with_nonlinearity(1000, 0, 1, 1, 0.5)
model_nonlinear <- lm(y ~ x + x2, data = data_nonlinear)
summary(model_nonlinear)

plot(data_nonlinear$x, data_nonlinear$y, main = "Scatter Plot with Nonlinear Model",
     xlab = "Predictor x", ylab = "Response y")
points(data_nonlinear$x, fitted(model_nonlinear), col = "red", pch = 20)
lines(sort(data_nonlinear$x), predict(model_nonlinear)[order(data_nonlinear$x)], col = "blue")
legend("topright", legend = c("Fitted values", "Nonlinear Model"), col = c("red", "blue"), pch = c(20, NA), lty = c(NA, 1))

library(car)
qqPlot(model_nonlinear$residuals, main = "QQ Plot of Nonlinear Model Residuals")
print(summary(model_nonlinear)$coefficients)


data <- data.frame(
  x = c(1, 2, 3, 4, 5),
  y = c(2, 4, 9, 16, 25)
)

model <- nls(y ~ a * x^b, data = data, start = list(a = 1, b = 1))
summary(model)

new_data <- data.frame(x = c(6, 7, 8))
predictions <- predict(model, newdata = new_data)

print(predictions)
