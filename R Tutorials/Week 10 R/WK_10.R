a <- c(1, FALSE)
print(a)

b <- c('a', 1)
print(b)

c <- c(list(1), 'a')
print(c)

d <- c(TRUE, 1L)
print(d)

y <- 10
f1 <- function(x) {
  function() {
    x + 15
  }
}
print(f1(1)())

x <- 1:10
y <- x^2
df <- data.frame(x, y)
plot(df, col='blue')

f2 <- function(x) {
  (x - 32) * 5/9
}
farenheit <- 0:100
celsius <- f2(farenheit)
temps <- data.frame(farenheit, celsius)
plot(temps, xlab='Farenheit', ylab='Celsius', col='red')

data <- read.csv('data.csv')
class(data)

rescale <- function(x) {
  rescaled_values <- numeric(length(x))
  for (i in 1:length(x)) {
    rescaled_values[i] <- (x[i] - min(x))/(max(x) - min(x))
  }
  return(rescaled_values)
}

data2 <- lapply(data, rescale)

print(data2)
write.csv(data2, file = 'NewData.csv', row.names = FALSE)