plot.desc <- function(x) {
  par(mfrow = c(2, 2))
  plot(density(x, na.rm = TRUE))
  hist(x)
  qqnorm(x)
  qqline(x, col = "green", lwd = 2, lty = 2)
  par(mfrow = c(1, 1))
}