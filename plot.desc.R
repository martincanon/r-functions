plot.desc <- function(x, main) {
  par(mfrow = c(2, 2))
  if(missing(main))
    main <- deparse(substitute(x))
  if(grepl("\\$", main)) {
            main <- strsplit(main, "\\$")[[1]][2]
        }
  plot(x, main = main)
  plot(density(x, na.rm = TRUE), main = main)
  hist(x, main = main)
  qqnorm(x, main = main)
  qqline(x, col = "green", lwd = 2, lty = 2)
  par(mfrow = c(1, 1))
}
