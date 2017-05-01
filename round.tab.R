round.tab <- function(x) {
  tab1 <- table(x, useNA = "ifany")
  tab2 <- round(prop.table(table(x, useNA = "ifany" )) * 100, 1)
  tab3 <- cbind(tab1, tab2)
  colnames(tab3) <- c("n", "%")
  tab3
}