grade_imprecision <- function(modelo, data,
                              rrr = 0.25, alpha = 0.05, poder = 0.80) {
  
  idx     <- which(modelo$subset)
  res     <- summary(modelo)
  
  p_ctrl  <- median(data$casosCtr[idx] / data$totalCtr[idx])
  or      <- exp(res$beta[1])
  or_lb   <- exp(res$ci.lb)
  or_ub   <- exp(res$ci.ub)
  
  or_a_rr <- function(o, p) o / ((1 - p) + (p * o))
  rd      <- function(o, p) p - or_a_rr(o, p) * p
  
  p_int       <- p_ctrl * (1 - rrr)
  n_por_grupo <- (qnorm(1 - alpha/2) + qnorm(poder))^2 *
    (p_ctrl * (1 - p_ctrl) + p_int * (1 - p_int)) /
    (p_ctrl - p_int)^2
  ois         <- ceiling(2 * n_por_grupo)
  n_total     <- sum(data$totalTto[idx] + data$totalCtr[idx])
  
  `rownames<-`(data.frame(
    OR          = round(or,                       2),
    OR_lb       = round(or_lb,                    2),
    OR_ub       = round(or_ub,                    2),
    p_ctrl      = round(p_ctrl,                   3),
    RD_x1000    = round(rd(or,    p_ctrl) * 1000, 1),
    RD_lb_x1000 = round(rd(or_ub, p_ctrl) * 1000, 1),
    RD_ub_x1000 = round(rd(or_lb, p_ctrl) * 1000, 1),
    CI_ratio    = round(or_ub / or_lb,            2),
    CI_ratio_ok = or_ub / or_lb <= 2.5,
    OIS         = ois,
    N_total     = n_total,
    cumple_OIS  = n_total >= ois
  ), "")
}

# Uso
grade_imprecision(mod.colon.ustek, df)