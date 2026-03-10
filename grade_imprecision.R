library(knitr)
library(kableExtra)

grade_imprecision <- function(modelo, data,
                               rrr = 0.25, alpha = 0.05, poder = 0.80) {
  
  if (!is.null(modelo$subset) && is.logical(modelo$subset)) {
    idx <- which(modelo$subset)
  } else {
    idx <- seq_len(nrow(data))
  }
  
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
  ci_ratio    <- or_ub / or_lb

  # ── Tabla de resultados ───────────────────────────────────────────────────
  resultado <- data.frame(
    Parámetro = c(
      "OR",  "OR — límite inferior", "OR — límite superior",
      "p control (mediana)",
      "RD × 1000", "RD lb × 1000", "RD ub × 1000",
      "CI ratio", "CI ratio ≤ 2.5",
      "OIS", "N total", "Cumple OIS"
    ),
    Valor = c(
      round(or, 2), round(or_lb, 2), round(or_ub, 2),
      round(p_ctrl, 3),
      round(rd(or,    p_ctrl) * 1000, 1),
      round(rd(or_ub, p_ctrl) * 1000, 1),
      round(rd(or_lb, p_ctrl) * 1000, 1),
      round(ci_ratio, 2),
      ifelse(ci_ratio <= 2.5, "Sí", "No"),
      ois, n_total,
      ifelse(n_total >= ois, "Sí", "No")
    ),
    Interpretación = c(
      "Odds ratio pooled", "Límite inferior IC 95%", "Límite superior IC 95%",
      "Tasa de eventos en control",
      "Diferencia de riesgos (por 1000)", "Límite inferior", "Límite superior",
      "Razón ub/lb del IC del OR",
      ifelse(ci_ratio <= 2.5,
             "No bajar 2 niveles por este criterio",
             "Considerar bajar 2 niveles por imprecisión"),
      "Tamaño óptimo de información",
      "Participantes en el subgrupo",
      ifelse(n_total >= ois,
             "No bajar por imprecisión",
             "Considerar bajar por imprecisión")
    )
  )

  # ── Render en Viewer ──────────────────────────────────────────────────────
  resultado |>
    kable(format = "html", align = c("l", "c", "l"),
          caption = "Evaluación de imprecisión — Core GRADE 2 (Guyatt et al., BMJ 2025)") |>
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "bordered"),
      full_width        = FALSE,
      position          = "left",
      font_size         = 13
    ) |>
    row_spec(0, bold = TRUE, background = "#2C3E50", color = "white") |>
    row_spec(c(9, 12),                               # CI ratio ok y cumple OIS
             bold      = TRUE,
             color     = ifelse(c(ci_ratio <= 2.5, n_total >= ois),
                                "#1A7A4A", "#C0392B")) |>
    pack_rows("Efecto relativo",   1, 3)  |>
    pack_rows("Efecto absoluto",   4, 7)  |>
    pack_rows("Imprecisión GRADE", 8, 12)
}

# Uso
grade_imprecision(mod.colon.ustek, df_colon_ustek)
