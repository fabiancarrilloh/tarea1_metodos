need <- c(
  "tidyverse", "janitor", "readr", "broom",
  "gt", "sandwich", "lmtest"
)
to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)

library(tidyverse)
library(janitor)
library(readr)
library(broom)
library(gt)
library(sandwich)
library(lmtest)

df <- read_csv(
  "data/earnings_height.csv",
  show_col_types = FALSE
) |>
  clean_names() |>
  mutate(earnings_thou = earnings / 1000)

m_con <- lm(earnings_thou ~ height, data = df)
m_sin <- lm(earnings_thou ~ 0 + height, data = df)
ct_con <- coeftest(m_con, vcov = vcovHC(m_con, type = "HC1"))
ct_sin <- coeftest(m_sin, vcov = vcovHC(m_sin, type = "HC1"))

tidy_con <- tidy(ct_con)
tidy_sin <- tidy(ct_sin)

r2_con <- summary(m_con)$r.squared
r2_sin <- summary(m_sin)$r.squared
n <- nobs(m_con)

fmt_row <- function(est, se, p) {
  stars <- ifelse(
    p < 0.01, "***",
    ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", ""))
  )
  paste0(
    format(round(est, 2), nsmall = 2), stars, "\n(",
    format(round(se, 2), nsmall = 2), ")"
  )
}

const_con <- fmt_row(
  tidy_con$estimate[tidy_con$term == "(Intercept)"],
  tidy_con$std.error[tidy_con$term == "(Intercept)"],
  tidy_con$p.value[tidy_con$term == "(Intercept)"]
)

beta_con <- fmt_row(
  tidy_con$estimate[tidy_con$term == "height"],
  tidy_con$std.error[tidy_con$term == "height"],
  tidy_con$p.value[tidy_con$term == "height"]
)

beta_sin <- fmt_row(
  tidy_sin$estimate[tidy_sin$term == "height"],
  tidy_sin$std.error[tidy_sin$term == "height"],
  tidy_sin$p.value[tidy_sin$term == "height"]
)

tab_df <- tibble(
  Variable = c("Constante", "Altura (pulgadas)"),
  `Con constante` = c(const_con, beta_con),
  `Sin constante` = c("—",        beta_sin)
)

tabla <- gt(tab_df) |>
  tab_header(
    title = "Tabla 4: OLS ingresos vs altura (con/sin constante)",
    subtitle = paste(
      "Var. dependiente: Ingreso anual (US$ miles).",
      "EE robustos (HC1)."
    )
  ) |>
  tab_source_note(
    source_note = md(
      paste0(
        "Notas: * p<0.10, ** p<0.05, *** p<0.01. N = ", n,
        ". R² (con) = ", round(r2_con, 3),
        "; R² (sin, no centrado) = ", round(r2_sin, 3), "."
      )
    )
  )

dir.create("output", showWarnings = FALSE)
gtsave(tabla, "output/tabla4_q6.html")

beta_con_num <- tidy_con$estimate[tidy_con$term == "height"]
beta_sin_num <- tidy_sin$estimate[tidy_sin$term == "height"]
p_constante <- tidy_con$p.value[tidy_con$term == "(Intercept)"]

resumen <- paste0(
  "Comparación de pendientes height:\n",
  "  Con constante: ", round(beta_con_num, 2),
  " (EE robusto ",
  round(tidy_con$std.error[tidy_con$term == "height"], 2), ")\n",
  "  Sin constante: ", round(beta_sin_num, 2),
  " (EE robusto ",
  round(tidy_sin$std.error[tidy_sin$term == "height"], 2), ")\n",
  "Diferencia numérica (sin - con): ",
  round(beta_sin_num - beta_con_num, 2), "\n\n",
  "Test de incluir constante: H0: Intercepto = 0 en el modelo con constante.\n",
  "  p-valor = ", signif(p_constante, 3), ".\n",
  "R² con constante = ", round(r2_con, 3),
  "; R² sin constante (no centrado) = ", round(r2_sin, 3), ".\n"
)
writeLines(resumen, "output/q6_resumen.txt")
