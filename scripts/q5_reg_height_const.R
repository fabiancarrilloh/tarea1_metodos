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
  clean_names()

m1 <- lm(earnings ~ height, data = df)

ct <- coeftest(m1, vcov = vcovHC(m1, type = "HC1"))
tidy_m1 <- tidy(ct)
r2 <- summary(m1)$r.squared
n  <- nobs(m1)

preds <- tibble(height = c(65, 67, 70)) |>
  mutate(
    earnings_pred = predict(m1, newdata = cur_data_all())
  )

dir.create("output", showWarnings = FALSE)
write_csv(preds, "output/q5_predicciones.csv")

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

tab_df <- tibble(
  Variable = c("Constante", "Altura (pulgadas)"),
  Modelo = c(
    fmt_row(
      tidy_m1$estimate[1],
      tidy_m1$std.error[1],
      tidy_m1$p.value[1]
    ),
    fmt_row(
      tidy_m1$estimate[2],
      tidy_m1$std.error[2],
      tidy_m1$p.value[2]
    )
  )
)

tabla <- gt(tab_df) |>
  tab_header(
    title = "Tabla 3: OLS de ingresos sobre altura",
    subtitle = paste(
      "Var. dependiente: Ingreso anual (USD).",
      "EE robustos (HC1)."
    )
  ) |>
  tab_source_note(
    source_note = md(
      paste0(
        "Notas: * p<0.10, ** p<0.05, *** p<0.01. N = ", n,
        ". R² = ", round(r2, 3), "."
      )
    )
  )

gtsave(tabla, "output/tabla3_q5.html")

resumen <- paste0(
  "Regresión: earnings ~ height + constante\n",
  "Coef. height (robusto): ", round(tidy_m1$estimate[2], 2),
  " (EE ", round(tidy_m1$std.error[2], 2), "), p-valor ",
  signif(tidy_m1$p.value[2], 3), ".\n",
  "R^2 = ", round(r2, 3), ", N = ", n, ".\n\n",
  "Predicciones (USD):\n",
  "  65 pulgadas: ",
  round(preds$earnings_pred[preds$height == 65], 0), "\n",
  "  67 pulgadas: ",
  round(preds$earnings_pred[preds$height == 67], 0), "\n",
  "  70 pulgadas: ",
  round(preds$earnings_pred[preds$height == 70], 0), "\n"
)
writeLines(resumen, "output/q5_resumen.txt")
