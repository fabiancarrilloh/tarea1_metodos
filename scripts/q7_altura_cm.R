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
  mutate(height_cm = height * 2.54)

m_in <- lm(earnings ~ height, data = df)
m_cm <- lm(earnings ~ height_cm, data = df)

ct_in <- coeftest(m_in, vcov = vcovHC(m_in, type = "HC1"))
ct_cm <- coeftest(m_cm, vcov = vcovHC(m_cm, type = "HC1"))

tidy_in <- tidy(ct_in)
tidy_cm <- tidy(ct_cm)

r2_in <- summary(m_in)$r.squared
r2_cm <- summary(m_cm)$r.squared
n     <- nobs(m_in)

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
  Variable = c(
    "Constante",
    "Altura (pulgadas)",
    "Altura (centímetros)"
  ),
  `Modelo con pulgadas` = c(
    fmt_row(
      tidy_in$estimate[tidy_in$term == "(Intercept)"],
      tidy_in$std.error[tidy_in$term == "(Intercept)"],
      tidy_in$p.value[tidy_in$term == "(Intercept)"]
    ),
    fmt_row(
      tidy_in$estimate[tidy_in$term == "height"],
      tidy_in$std.error[tidy_in$term == "height"],
      tidy_in$p.value[tidy_in$term == "height"]
    ),
    "—"
  ),
  `Modelo con centímetros` = c(
    fmt_row(
      tidy_cm$estimate[tidy_cm$term == "(Intercept)"],
      tidy_cm$std.error[tidy_cm$term == "(Intercept)"],
      tidy_cm$p.value[tidy_cm$term == "(Intercept)"]
    ),
    "—",
    fmt_row(
      tidy_cm$estimate[tidy_cm$term == "height_cm"],
      tidy_cm$std.error[tidy_cm$term == "height_cm"],
      tidy_cm$p.value[tidy_cm$term == "height_cm"]
    )
  )
)

tabla <- gt(tab_df) |>
  tab_header(
    title = "Tabla 5: OLS ingresos (pulgadas vs centímetros)",
    subtitle = paste(
      "Var. dependiente: Ingreso anual (USD).",
      "EE robustos (HC1)."
    )
  ) |>
  tab_source_note(
    source_note = md(
      paste0(
        "Notas: * p<0.10, ** p<0.05, *** p<0.01. N = ", n,
        ". R² (pulg.) = ", round(r2_in, 3),
        "; R² (cm) = ", round(r2_cm, 3), "."
      )
    )
  )

dir.create("output", showWarnings = FALSE)
gtsave(tabla, "output/tabla5_q7.html")

b_in <- tidy_in$estimate[tidy_in$term == "height"]
se_in <- tidy_in$std.error[tidy_in$term == "height"]
b_cm <- tidy_cm$estimate[tidy_cm$term == "height_cm"]
se_cm <- tidy_cm$std.error[tidy_cm$term == "height_cm"]
a_in <- tidy_in$estimate[tidy_in$term == "(Intercept)"]
a_cm <- tidy_cm$estimate[tidy_cm$term == "(Intercept)"]

resumen <- c(
  "Resultados clave P7:",
  paste0(
    "  Pendiente pulgadas: ", round(b_in, 2),
    " (EE ", round(se_in, 2), ")"
  ),
  paste0(
    "  Pendiente centímetros: ", round(b_cm, 2),
    " (EE ", round(se_cm, 2), ")"
  ),
  paste0(
    "  Chequeo unidades: b_in ~ 2.54*b_cm dif = ",
    round(b_in - 2.54 * b_cm, 6)
  ),
  paste0(
    "  Intercepto pulgadas: ", round(a_in, 2),
    " ; intercepto cm: ", round(a_cm, 2),
    " ; dif = ", round(a_in - a_cm, 6)
  ),
  paste0(
    "  R^2 pulgadas = ", round(r2_in, 6),
    " ; R^2 cm = ", round(r2_cm, 6)
  )
)
writeLines(resumen, "output/q7_resumen.txt")
