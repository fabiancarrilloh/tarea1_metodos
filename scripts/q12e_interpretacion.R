need <- c(
  "tidyverse", "janitor", "readr", "sandwich", "lmtest", "broom", "gt"
)

to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install) > 0) {
  install.packages(to_install, dependencies = TRUE)
}

library(tidyverse)
library(janitor)
library(readr)
library(sandwich)
library(lmtest)
library(broom)
library(gt)

sex_map <- function(x) {
  if (is.numeric(x)) {
    unq <- na.omit(unique(x))
    if (all(unq %in% c(1, 2))) {
      return(factor(x, levels = c(1, 2), labels = c("Hombre", "Mujer")))
    }
    if (all(unq %in% c(0, 1))) {
      return(factor(x, levels = c(1, 0), labels = c("Hombre", "Mujer")))
    }
  }
  if (is.character(x)) {
    xl <- stringr::str_to_lower(x)
    out <- ifelse(
      xl %in% c("male", "m", "hombre"), "Hombre",
      ifelse(xl %in% c("female", "f", "mujer"), "Mujer", NA)
    )
    return(factor(out, levels = c("Hombre", "Mujer")))
  }
  factor(NA_character_, levels = c("Hombre", "Mujer"))
}

df <- read_csv(
  "data/earnings_height.csv",
  show_col_types = FALSE
) |>
  clean_names() |>
  mutate(
    sex = sex_map(sex),
    earnings_thou = earnings / 1000
  ) |>
  filter(
    sex == "Hombre", !is.na(earnings_thou), !is.na(height), !is.na(educ)
  ) |>
  mutate(
    lt_hs = as.integer(educ < 12),
    hs = as.integer(educ == 12),
    some_col = as.integer(educ > 12 & educ < 16),
    college = as.integer(educ >= 16)
  )

m2 <- lm(earnings_thou ~ height + lt_hs + hs + some_col, data = df)
ct2 <- coeftest(m2, vcov = vcovHC(m2, type = "HC1"))
t2 <- tidy(ct2)

educ_terms <- c("lt_hs", "hs", "some_col")
educ_tbl <- t2 |>
  filter(term %in% educ_terms) |>
  transmute(
    categoria = c(
      "Menos que secundaria (<12)",
      "Secundaria (=12)",
      "Algo de universidad (12<educ<16)"
    ),
    est = estimate,
    ee = std.error,
    p = p.value
  )

stars <- function(p) {
  ifelse(
    p < 0.01, "***",
    ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", ""))
  )
}

educ_tbl_fmt <- educ_tbl |>
  mutate(
    `Coeficiente (US$ miles)` = paste0(
      format(round(est, 2), nsmall = 2), stars(p)
    ),
    `EE robusto (HC1)` = format(round(ee, 2), nsmall = 2),
    `p-valor` = signif(p, 3)
  ) |>
  select(
    categoria, `Coeficiente (US$ miles)`, `EE robusto (HC1)`, `p-valor`
  )

tabla <- gt(educ_tbl_fmt) |>
  tab_header(
    title = "Tabla 10: Hombres — dummies educación",
    subtitle = paste(
      "Var. dep.: Ingreso anual (US$ miles). Base: college (educ ≥ 16)."
    )
  ) |>
  tab_source_note(
    source_note = md(
      paste(
        "* p<0.10, ** p<0.05, *** p<0.01. Coeficientes = diferencia",
        "vs base (college) manteniendo altura constante."
      )
    )
  )

dir.create("output", showWarnings = FALSE)
gtsave(tabla, "output/tabla10_q12e.html")

lines <- apply(
  educ_tbl, 1,
  function(r) {
    catg <- r[["categoria"]]
    b <- as.numeric(r[["est"]])
    paste0("• ", catg, ": ", round(b, 2))
  }
)

resumen <- c(
  "Interpretación educación (hombres, modelo height + dummies):",
  paste(
    "Coeficientes = diferencias en ingresos (US$ miles) vs base",
    "college (educ ≥ 16), manteniendo altura constante."
  ),
  "Signo positivo: grupo > college; negativo: grupo < college (ctrl altura).",
  lines
)

hbar <- mean(df$height, na.rm = TRUE)
pred_levels <- tribble(
  ~categoria, ~lt_hs, ~hs, ~some_col,
  "Menos que secundaria (<12)", 1, 0, 0,
  "Secundaria (=12)", 0, 1, 0,
  "Algo de universidad (12<educ<16)", 0, 0, 1,
  "College (>=16)", 0, 0, 0
) |>
  mutate(height = hbar)

pred <- predict(
  m2,
  newdata = select(pred_levels, height, lt_hs, hs, some_col),
  se.fit = TRUE
)

pred_tbl <- pred_levels |>
  transmute(
    categoria,
    pred_usd_miles = pred$fit,
    ee = pred$se.fit
  )

write_csv(pred_tbl, "output/q12e_pred_por_educ.csv")

lines_pred <- paste0(
  "Nivel predicho (altura media) - ",
  pred_tbl$categoria, ": ",
  round(pred_tbl$pred_usd_miles, 2), " US$ miles"
)

resumen <- c(
  resumen,
  "---",
  "Niveles predichos (US$ miles) a altura promedio:",
  lines_pred
)

writeLines(resumen, "output/q12e_resumen.txt")
