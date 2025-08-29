need <- c(
  "tidyverse", "janitor", "readr", "sandwich", "lmtest", "gt", "broom"
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
library(gt)
library(broom)

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

df0 <- read_csv(
  "data/earnings_height.csv",
  show_col_types = FALSE
) |>
  clean_names()
df <- df0 |>
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

m1 <- lm(earnings_thou ~ height, data = df)
m2 <- lm(earnings_thou ~ height + lt_hs + hs + some_col, data = df)

wald_cmp <- waldtest(m1, m2, vcov = function(x) vcovHC(x, type = "HC1"))
n <- nobs(m2)
r2_1 <- summary(m1)$r.squared
r2_2 <- summary(m2)$r.squared
f_val <- wald_cmp$F[2]
p_val <- wald_cmp$`Pr(>F)`[2]
df_restr <- wald_cmp$Df[2]
df_res <- wald_cmp$Res.Df[2]

tab_df <- tibble(
  `Modelo restringido` = "earnings_thou ~ height",
  `Modelo irrestricto` = "earnings_thou ~ height + lt_hs + hs + some_col",
  `Hipótesis nula` = "lt_hs = hs = some_col = 0",
  Test = "Wald robusto (HC1)",
  F = round(f_val, 2),
  `gl restricciones` = df_restr,
  `gl residuales` = df_res,
  `p-valor` = signif(p_val, 3),
  N = n,
  `R² (rest.)` = round(r2_1, 3),
  `R² (irrest.)` = round(r2_2, 3)
)

tabla <- gt(tab_df) |>
  tab_header(
    title = "Tabla 9: Test conjunto dummies educación (hombres)",
    subtitle = paste(
      "Var. dep.: Ingreso anual (US$ miles).",
      "H0: lt_hs = hs = some_col = 0. EE robustos (HC1)."
    )
  )

dir.create("output", showWarnings = FALSE)
gtsave(tabla, "output/tabla9_q12d.html")

resumen <- paste0(
  "Test conjunto educación (hombres), Wald HC1\n",
  "  Modelos: restringido 'height' vs irrestricto 'height + dummies educ'.\n",
  "  F(", df_restr, ", ", df_res, ") = ", round(f_val, 2),
  ", p-valor = ", signif(p_val, 3), ".\n",
  "  R² rest. = ", round(r2_1, 3),
  "; R² irrest. = ", round(r2_2, 3), ".\n",
  if (p_val < 0.05) {
    "  Resultado: se rechaza H0 al 5%. Dummies educación significativas.\n"
  } else {
    "  Resultado: no se rechaza H0 al 5%. 
    No hay evidencia de significancia conjunta.\n"
  }
)
writeLines(resumen, "output/q12d_resumen.txt")
