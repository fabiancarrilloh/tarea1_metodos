need <- c(
  "tidyverse", "janitor", "readr", "broom", "gt", "sandwich", "lmtest"
)
to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install) > 0) {
  install.packages(to_install, dependencies = TRUE)
}

library(tidyverse)
library(janitor)
library(readr)
library(broom)
library(gt)
library(sandwich)
library(lmtest)

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
stopifnot(all(c("earnings", "height", "sex", "educ") %in% names(df0)))

df <- df0 |>
  mutate(
    sex = sex_map(sex),
    earnings_thou = earnings / 1000
  ) |>
  filter(
    sex == "Hombre",
    !is.na(earnings_thou), !is.na(height), !is.na(educ)
  ) |>
  mutate(
    lt_hs = as.integer(educ < 12),
    hs = as.integer(educ == 12),
    some_col = as.integer(educ > 12 & educ < 16),
    college = as.integer(educ >= 16)
  )

m1 <- lm(earnings_thou ~ height, data = df)
m2 <- lm(earnings_thou ~ height + lt_hs + hs + some_col, data = df)
ct1 <- coeftest(m1, vcov = vcovHC(m1, type = "HC1"))
ct2 <- coeftest(m2, vcov = vcovHC(m2, type = "HC1"))

tidy1 <- tidy(ct1)
tidy2 <- tidy(ct2)
r21 <- summary(m1)$r.squared
r22 <- summary(m2)$r.squared
n1 <- nobs(m1)
n2 <- nobs(m2)

fmt <- function(est, se, p) {
  stars <- ifelse(
    p < 0.01, "***",
    ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", ""))
  )
  paste0(
    format(round(est, 2), nsmall = 2), stars,
    "\n(", format(round(se, 2), nsmall = 2), ")"
  )
}
cell <- function(tidy_obj, term) {
  if (!(term %in% tidy_obj$term)) return("—")
  fmt(
    tidy_obj$estimate[tidy_obj$term == term],
    tidy_obj$std.error[tidy_obj$term == term],
    tidy_obj$p.value[tidy_obj$term == term]
  )
}

tab_df <- tibble(
  variable = c(
    "Constante", "Altura (pulgadas)",
    "lt_hs (<12)", "hs (=12)", "some_col (12<educ<16)"
  ),
  `(1) height` = c(
    cell(tidy1, "(Intercept)"),
    cell(tidy1, "height"), "—", "—", "—"
  ),
  `(2) height + educ` = c(
    cell(tidy2, "(Intercept)"),
    cell(tidy2, "height"),
    cell(tidy2, "lt_hs"),
    cell(tidy2, "hs"),
    cell(tidy2, "some_col")
  )
)

tabla <- gt(tab_df) |>
  tab_header(
    title = "Tabla 8: Hombres — OLS ingresos y dummies educación",
    subtitle = paste(
      "Var. dep.: Ingreso anual (US$ miles).",
      "EE robustos (HC1). Base: college (educ ≥ 16)."
    )
  ) |>
  tab_source_note(
    source_note = md(
      paste0(
        "Notas: * p<0.10, ** p<0.05, *** p<0.01. N(1)=", n1,
        ", R²(1)=", round(r21, 3), ". N(2)=", n2,
        ", R²(2)=", round(r22, 3), "."
      )
    )
  )

dir.create("output", showWarnings = FALSE)
gtsave(tabla, "output/tabla8_q12a.html")

b1 <- tidy1$estimate[tidy1$term == "height"]
se1 <- tidy1$std.error[tidy1$term == "height"]
b2 <- tidy2$estimate[tidy2$term == "height"]
se2 <- tidy2$std.error[tidy2$term == "height"]

res <- paste0(
  "Hombres. Coef. height (US$ miles/pulgada):\n",
  "  (1) sin educación: ", round(b1, 2),
  " (EE ", round(se1, 2), ")\n",
  "  (2) con educación: ", round(b2, 2),
  " (EE ", round(se2, 2), ")\n",
  "Cambio relativo pendiente (educ): ",
  round(100 * (b2 - b1) / b1, 1), "%.\n"
)
writeLines(res, "output/q12a_resumen.txt")

out <- bind_rows(
  tidy1 |>
    mutate(modelo = "(1) height"),
  tidy2 |>
    mutate(modelo = "(2) height + educ")
)
write_csv(out, "output/q12a_coefs.csv")