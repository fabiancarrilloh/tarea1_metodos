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

df0 <- read_csv(
  "data/earnings_height.csv",
  show_col_types = FALSE
) |>
  clean_names()

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

df <- df0 |>
  mutate(
    sex = sex_map(sex),
    earnings_thou = earnings / 1000
  ) |>
  filter(
    !is.na(earnings_thou), !is.na(height), !is.na(sex)
  ) |>
  droplevels()

stopifnot(all(levels(df$sex) %in% c("Hombre", "Mujer")))

m_r <- lm(earnings_thou ~ height + sex, data = df)
m_u <- lm(earnings_thou ~ height * sex, data = df)

ct_u <- coeftest(m_u, vcov = vcovHC(m_u, type = "HC1"))
tidy_u <- tidy(ct_u)

wald_cmp <- waldtest(m_r, m_u, vcov = vcovHC)
vcov_u <- vcovHC(m_u, type = "HC1")
b_h <- coef(m_u)[["height"]]
b_int <- coef(m_u)[["height:sexMujer"]]

se_h <- sqrt(vcov_u["height", "height"])
se_f <- sqrt(
  vcov_u["height", "height"] +
    vcov_u["height:sexMujer", "height:sexMujer"] +
    2 * vcov_u["height", "height:sexMujer"]
)

df_res <- df.residual(m_u)
t_h <- b_h / se_h
p_h <- 2 * pt(abs(t_h), df = df_res, lower.tail = FALSE)
t_f <- (b_h + b_int) / se_f
p_f <- 2 * pt(abs(t_f), df = df_res, lower.tail = FALSE)

slopes <- tibble(
  grupo = c("Hombres", "Mujeres"),
  `pendiente (US$ miles por pulgada)` = c(b_h, b_h + b_int),
  `ee_robusto (HC1)` = c(se_h, se_f),
  t = c(t_h, t_f),
  `p_valor` = c(p_h, p_f)
)

fmt_row <- function(est, se, p) {
  stars <- ifelse(
    p < 0.01, "***",
    ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", ""))
  )
  paste0(
    format(round(est, 2), nsmall = 2), stars,
    "\n(", format(round(se, 2), nsmall = 2), ")"
  )
}

tab_df <- tibble(
  variable = c(
    "Constante", "Mujer", "Altura (pulgadas)", "Altura × Mujer"
  ),
  `modelo con interacción` = c(
    fmt_row(
      tidy_u$estimate[tidy_u$term == "(Intercept)"],
      tidy_u$std.error[tidy_u$term == "(Intercept)"],
      tidy_u$p.value[tidy_u$term == "(Intercept)"]
    ),
    fmt_row(
      tidy_u$estimate[tidy_u$term == "sexMujer"],
      tidy_u$std.error[tidy_u$term == "sexMujer"],
      tidy_u$p.value[tidy_u$term == "sexMujer"]
    ),
    fmt_row(
      tidy_u$estimate[tidy_u$term == "height"],
      tidy_u$std.error[tidy_u$term == "height"],
      tidy_u$p.value[tidy_u$term == "height"]
    ),
    fmt_row(
      tidy_u$estimate[tidy_u$term == "height:sexMujer"],
      tidy_u$std.error[tidy_u$term == "height:sexMujer"],
      tidy_u$p.value[tidy_u$term == "height:sexMujer"]
    )
  )
)

r2 <- summary(m_u)$r.squared
n <- nobs(m_u)

tabla <- gt(tab_df) |>
  tab_header(
    title = "Tabla 6: Efecto de la altura en ingresos por sexo (interacción)",
    subtitle = paste(
      "Var. dependiente: Ingreso anual (US$ miles).",
      "Errores estándar robustos (HC1)."
    )
  ) |>
  tab_source_note(
    source_note = md(
      paste0(
        "Notas: * p<0.10, ** p<0.05, *** p<0.01. N = ", n,
        ". R² = ", round(r2, 3),
        ". Test de pendientes iguales (Wald robusto, m_r vs m_u): F = ",
        round(wald_cmp$F[2], 2), ", p-valor = ",
        signif(wald_cmp$`Pr(>F)`[2], 3), "."
      )
    )
  )

dir.create("output", showWarnings = FALSE)
gtsave(tabla, "output/tabla6_q9.html")

write_csv(
  slopes |>
    mutate(across(where(is.numeric), ~ round(.x, 3))),
  "output/q9_pendientes_por_sexo.csv"
)

resumen <- paste0(
  "Pendientes por sexo (US$ miles por pulgada):\n",
  "  Hombres: ", round(b_h, 3),
  " (EE ", round(se_h, 3), "), p = ", signif(p_h, 3), "\n",
  "  Mujeres: ", round(b_h + b_int, 3),
  " (EE ", round(se_f, 3), "), p = ", signif(p_f, 3), "\n",
  "Test H0 (pendientes iguales): coeficiente interacción = 0.\n",
  "  Wald robusto (m_r vs m_u): F = ", round(wald_cmp$F[2], 2),
  ", p-valor = ", signif(wald_cmp$`Pr(>F)`[2], 3), ".\n"
)
writeLines(resumen, "output/q9_resumen.txt")
