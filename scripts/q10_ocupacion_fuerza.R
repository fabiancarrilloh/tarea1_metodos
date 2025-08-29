need <- c(
  "tidyverse", "janitor", "readr", "broom", "gt",
  "sandwich", "lmtest", "ggplot2", "scales"
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
library(ggplot2)
library(scales)

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
    earnings_thou = earnings / 1000,  # US$ miles
    occupation = as.factor(occupation)
  ) |>
  filter(
    !is.na(earnings_thou), !is.na(height),
    !is.na(sex), !is.na(occupation)
  ) |>
  droplevels()

m1 <- lm(earnings_thou ~ height + sex, data = df)
m2 <- lm(earnings_thou ~ height + sex + occupation, data = df)

ct1 <- coeftest(m1, vcov = vcovHC(m1, type = "HC1"))
ct2 <- coeftest(m2, vcov = vcovHC(m2, type = "HC1"))

tidy1 <- tidy(ct1)
tidy2 <- tidy(ct2)
b1 <- tidy1$estimate[tidy1$term == "height"]
se1 <- tidy1$std.error[tidy1$term == "height"]
b2 <- tidy2$estimate[tidy2$term == "height"]
se2 <- tidy2$std.error[tidy2$term == "height"]

r21 <- summary(m1)$r.squared
r22 <- summary(m2)$r.squared
n <- nobs(m2)

drop_pct <- 100 * (b2 - b1) / b1

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
row1 <- fmt_row(b1, se1, tidy1$p.value[tidy1$term == "height"])
row2 <- fmt_row(b2, se2, tidy2$p.value[tidy2$term == "height"])

tab_df <- tibble(
  modelo = c(
    "Sin ocupación (controla sexo)",
    "Con efectos fijos de ocupación"
  ),
  `pendiente altura (US$ miles/pulgada)` = c(row1, row2),
  `r2` = c(round(r21, 3), round(r22, 3))
)

tabla <- gt(tab_df) |>
  tab_header(
    title = "Tabla 7: Altura e ingresos controlando por ocupación",
    subtitle = paste(
      "Var. dependiente: Ingreso anual (US$ miles).",
      "Errores estándar robustos (HC1)."
    )
  ) |>
  tab_source_note(
    source_note = md(
      paste0(
        "Notas: * p<0.10, ** p<0.05, *** p<0.01. N = ", n,
        ". Cambio relativo de la pendiente al agregar ocupación: ",
        round(drop_pct, 1), "%"
      )
    )
  )

dir.create("output", showWarnings = FALSE)
gtsave(tabla, "output/tabla7_q10.html")

occ_sum <- df |>
  group_by(occupation) |>
  summarise(
    n = n(),
    mean_height = mean(height, na.rm = TRUE),
    mean_earn_thou = mean(earnings_thou, na.rm = TRUE),
    .groups = "drop"
  )

p <- ggplot(
  occ_sum,
  aes(x = mean_height, y = mean_earn_thou, size = n)
) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(
    labels = label_dollar(prefix = "US$ ", suffix = "k")
  ) +
  labs(
    title = "Promedios por ocupación: altura vs ingreso",
    subtitle = "Cada punto es una ocupación; tamaño = n",
    x = "Altura promedio (pulgadas)",
    y = "Ingreso promedio (US$ miles)"
  ) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank())

ggsave(
  "output/fig_q10_occ_scatter.png",
  p,
  width = 7,
  height = 5,
  dpi = 300,
  bg = "white"
)

resumen <- paste0(
  "Coef. height sin ocupación: ", round(b1, 2),
  " (EE ", round(se1, 2), "), R² = ", round(r21, 3), "\n",
  "Coef. height con ocupación: ", round(b2, 2),
  " (EE ", round(se2, 2), "), R² = ", round(r22, 3), "\n",
  "Cambio relativo de la pendiente (ocupación): ",
  round(drop_pct, 1), "%.\n",
  "Interpretación: si la pendiente cae hacia 0 al controlar por ocupación, ",
  "parte de la asociación altura–ingresos opera vía selección ocupacional ",
  "que premia fuerza/estatura. Si casi no cambia, la ocupación no explica ",
  "ese vínculo."
)
writeLines(resumen, "output/q10_resumen.txt")
