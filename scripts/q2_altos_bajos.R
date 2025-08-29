need <- c("tidyverse", "janitor", "readr", "gt", "broom")
to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install) > 0) {
  install.packages(to_install, dependencies = TRUE)
}

library(tidyverse)
library(janitor)
library(readr)
library(gt)
library(broom)

data_path <- "data/earnings_height.csv"
if (!file.exists(data_path)) {
  stop("No se encuentra 'data/earnings_height.csv'.")
}
df <- read_csv(data_path, show_col_types = FALSE) |> clean_names()
stopifnot(all(c("height", "earnings") %in% names(df)))
df2 <- df |>
  mutate(
    tall_fac = if_else(height > 67, "Altos (>67)", "Bajos (≤67)"),
    tall_fac = factor(tall_fac, levels = c("Bajos (≤67)", "Altos (>67)")),
    earnings_thou = earnings / 1000
  )

grp <- df2 |>
  filter(!is.na(tall_fac), !is.na(earnings_thou)) |>
  group_by(tall_fac) |>
  summarise(
    N = n(),
    ingreso_prom_mil = mean(earnings_thou),
    sd_mil = sd(earnings_thou),
    .groups = "drop"
  )

mean_bajos <- grp$ingreso_prom_mil[grp$tall_fac == "Bajos (≤67)"]
mean_altos <- grp$ingreso_prom_mil[grp$tall_fac == "Altos (>67)"]
diff_ab <- mean_altos - mean_bajos

tt <- t.test(earnings_thou ~ tall_fac, data = df2)
ttidy <- tidy(tt)
ci_ab <- c(-ttidy$conf.high, -ttidy$conf.low)
t_ab <- -ttidy$statistic
pval <- ttidy$p.value
dir.create("output", showWarnings = FALSE)

write_csv(
  grp |>
    transmute(
      Grupo = tall_fac,
      N,
      `Ingreso promedio (USD miles)` = round(ingreso_prom_mil, 0),
      `Desv. estándar (USD miles)`   = round(sd_mil, 0)
    ),
  "output/q2_ingresos_altos_bajos.csv"
)

resumen_txt <- paste0(
  "Ingresos promedio (miles USD):\n",
  "  Altos (>67): ", round(mean_altos, 0), "\n",
  "  Bajos (≤67): ", round(mean_bajos, 0), "\n",
  "Diferencia (Altos - Bajos): ", round(diff_ab, 1), " mil USD\n",
  "Intervalo 95%: [", round(ci_ab[1], 1), ", ",
  round(ci_ab[2], 1), "] mil USD\n",
  "t = ", round(t_ab, 2), ", p-valor = ", signif(pval, 3), "\n"
)
writeLines(resumen_txt, "output/q2_resumen.txt")

tabla2 <- grp |>
  transmute(
    Grupo = tall_fac,
    N,
    `Ingreso promedio (USD miles)` = round(ingreso_prom_mil, 0),
    `Desv. estándar (USD miles)`   = round(sd_mil, 0)
  ) |>
  gt() |>
  tab_header(
    title = "Tabla 2: Ingresos promedio por grupo de altura",
    subtitle = paste(
      "Definición: Bajos ≤ 67 pulgadas; Altos > 67 pulgadas.",
      "Ingreso anual en miles de USD."
    )
  ) |>
  tab_source_note(
    source_note = md(
      paste0(
        "Notas: Test de Welch. Diferencia (Altos − Bajos) = **",
        round(diff_ab, 1), "** mil USD; IC95% = **[",
        round(ci_ab[1], 1), ", ", round(ci_ab[2], 1), "]**; t = **",
        round(t_ab, 2), "**; p-valor = **", signif(pval, 3), "**."
      )
    )
  )

gtsave(tabla2, "output/tabla2_q2.html")
