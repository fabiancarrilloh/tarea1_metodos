need <- c("tidyverse", "janitor", "readr", "gt", "broom")
to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)

library(tidyverse)
library(janitor)
library(readr)
library(gt)
library(broom)

data_path <- "data/earnings_height.csv"

if (!file.exists(data_path)) {
  stop("Falta 'data/earnings_height.csv'. Coloca el CSV en 'data/'.")
}

df <- read_csv(data_path, show_col_types = FALSE) |> clean_names()

cols_necesarias <- c("height", "sex", "earnings")
if (!all(cols_necesarias %in% names(df))) {
  stop(
    paste0(
      "Faltan columnas esperadas: ",
      paste(setdiff(cols_necesarias, names(df)), collapse = ", "),
      ". Revisa el CSV y el diccionario."
    )
  )
}

df <- df |>
  mutate(
    sex = case_when(
      is.numeric(sex) & all(na.omit(unique(sex)) %in% c(1, 2)) ~
        factor(sex, levels = c(1, 2), labels = c("Hombre", "Mujer")),
      is.numeric(sex) & all(na.omit(unique(sex)) %in% c(0, 1)) ~
        factor(sex, levels = c(0, 1), labels = c("Mujer", "Hombre")),
      is.character(sex) ~ case_when(
        str_to_lower(sex) %in% c("male", "m", "hombre") ~ "Hombre",
        str_to_lower(sex) %in% c("female", "f", "mujer") ~ "Mujer",
        TRUE ~ NA_character_
      ),
      TRUE ~ NA_character_
    )
  ) |>
  mutate(sex = factor(sex, levels = c("Hombre", "Mujer")))

n_total <- nrow(df)
altura_media_total <- mean(df$height, na.rm = TRUE)

por_sexo <- df |>
  filter(!is.na(sex), !is.na(height)) |>
  group_by(sex) |>
  summarise(
    N = n(),
    altura_promedio = mean(height, na.rm = TRUE),
    .groups = "drop"
  )

t_test_res <- NULL
if (all(c("Hombre", "Mujer") %in% por_sexo$sex)) {
  t_test_res <- t.test(
    height ~ sex,
    data = df |> filter(sex %in% c("Hombre", "Mujer"))
  )
  t_test_tidy <- tidy(t_test_res)
} else {
  warning(
    paste(
      "No se encontraron ambas categorías de sexo tras el mapeo.",
      "Revisa el diccionario o el CSV."
    )
  )
  t_test_tidy <- tibble(
    estimate = NA_real_, statistic = NA_real_, p.value = NA_real_,
    conf.low = NA_real_, conf.high = NA_real_
  )
}

dir.create("output", showWarnings = FALSE)

write_csv(por_sexo, "output/q1_altura_por_sexo.csv")

resumen_txt <- paste0(
  "Observaciones totales: ", n_total, "\n",
  "Altura promedio total: ", round(altura_media_total, 2),
  " pulgadas\n",
  if (!is.null(t_test_res)) {
    paste0(
      "Dif. medias (Hombre - Mujer): ",
      round(t_test_tidy$estimate, 2), " pulgadas\n",
      "p-valor: ", signif(t_test_tidy$p.value, 3), "\n"
    )
  } else {
    "Test de diferencia de medias no disponible: faltan categorías.\n"
  }
)
writeLines(resumen_txt, "output/q1_resumen.txt")

tabla1 <- por_sexo |>
  mutate(altura_promedio = round(altura_promedio, 2)) |>
  gt(rowname_col = "sex") |>
  tab_header(
    title = "Tabla 1: Estadística descriptiva por sexo",
    subtitle = "Muestra de trabajadores - Altura en pulgadas"
  ) |>
  cols_label(
    N = "N",
    altura_promedio = "Altura promedio (pulgadas)"
  ) |>
  fmt_number(columns = altura_promedio, decimals = 2) |>
  tab_source_note(
    source_note = md(
      paste0(
        "Notas: N total = **", n_total, "**. Altura prom. total = **",
        round(altura_media_total, 2),
        "** pulgadas.",
        if (!is.null(t_test_res)) {
          paste0(
            " Test t de diferencia por sexo. Dif. (H-M) = **",
            round(t_test_tidy$estimate, 2), "**, p-valor = **",
            signif(t_test_tidy$p.value, 3), "**."
          )
        } else {
          " No fue posible calcular el test de diferencia de medias."
        }
      )
    )
  )

gtsave(tabla1, "output/tabla1_q1.html")
