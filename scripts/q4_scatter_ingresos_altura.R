need <- c(
  "tidyverse", "janitor", "readr",
  "ggplot2", "scales", "viridis"
)
to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)
library(tidyverse)
library(janitor)
library(readr)
library(ggplot2)
library(scales)
library(viridis)

df <- read_csv(
  "data/earnings_height.csv",
  show_col_types = FALSE
) |>
  clean_names()

df <- df |>
  mutate(earnings_thou = earnings / 1000)

ylim <- quantile(df$earnings_thou, c(0.01, 0.99), na.rm = TRUE)
p_clean <- ggplot(df, aes(x = height, y = earnings_thou)) +
  geom_point(
    alpha = 0.25, size = 0.7,
    position = position_jitter(
      width = 0,
      height = diff(ylim) * 0.01
    )
  ) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  coord_cartesian(ylim = ylim) +
  scale_y_continuous(
    labels = label_dollar(
      prefix = "US$ ",
      suffix = "k",
      accuracy = 1
    )
  ) +
  labs(
    title = "Ingresos anuales vs altura",
    subtitle = paste(
      "Ingreso anual en miles de US$;",
      "jitter vertical rompe discretización"
    ),
    x = "Altura (pulgadas)",
    y = "Ingreso anual (US$ miles)"
  ) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank())

dir.create("output", showWarnings = FALSE)
ggsave(
  "output/fig_q4_scatter_clean.png", p_clean,
  width = 7, height = 5, dpi = 300, bg = "white"
)

p_density <- ggplot(df, aes(height, earnings_thou)) +
  geom_bin2d(bins = 40) +
  coord_cartesian(ylim = ylim) +
  scale_fill_viridis_c(
    name = "N",
    option = "C",
    trans = "sqrt"
  ) +
  labs(
    title = "Densidad: ingresos vs altura",
    subtitle = paste(
      "Color = observaciones por celda;",
      "ingreso en miles de US$"
    ),
    x = "Altura (pulgadas)",
    y = "Ingreso anual (US$ miles)"
  ) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank())

ggsave(
  "output/fig_q4_bin2d.png", p_density,
  width = 7, height = 5, dpi = 300, bg = "white"
)
