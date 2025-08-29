# TAREA 1 · Métodos Cuantitativos I

## Descripción general

Este repositorio contiene la solución de la Tarea 1 del curso Métodos Cuantitativos I (Primavera 2025).
Trabaja con una submuestra del set usado por Case y Paxson (2008) para estudiar la relación entre **altura** e **ingresos**.

Incluye:

* **Datos** en `data/`
* **Scripts de R** en `scripts/` (uno por pregunta y un orquestador `run_all.R`)
* **Informe final** en `docs/Informe.pdf` (plantilla/versión compilada)

## Estructura del proyecto

```
.
├── data/
│   ├── earnings_height.csv               # Base de datos (limpia)
├── docs/
│   ├── Informe.pdf                       # Informe final
│   └── Earnings_and_Height_Descri...pdf  # Diccionario / documentación
├── scripts/
│   ├── q1_import_descriptivos.R
│   ├── q2_altos_bajos.R
│   ├── q4_scatter_ingresos_altura.R
│   ├── q5_reg_height_const.R
│   ├── q6_reg_sin_constante.R
│   ├── q7_altura_cm.R
│   ├── q9_interaccion_sexo.R
│   ├── q10_ocupacion_fuerza.R
│   ├── q12a_educ_dummies_hombres.R
│   ├── q12d_test_conjunto.R
│   ├── q12e_interpretacion.R
└── run_all.R                             # Pipeline para ejecutar todo

```

> Nota: al ejecutar los scripts se crea (o actualiza) un directorio `output/` con tablas `.html`, resúmenes `.txt`, figuras `.png` y CSVs auxiliares. Ese directorio no aparece si aún no se ha ejecutado nada.

## Requisitos

* **R ≥ 4.2** (Windows/macOS/Linux)
* Permisos de escritura en el directorio del proyecto

Los scripts instalan automáticamente los paquetes necesarios si no están presentes:
`tidyverse`, `janitor`, `readr`, `ggplot2`, `scales`, `viridis`,
`broom`, `gt`, `sandwich`, `lmtest`.

## Cómo ejecutar

### Opción 1: todo con un comando

Desde la raíz del repo:

```bash
Rscript scripts/run_all.R
# o en Windows:
# Rscript.exe scripts/run_all.R
```

El pipeline:

1. carga y limpia datos
2. ejecuta todas las preguntas (1, 2, 4, 5, 6, 7, 9, 10, 12a–e)
3. guarda salidas en `output/` con nombres autoexplicativos
4. deja listo el material para insertar en el informe (tablas y figuras)

### Opción 2: script por pregunta

Ejemplo:

```bash
Rscript scripts/q5_reg_height_const.R
```

Esto genera `output/tabla3_q5.html`, `output/q5_resumen.txt` y predicciones asociadas.

## Qué produce cada script

* `q1_import_descriptivos.R`: tamaño muestral, medias por sexo y test de diferencia de alturas.
  Salidas: `tabla1_q1.html`, `q1_resumen.txt`.
* `q2_altos_bajos.R`: crea dummy “alto” (>67 in) y compara ingresos promedio; test de medias.
  Salidas: `tabla2_q2.html`, `q2_resumen.txt`.
* `q4_scatter_ingresos_altura.R`: figura de dispersión ingresos vs. altura con recta OLS.
  Salidas: `fig_q4_scatter_clean.png`.
* `q5_reg_height_const.R`: OLS con constante; interpreta y calcula predichos 65/67/70 in.
  Salidas: `tabla3_q5.html`, `q5_predicciones.csv`, `q5_resumen.txt`.
* `q6_reg_sin_constante.R`: OLS sin constante; compara con constante y discute R² no centrado.
  Salidas: `tabla4_q6.html`, `q6_resumen.txt`.
* `q7_altura_cm.R`: convierte a cm y reestima; muestra invariancia de intercepto y R².
  Salidas: `tabla5_q7.html`, `q7_resumen.txt`.
* `q9_interaccion_sexo.R`: interacción `height × mujer`; pendientes por sexo y test Wald.
  Salidas: `tabla6_q9.html`, `q9_pendientes_por_sexo.csv`, `q9_resumen.txt`.
* `q10_ocupacion_fuerza.R`: efectos fijos de ocupación; comparación de pendientes y gráfico por ocupación.
  Salidas: `tabla7_q10.html`, `fig_q10_occ_scatter.png`, `q10_resumen.txt`.
* `q12a_educ_dummies_hombres.R`: dummies de educación (hombres); modelos (1) y (2).
  Salidas: `tabla8_q12a.html`, `q12a_resumen.txt`, `q12a_coefs.csv`.
* `q12d_test_conjunto.R`: test conjunto de dummies de educación (Wald robusto).
  Salidas: `tabla9_q12d.html`, `q12d_resumen.txt`.
* `q12e_interpretacion.R`: interpretación de dummies y niveles predichos por educación.
  Salidas: `tabla10_q12e.html`, `q12e_resumen.txt`, `q12e_pred_por_educ.csv`.


Criterios clave:

* Coeficientes de altura suelen ser **positivos** y del orden de **0.7–1.3 US\$ mil** por pulgada (dependiendo de controles).
* Al controlar **ocupación** y **educación**, la pendiente **se atenúa** pero permanece significativa.
* Tests de interacción por **sexo** y tests conjuntos de **educación** muestran heterogeneidad y relevancia de controles.
* Cambiar la unidad de altura a **cm** reescala la pendiente por **1/2.54**; **intercepto** y **R²** no cambian.

## Parametrización y supuestos

* El archivo de datos esperado es `data/earnings_height.csv`.
* Si cambias de ubicación o nombre de archivo, ajusta la ruta al inicio de cada script.
* Todos los modelos usan **errores estándar robustos (HC1)**.

## Reproducibilidad

* Los scripts instalan dependencias si faltan.
* Ejecución determinista; no se usan semillas salvo en gráficos con jitter (afecta solo la estética, no los resultados).

## Referencia bibliográfica

Case, A., & Paxson, C. (2008). “Stature and Status: Height, Ability, and Labor Market Outcomes.” *Journal of Political Economy*, 116(3), 499–532.

## Autores

* **Estudiantes**: *Fabián Carrillo* y *Yasna Lobos*
* **Curso**: Métodos Cuantitativos I — Primavera 2025
* **Profesora**: Valentina Paredes

## Licencia

Uso académico. Si reutilizas el código o tablas, cita este repositorio y la fuente de datos.
