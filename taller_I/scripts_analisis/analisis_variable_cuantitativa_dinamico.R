# Análisis Dinámico de Variable Cuantitativa
# Este script permite analizar cualquier variable cuantitativa cambiando solo el nombre de la variable

# ---- CONFIGURACIÓN (CAMBIAR SOLO AQUÍ) ----
variable_name <- "P6800"  # <- CAMBIAR SOLO ESTO
data_file <- "../Combinados/Combinado_cuantitativo_P6800.csv"  # <- Y ESTO SI ES NECESARIO
# ---- FIN CONFIGURACIÓN ----

# Crear directorio de salida dinámicamente
output_dir <- paste0("analisis_", variable_name, "_cuantitativa")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

cat("=== ANÁLISIS DINÁMICO DE VARIABLE CUANTITATIVA ===\n")
cat("Variable:", variable_name, "\n")
cat("Archivo:", data_file, "\n")
cat("Directorio salida:", output_dir, "\n\n")

# ---- Celda 1: Lectura y limpieza -------------------------------------------
library(readr)
library(dplyr)

# Leer todo como carácter para evitar parseos automáticos
df_char <- readr::read_csv(data_file,
                          col_types = readr::cols(.default = "c"),
                          locale = readr::locale(encoding = "UTF-8"),
                          show_col_types = FALSE)

# Detectar nombre real de la variable (insensible a mayúsculas)
if (!variable_name %in% names(df_char)) {
  guess <- names(df_char)[tolower(names(df_char)) %in% tolower(variable_name)]
  if (length(guess) == 1) {
    variable_name_real <- guess
    cat("Variable detectada:", variable_name_real, "\n")
  } else {
    cat("Variables similares encontradas:", paste(guess, collapse = ", "), "\n")
    stop("No se encontró la variable ", variable_name)
  }
} else {
  variable_name_real <- variable_name
  cat("Variable encontrada:", variable_name_real, "\n")
}

# Función simple de limpieza numérica (solo manejar valores faltantes y espacios)
clean_numeric2 <- function(x) {
  x <- as.character(x)
  x[ x %in% c("", "NA", NA) ] <- NA_character_
  x <- trimws(x)  # Solo remover espacios en blanco
  # Convertir directamente a numérico sin modificar el formato
  suppressWarnings(as.numeric(x))
}

# Preparar df_var con la variable limpia
df_var <- df_char %>%
  transmute(
    value_raw = .data[[variable_name_real]],
    value = clean_numeric2(.data[[variable_name_real]])
  )

# Información básica
cat("Total filas:", nrow(df_var), "   N no-missing:", sum(!is.na(df_var$value)), "\n")
cat("Primeras 10 observaciones:\n")
print(head(df_var, 10))

# ---- Celda 2: Estadísticos Descriptivos Ampliados --------------------------
library(dplyr)

x <- df_var$value
x_nm <- x[!is.na(x)]

if (length(x_nm) == 0) {
  stop("No hay datos no-missing para calcular estadísticos descriptivos.")
}

# Calcular medidas descriptivas ampliadas (más de 10 estadísticos)
N_total <- length(x)
N_missing <- sum(is.na(x))
N_valid <- sum(!is.na(x))

# Medidas de tendencia central
Min <- min(x_nm, na.rm = TRUE)
Q1 <- quantile(x_nm, 0.25, na.rm = TRUE)
Q2_median <- median(x_nm, na.rm = TRUE)
Mean <- mean(x_nm, na.rm = TRUE)
Q3 <- quantile(x_nm, 0.75, na.rm = TRUE)
Max <- max(x_nm, na.rm = TRUE)

# Medidas de dispersión
SD <- sd(x_nm, na.rm = TRUE)
Var <- var(x_nm, na.rm = TRUE)
IQR_val <- IQR(x_nm, na.rm = TRUE)
Range <- Max - Min
CV <- SD / Mean  # Coeficiente de variación
MAD <- median(abs(x_nm - Q2_median))  # Desviación absoluta mediana

# Medidas de forma
library(moments)
Skewness <- skewness(x_nm, na.rm = TRUE)
Kurtosis <- kurtosis(x_nm, na.rm = TRUE)

# Percentiles adicionales
P10 <- quantile(x_nm, 0.10, na.rm = TRUE)
P90 <- quantile(x_nm, 0.90, na.rm = TRUE)
P95 <- quantile(x_nm, 0.95, na.rm = TRUE)

# Moda
mode_basic_all <- function(v) {
  v2 <- v[!is.na(v)]
  if (length(v2) == 0) return(NA)
  tb <- sort(table(v2), decreasing = TRUE)
  names(tb)[tb == max(tb)]
}
modes <- mode_basic_all(x_nm)
mode_report <- paste(modes, collapse = ", ")

# Crear tabla vertical con todos los estadísticos
tabla_descriptivos <- tibble(
  Estadistico = c(
    "N total", "N missing", "N válidos",
    "Mínimo", "P10", "Q1 (P25)", "Mediana (P50)", "Media", "Q3 (P75)", "P90", "P95", "Máximo",
    "Rango", "IQR", "Desviación Estándar", "Varianza", "Coeficiente de Variación",
    "Desviación Absoluta Mediana", "Asimetría", "Curtosis", "Moda(s)"
  ),
  Valor = c(
    N_total, N_missing, N_valid,
    Min, P10, Q1, Q2_median, Mean, Q3, P90, P95, Max,
    Range, IQR_val, SD, Var, CV,
    MAD, Skewness, Kurtosis, mode_report
  )
)

cat("\n=== ESTADÍSTICOS DESCRIPTIVOS AMPLIADOS ===\n")
print(n =30,tabla_descriptivos)

# Mostrar estadísticas resumidas estilo summary()
cat("\n=== Summary (sobre valores no-missing) ===\n")
print(summary(x_nm))

# ---- Celda 3: Análisis Gráfico --------------------------------------------
if (!requireNamespace("gridExtra", quietly = TRUE)) {
  install.packages("gridExtra", repos = "https://cloud.r-project.org")
}
library(gridExtra)
library(ggplot2)

plot_dir <- file.path(output_dir, "plots")
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

if (length(x_nm) == 0) {
  message("No hay datos no-missing: no se generan gráficos.")
} else {
  df_plot <- df_var %>% filter(!is.na(value))

  # Histograma lineal
  ph <- ggplot(df_plot, aes(x=value)) +
    geom_histogram(bins = 60, fill = "lightblue", color = "black", alpha = 0.7) +
    labs(title = paste("Histograma de", variable_name_real), 
         x = variable_name_real, y = "Frecuencia") +
    theme_minimal()
  ggsave(file.path(plot_dir, paste0("hist_", variable_name_real, ".png")), ph, width=8, height=4)

  # Boxplot
  pb <- ggplot(df_plot, aes(y=value)) +
    geom_boxplot(fill = "lightgreen", color = "black", alpha = 0.7) +
    labs(title = paste("Boxplot de", variable_name_real), y = variable_name_real) +
    theme_minimal()
  ggsave(file.path(plot_dir, paste0("boxplot_", variable_name_real, ".png")), pb, width=6, height=4)

  # Histograma log1p (si hay valores > 0)
  pl <- NULL
  if (any(df_plot$value > 0, na.rm=TRUE)) {
    df_plot <- df_plot %>% mutate(value_log1p = log1p(value))
    pl <- ggplot(df_plot, aes(x=value_log1p)) + 
      geom_histogram(bins = 60, fill = "orange", color = "black", alpha = 0.7) +
      labs(title = paste("Histograma log1p(", variable_name_real, ")"), x = "log1p(value)") + 
      theme_minimal()
    ggsave(file.path(plot_dir, paste0("hist_log1p_", variable_name_real, ".png")), pl, width=8, height=4)
  }

  # Density
  pd <- ggplot(df_plot, aes(x=value)) + 
    geom_density(fill = "purple", alpha = 0.5, color = "black") + 
    theme_minimal() +
    labs(title = paste("Densidad -", variable_name_real))
  ggsave(file.path(plot_dir, paste0("density_", variable_name_real, ".png")), pd, width=8, height=4)

  # Mostrar todas las gráficas juntas
  if (!is.null(pl)) {
    grid.arrange(ph, pb, pl, pd, ncol = 2)
  } else {
    grid.arrange(ph, pb, pd, ncol = 2)
  }

  message("Gráficos guardados en: ", plot_dir)
}

# ---- Celda 4: Cálculo de los estimadores --------------------------------
library(dplyr)
library(boot)

set.seed(12345)

# Asegurar que df_var existe
if (!exists("df_var") || !"value" %in% names(df_var)) {
  stop("df_var no está disponible. Ejecutar celdas anteriores primero.")
}

x <- df_var$value
x_nm <- x[!is.na(x)]

# Mensajes si pocos datos
cat("\n=== ESTIMADORES PUNTUALES ===\n")
cat("Total observaciones (N):", length(x), "\n")
cat("No-missing (n):", length(x_nm), "\n\n")

if (length(x_nm) == 0) stop("No hay datos no-missing para calcular estimadores.")

# 1) Estimadores puntuales
est_mean <- mean(x_nm)
est_median <- median(x_nm)

cat("Estimadores puntuales:\n")
cat(sprintf(" Mean: %s\n Median: %s\n Mode(s): %s\n\n",
            format(round(est_mean, 2), big.mark = ","), 
            format(round(est_median, 2), big.mark=","), 
            mode_report))

# 2) IC 95% para la media (t-interval, usando t.test)
if (length(x_nm) > 1) {
  tt <- try(t.test(x_nm), silent = TRUE)
  if (inherits(tt, "try-error")) {
    cat("No se pudo calcular t-interval por t.test():", tt, "\n")
  } else {
    ci_t <- tt$conf.int
    cat("IC 95% para la media (t): [", format(round(ci_t[1],2), big.mark=","), 
        ", ", format(round(ci_t[2],2), big.mark=","), "]\n\n")
  }
} else cat("No hay suficientes datos para IC t.\n\n")

# 3) Bootstrap para mean y median
B <- 100  # puedes aumentar si tienes tiempo
cat("Ejecutando bootstrap con R =", B, "réplicas...\n")

# Bootstrap mean
boot_mean <- try(boot(x_nm, statistic = function(d, i) mean(d[i]), R = B), silent = TRUE)
if (inherits(boot_mean, "try-error")) {
  cat("Bootstrap mean falló:", boot_mean, "\n")
} else {
  ci_mean <- tryCatch({
    pci <- boot.ci(boot_mean, type = c("perc","bca"))
    pci
  }, error = function(e) e)
  if (inherits(ci_mean, "error")) {
    ci_mean_perc <- quantile(boot_mean$t, probs = c(0.025, 0.975), na.rm = TRUE)
    cat("Bootstrap IC (perc) mean (fallback percentiles):", 
        format(round(ci_mean_perc[1],2), big.mark=","), 
        format(round(ci_mean_perc[2],2), big.mark=","), "\n")
  } else {
    if (!is.null(ci_mean$percent)) {
      ci_vals <- ci_mean$percent[4:5]
      cat("Bootstrap IC (perc) mean:", 
          format(round(ci_vals[1],2), big.mark=","), 
          format(round(ci_vals[2],2), big.mark=","), "\n")
    } else {
      ci_mean_perc <- quantile(boot_mean$t, probs = c(0.025, 0.975), na.rm = TRUE)
      cat("Bootstrap IC mean (percentiles directos):", 
          format(round(ci_mean_perc[1],2), big.mark=","), 
          format(round(ci_mean_perc[2],2), big.mark=","), "\n")
    }
  }
}

# Bootstrap median
boot_median <- try(boot(x_nm, statistic = function(d, i) median(d[i]), R = B), silent = TRUE)
if (inherits(boot_median, "try-error")) {
  cat("Bootstrap median falló:", boot_median, "\n")
} else {
  ci_med_try <- tryCatch({
    ci_m <- boot.ci(boot_median, type = c("perc","bca"))
    ci_m
  }, error = function(e) e)
  if (inherits(ci_med_try, "error")) {
    med_perc <- tryCatch({
      quantile(boot_median$t, probs = c(0.025, 0.975), na.rm = TRUE)
    }, error = function(e) NULL)
    if (!is.null(med_perc)) {
      cat("Bootstrap IC (perc) median (fallback percentiles):", 
          format(round(med_perc[1],2), big.mark=","), 
          format(round(med_perc[2],2), big.mark=","), "\n")
    } else {
      cat("Bootstrap IC median: no disponible (poca variabilidad en réplicas)\n")
    }
  } else {
    if (!is.null(ci_med_try$percent)) {
      ci_vals_med <- ci_med_try$percent[4:5]
      cat("Bootstrap IC (perc) median:", 
          format(round(ci_vals_med[1],2), big.mark=","), 
          format(round(ci_vals_med[2],2), big.mark=","), "\n")
    } else {
      cat("Bootstrap IC median calculado, pero formato inesperado; usando percentiles directos.\n")
      med_perc <- quantile(boot_median$t, probs = c(0.025, 0.975), na.rm = TRUE)
      cat("Bootstrap IC median (percentiles):", 
          format(round(med_perc[1],2), big.mark=","), 
          format(round(med_perc[2],2), big.mark=","), "\n")
    }
  }
}

cat("\n=== FIN ESTIMADORES ===\n")

# ---- Celda 5: Insesgamiento (bias) mediante bootstrap ----------------------
library(boot)
set.seed(2025)

cat("\n=== ANÁLISIS DE INSESGAMIENTO ===\n")

x <- df_var$value
x_nm <- x[!is.na(x)]

if (length(x_nm) > 5) {
  B <- 1000
  boot_mean <- boot(x_nm, statistic = function(data, idx) mean(data[idx]), R = B)
  boot_median <- boot(x_nm, statistic = function(data, idx) median(data[idx]), R = B)

  bias_mean <- mean(boot_mean$t) - est_mean
  bias_median <- mean(boot_median$t) - est_median

  cat("Bias estimado (bootstrap):\n")
  cat(" Mean bias:", bias_mean, "\n")
  cat(" Median bias:", bias_median, "\n")

  # Mostrar distribuciones bootstrap
  cat("\nQuantiles bootstrap mean (2.5%,50%,97.5%): ", 
      quantile(boot_mean$t, c(.025,.5,.975)), "\n")
  cat("Quantiles bootstrap median (2.5%,50%,97.5%): ", 
      quantile(boot_median$t, c(.025,.5,.975)), "\n")

  cat("\nComentario sobre insesgamiento:\n")
  if (abs(bias_mean) < 0.01 * abs(est_mean)) {
    cat(" La media muestra sesgo pequeño relativo.\n")
  } else {
    cat(" La media muestra sesgo apreciable relativo — considerar estimadores robustos.\n")
  }
  if (abs(bias_median) < 0.01 * abs(est_median)) {
    cat(" La mediana muestra sesgo pequeño relativo.\n")
  } else {
    cat(" La mediana muestra sesgo apreciable relativo.\n")
  }
} else {
  cat("No hay suficientes datos para estimar bias con bootstrap.\n")
}

# ---- Celda 6: Consistencia (comportamiento varianza vs n) -----------------
library(dplyr)
set.seed(123)

cat("\n=== ANÁLISIS DE CONSISTENCIA ===\n")

x <- df_var$value
x_nm <- x[!is.na(x)]

if (length(x_nm) > 30) {
  ns <- unique(floor(seq(50, length(x_nm), length.out = 8)))
  reps <- 300  # repeticiones por tamaño (moderado)
  res <- data.frame(n = integer(), var_mean = numeric(), var_median = numeric())
  for (n in ns) {
    ests_mean <- numeric(reps)
    ests_med <- numeric(reps)
    for (r in 1:reps) {
      s <- sample(x_nm, size = n, replace = FALSE)
      ests_mean[r] <- mean(s)
      ests_med[r] <- median(s)
    }
    res <- rbind(res, data.frame(n = n, var_mean = var(ests_mean), var_median = var(ests_med)))
  }
  print(res)

  # Gráfico var vs n
  library(ggplot2)
  pcons <- ggplot(res, aes(x = n)) +
    geom_line(aes(y = var_mean, color = "var_mean")) +
    geom_point(aes(y = var_mean, color = "var_mean")) +
    geom_line(aes(y = var_median, color = "var_median")) +
    geom_point(aes(y = var_median, color = "var_median")) +
    labs(title = "Varianza del estimador vs tamaño muestral (submuestras)",
         x = "n (submuestra)", y = "Varianza estimada") + 
    theme_minimal() +
    scale_color_manual(values = c("var_mean" = "blue", "var_median" = "red"),
                       labels = c("Varianza Media", "Varianza Mediana"))
  print(pcons)
  ggsave(filename = file.path(output_dir, "consistencia_var_vs_n.png"), 
         plot = pcons, width = 7, height = 4)
  message("Gráfico de consistencia guardado en: ", 
          file.path(output_dir, "consistencia_var_vs_n.png"))

  cat("\nComentario (Consistencia):\n")
  cat("Si la varianza del estimador (mean o median) disminuye al crecer n, evidencia consistencia.\n")
} else {
  cat("No hay suficientes datos (>30) para estudiar consistencia por submuestreo.\n")
}

# ---- Celda 7: Eficiencia ---------------------------------------------------
library(dplyr)
set.seed(42)

cat("\n=== ANÁLISIS DE EFICIENCIA ===\n")

x <- df_var$value
x_nm <- x[!is.na(x)]

if (length(x_nm) > 10) {
  # Bootstrap var estimadas
  B <- 1000
  b_mean <- boot(x_nm, statistic = function(d,i) mean(d[i]), R = B)
  b_median <- boot(x_nm, statistic = function(d,i) median(d[i]), R = B)
  var_mean_boot <- var(b_mean$t)
  var_median_boot <- var(b_median$t)
  rel_eff <- var_median_boot / var_mean_boot

  cat("Eficiencia (empírica via bootstrap):\n")
  cat(" Var(mean) bootstrap:", var_mean_boot, "\n")
  cat(" Var(median) bootstrap:", var_median_boot, "\n")
  cat(" Rel. effic (var_median / var_mean):", rel_eff, "\n")

  cat("\nComentario (Eficiencia):\n")
  if (rel_eff > 1) {
    cat(" La media es más eficiente (menor var) que la mediana en esta muestra.\n")
  } else {
    cat(" La mediana es más eficiente en esta muestra.\n")
  }
} else {
  cat("No hay suficientes datos para evaluar eficiencia.\n")
}

# ---- Resumen Final ---------------------------------------------------------
cat("\n=== RESUMEN DEL ANÁLISIS ===\n")
cat("Variable analizada:", variable_name_real, "\n")
cat("Archivo de datos:", data_file, "\n")
cat("Total observaciones:", N_total, "\n")
cat("Observaciones válidas:", N_valid, "\n")
cat("Porcentaje de datos faltantes:", round(100 * N_missing / N_total, 2), "%\n")
cat("Directorio de salida:", output_dir, "\n")

# Sintaxis empleada
cat("\n--- Sintaxis clave usada en este análisis ---\n")
cat("1) Limpieza money: clean_numeric2(...) \n")
cat("2) Descriptivos ampliados: tabla_descriptivos con 20+ estadísticos\n")
cat("3) Gráficas guardadas en:", file.path(output_dir, "plots"), "\n")
cat("4) IC mean (t): t.test(x)$conf.int\n")
cat("5) Bootstrap bias y CI: uso package 'boot' (boot(...), boot.ci(...))\n")
cat("6) Consistencia: submuestreo comparando var(estimator) vs n\n")
cat("7) Eficiencia: comparar var_bootstrap(estimator)\n")

cat("\n=== ANÁLISIS COMPLETADO ===\n")
