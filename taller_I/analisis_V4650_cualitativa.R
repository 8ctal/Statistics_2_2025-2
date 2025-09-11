# Análisis de Variable Cualitativa V4650 (P5090)

# Variable: V4650 (P5090) - La vivienda ocupada por este hogar es:
# Codificación: 
# 1 = Propia, totalmente pagada
# 2 = Propia, la están pagando  
# 3 = En arriendo o subarriendo
# 4 = En usufructo
# 5 = Posesión sin título (Ocupante de hecho) ó propiedad colectiva
# 6 = Otra

# Tipo: Variable cualitativa nominal

# ---- Celda 1: Lectura y limpieza para variable cualitativa ------------
library(readr)
library(dplyr)

data_path <- "Combinado.csv"
varname   <- "P5090"   # variable cualitativa
out_dir   <- "analisis_P5090_cualitativa"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Leer todo como carácter para evitar parseos automáticos
df_char <- readr::read_csv(data_path,
                           col_types = readr::cols(.default = "c"),
                           locale = readr::locale(encoding = "UTF-8"),
                           show_col_types = FALSE)

# detectar nombre real (insensible a mayúsculas)
if (!varname %in% names(df_char)) {
  guess <- names(df_char)[tolower(names(df_char)) %in% tolower(varname)]
  if (length(guess) == 1) varname_real <- guess else stop("No encontré la variable ", varname)
} else varname_real <- varname

# función de limpieza para variable cualitativa
clean_categorical <- function(x) {
  x <- as.character(x)
  x[x %in% c("", "NA", NA)] <- NA_character_
  x <- trimws(x)
  # Convertir a numérico para facilitar análisis
  suppressWarnings(as.numeric(x))
}

# preparar df_var con la variable limpia
df_var <- df_char %>%
  transmute(
    value_raw = .data[[varname_real]],
    value = clean_categorical(.data[[varname_real]])
  )

# info rápida
cat("Variable real detectada:", varname_real, "\n")
cat("Total filas:", nrow(df_var), "   N no-missing:", sum(!is.na(df_var$value)), "\n")
cat("Valores únicos:", paste(sort(unique(df_var$value[!is.na(df_var$value)])), collapse = ", "), "\n")

# ver primeras filas
print(head(df_var, 10))

# ---- Celda 2: Análisis Descriptivo para Variable Cualitativa -----------
library(dplyr)

x <- df_var$value
x_nm <- x[!is.na(x)]

# Calcular medidas descriptivas
N_total <- length(x)
N_missing <- sum(is.na(x))
N_valid <- sum(!is.na(x))

# Tabla de frecuencias
freq_table <- table(x_nm)
prop_table <- prop.table(freq_table)

# Crear etiquetas para las categorías
categorias <- c("1" = "Propia, totalmente pagada",
                "2" = "Propia, la están pagando", 
                "3" = "En arriendo o subarriendo",
                "4" = "En usufructo",
                "5" = "Posesión sin título",
                "6" = "Otra")

# Crear tabla descriptiva
tabla_descriptiva <- data.frame(
  Codigo = names(freq_table),
  Categoria = categorias[names(freq_table)],
  Frecuencia = as.numeric(freq_table),
  Proporcion = as.numeric(prop_table),
  Porcentaje = as.numeric(prop_table) * 100
)

cat("=== Análisis Descriptivo Variable Cualitativa ===\n")
print(tabla_descriptiva)

cat("\n=== Resumen General ===\n")
cat("N total:", N_total, "\n")
cat("N válidos:", N_valid, "\n")
cat("N missing:", N_missing, "\n")
cat("Porcentaje missing:", round(N_missing/N_total * 100, 2), "%\n")

# Moda
moda <- names(freq_table)[which.max(freq_table)]
cat("Moda:", moda, "(", categorias[moda], ")\n")

# Mostrar tabla de frecuencias original
cat("\n=== Tabla de Frecuencias ===\n")
print(freq_table)
cat("\n=== Proporciones ===\n")
print(round(prop_table, 4))

# ---- Celda 3: Análisis Gráfico para Variable Cualitativa --------------
library(ggplot2)
library(gridExtra)

plot_dir <- file.path(out_dir, "plots")
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

x <- df_var$value
x_nm <- x[!is.na(x)]

if (length(x_nm) == 0) {
  message("No hay datos no-missing: no se generan gráficos.")
} else {
  # Preparar datos para gráficos
  df_plot <- df_var %>% 
    filter(!is.na(value)) %>%
    mutate(
      categoria = factor(value, 
                        levels = names(categorias),
                        labels = categorias)
    )
  
  # Gráfico de barras (frecuencias)
  p1 <- ggplot(df_plot, aes(x = categoria)) +
    geom_bar(fill = "steelblue", alpha = 0.7) +
    labs(
      title = paste("Frecuencias de", varname_real),
      x = "Tipo de Vivienda",
      y = "Frecuencia"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Gráfico de barras (proporciones)
  p2 <- ggplot(df_plot, aes(x = categoria)) +
    geom_bar(aes(y = after_stat(prop), group = 1), fill = "darkgreen", alpha = 0.7) +
    labs(
      title = paste("Proporciones de", varname_real),
      x = "Tipo de Vivienda",
      y = "Proporción"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(labels = scales::percent_format())
  
  # Gráfico de pastel
  freq_data <- table(df_plot$categoria)
  pie_data <- data.frame(
    categoria = names(freq_data),
    frecuencia = as.numeric(freq_data),
    porcentaje = round(as.numeric(freq_data) / sum(freq_data) * 100, 1)
  )
  
  p3 <- ggplot(pie_data, aes(x = "", y = frecuencia, fill = categoria)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    labs(
      title = paste("Distribución de", varname_real),
      fill = "Tipo de Vivienda"
    ) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_text(aes(label = paste0(porcentaje, "%")), 
              position = position_stack(vjust = 0.5), 
              color = "white", size = 3, fontface = "bold")
  
  # Guardar gráficos individuales
  ggsave(file.path(plot_dir, paste0("barras_frecuencias_", varname_real, ".png")), p1, width = 12, height = 8)
  ggsave(file.path(plot_dir, paste0("barras_proporciones_", varname_real, ".png")), p2, width = 12, height = 8)
  ggsave(file.path(plot_dir, paste0("pie_chart_", varname_real, ".png")), p3, width = 10, height = 8)
  
  # Mostrar todos los gráficos juntos
  grid.arrange(p1, p2, p3, ncol = 2, nrow = 2)
  
  message("Gráficos guardados en: ", plot_dir)
}

# ---- Celda 4: Intervalos de Confianza para Proporciones ---------------
# Instalar paquete binom si no está disponible
if (!requireNamespace("binom", quietly = TRUE)) {
  install.packages("binom", repos = "https://cloud.r-project.org")
}
library(binom)

x <- df_var$value
x_nm <- x[!is.na(x)]

if (length(x_nm) > 0) {
  # Contar respuestas para cada categoría
  n_total <- length(x_nm)
  
  cat("=== Intervalos de Confianza para Proporciones ===\n")
  cat("Tamaño muestral (n):", n_total, "\n\n")
  
  # Calcular IC para cada categoría
  resultados_ic <- data.frame()
  
  for (cat_val in sort(unique(x_nm))) {
    n_cat <- sum(x_nm == cat_val)
    p_cat <- n_cat / n_total
    
    cat("Categoría", cat_val, "(", categorias[as.character(cat_val)], "):\n")
    cat("  Frecuencia:", n_cat, "\n")
    cat("  Proporción:", round(p_cat, 4), "(", round(p_cat * 100, 2), "%)\n")
    
    # Método manual (aproximación normal)
    if (n_cat > 0 && n_cat < n_total) {
      se_cat <- sqrt(p_cat * (1 - p_cat) / n_total)
      z_critico <- qnorm(0.975)
      
      ic_cat_manual <- c(
        max(0, p_cat - z_critico * se_cat),
        min(1, p_cat + z_critico * se_cat)
      )
      
      cat("  IC 95% (aproximación normal): [", 
          round(ic_cat_manual[1], 4), ", ", 
          round(ic_cat_manual[2], 4), "]\n")
      
      # Método exacto usando binom.test
      test_cat <- binom.test(n_cat, n_total, conf.level = 0.95)
      cat("  IC 95% (exacto): [", 
          round(test_cat$conf.int[1], 4), ", ", 
          round(test_cat$conf.int[2], 4), "]\n")
      
      # Método con paquete binom
      tryCatch({
        ic_cat_binom <- binom.confint(n_cat, n_total, conf.level = 0.95, methods = "exact")
        cat("  IC 95% (binom package): [", 
            round(ic_cat_binom$lower, 4), ", ", 
            round(ic_cat_binom$upper, 4), "]\n")
      }, error = function(e) {
        cat("  Nota: Error con paquete 'binom' para esta categoría.\n")
      })
      
      # Guardar resultados
      resultados_ic <- rbind(resultados_ic, data.frame(
        Categoria = cat_val,
        Descripcion = categorias[as.character(cat_val)],
        Frecuencia = n_cat,
        Proporcion = p_cat,
        IC_inf = ic_cat_manual[1],
        IC_sup = ic_cat_manual[2],
        IC_exacto_inf = test_cat$conf.int[1],
        IC_exacto_sup = test_cat$conf.int[2]
      ))
    } else {
      cat("  Nota: Categoría con frecuencia 0 o n_total, no se calcula IC.\n")
    }
    cat("\n")
  }
  
  # Mostrar tabla resumen de IC
  if (nrow(resultados_ic) > 0) {
    cat("=== Tabla Resumen de Intervalos de Confianza ===\n")
    print(resultados_ic)
  }
  
} else {
  cat("No hay datos válidos para calcular intervalos de confianza.\n")
}

# ---- Celda 5: Bootstrap para Proporciones (RÁPIDO) --------------
set.seed(12345)

x <- df_var$value
x_nm <- x[!is.na(x)]

if (length(x_nm) > 10) {
  B <- 100  # Número de réplicas bootstrap
  
  cat("=== Bootstrap para Proporciones ===\n")
  cat("Número de réplicas bootstrap:", B, "\n")
  cat("Tamaño muestral:", length(x_nm), "\n\n")
  
  # Bootstrap para cada categoría
  resultados_bootstrap <- data.frame()
  
  for (cat_val in sort(unique(x_nm))) {
    n_cat <- sum(x_nm == cat_val)
    p_original <- n_cat / length(x_nm)
    
    cat("Categoría", cat_val, "(", categorias[as.character(cat_val)], "):\n")
    cat("  Proporción original:", round(p_original, 4), "\n")
    
    # Bootstrap manual
    bootstrap_samples <- matrix(sample(x_nm, size = B * length(x_nm), replace = TRUE), 
                               nrow = B, ncol = length(x_nm))
    
    # Calcular proporciones para cada muestra bootstrap
    prop_bootstrap <- rowMeans(bootstrap_samples == cat_val)
    
    # Calcular estadísticas bootstrap
    mean_boot <- mean(prop_bootstrap)
    sd_boot <- sd(prop_bootstrap)
    bias_boot <- mean_boot - p_original
    
    # Intervalos de confianza bootstrap (percentil)
    ci_percentil <- quantile(prop_bootstrap, c(0.025, 0.975))
    
    cat("  IC 95% bootstrap (percentil): [", 
        round(ci_percentil[1], 4), ", ", 
        round(ci_percentil[2], 4), "]\n")
    cat("  Bias estimado:", round(bias_boot, 6), "\n")
    cat("  Error estándar bootstrap:", round(sd_boot, 4), "\n")
    
    # Comparación con método teórico
    se_teorico <- sqrt(p_original * (1 - p_original) / length(x_nm))
    cat("  Error estándar teórico:", round(se_teorico, 4), "\n")
    cat("  Diferencia relativa:", round(abs(sd_boot - se_teorico) / se_teorico * 100, 2), "%\n")
    
    # Guardar resultados
    resultados_bootstrap <- rbind(resultados_bootstrap, data.frame(
      Categoria = cat_val,
      Descripcion = categorias[as.character(cat_val)],
      Proporcion_original = p_original,
      IC_bootstrap_inf = ci_percentil[1],
      IC_bootstrap_sup = ci_percentil[2],
      Bias = bias_boot,
      EE_bootstrap = sd_boot,
      EE_teorico = se_teorico
    ))
    
    cat("\n")
  }
  
  # Mostrar tabla resumen de bootstrap
  cat("=== Tabla Resumen de Bootstrap ===\n")
  print(resultados_bootstrap)
  
} else {
  cat("No hay suficientes datos para bootstrap (n < 10).\n")
}

# ---- Celda 6: Consistencia para Variable Cualitativa ------------------
library(dplyr)
set.seed(123)

x <- df_var$value
x_nm <- x[!is.na(x)]

if (length(x_nm) > 30) {
  # Tamaños de submuestra
  ns <- unique(floor(seq(50, length(x_nm), length.out = 8)))
  reps <- 200  # repeticiones por tamaño
  
  # Seleccionar las categorías más frecuentes para el análisis
  freq_table <- table(x_nm)
  categorias_principales <- names(sort(freq_table, decreasing = TRUE))[1:3]  # Top 3
  
  cat("=== Análisis de Consistencia ===\n")
  cat("Categorías analizadas:", paste(categorias_principales, collapse = ", "), "\n")
  cat("Tamaños de submuestra:", paste(ns, collapse = ", "), "\n\n")
  
  res <- data.frame()
  
  for (n in ns) {
    ests_prop <- matrix(0, nrow = reps, ncol = length(categorias_principales))
    
    for (r in 1:reps) {
      s <- sample(x_nm, size = n, replace = FALSE)
      for (i in seq_along(categorias_principales)) {
        ests_prop[r, i] <- mean(s == as.numeric(categorias_principales[i]))
      }
    }
    
    # Calcular varianzas para cada categoría
    var_prop <- apply(ests_prop, 2, var)
    
    # Agregar a resultados
    for (i in seq_along(categorias_principales)) {
      res <- rbind(res, data.frame(
        n = n,
        categoria = categorias_principales[i],
        descripcion = categorias[as.character(categorias_principales[i])],
        var_prop = var_prop[i]
      ))
    }
  }
  
  print(res)
  
  # Gráfico de varianza vs n
  library(ggplot2)
  
  pcons <- ggplot(res, aes(x = n, y = var_prop, color = descripcion)) +
    geom_line() +
    geom_point() +
    labs(
      title = "Varianza del estimador vs tamaño muestral (submuestras)",
      subtitle = paste("Variable Cualitativa", varname_real),
      x = "n (submuestra)", 
      y = "Varianza estimada",
      color = "Categoría"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 8))
  
  print(pcons)
  ggsave(filename = file.path(out_dir, "consistencia_var_vs_n_cualitativa.png"), 
         plot = pcons, width = 12, height = 8)
  
  message("Gráfico de consistencia guardado en: ", 
          file.path(out_dir, "consistencia_var_vs_n_cualitativa.png"))
  
  cat("\nComentario (Consistencia):\n")
  cat("Si la varianza del estimador (proporción) disminuye al crecer n, evidencia consistencia.\n")
  cat("Para variables cualitativas, la consistencia se evalúa en términos de la estabilidad\n")
  cat("de las estimaciones de proporciones cuando aumenta el tamaño muestral.\n")
  
} else {
  cat("No hay suficientes datos (>30) para estudiar consistencia por submuestreo.\n")
}

# ---- Celda 7: Eficiencia y Resumen Final ------------------------------
library(dplyr)
library(boot)
set.seed(42)

x <- df_var$value
x_nm <- x[!is.na(x)]

if (length(x_nm) > 10) {
  # Para variables cualitativas, la eficiencia se puede evaluar comparando
  # la varianza de diferentes estimadores de proporción
  
  B <- 500
  
  cat("=== Análisis de Eficiencia ===\n")
  
  # Bootstrap para las categorías principales
  freq_table <- table(x_nm)
  categorias_principales <- names(sort(freq_table, decreasing = TRUE))[1:3]
  
  resultados_eficiencia <- data.frame()
  
  for (cat_val in categorias_principales) {
    # Bootstrap para proporción de esta categoría
    boot_prop <- boot(x_nm, 
                     statistic = function(d, i) mean(d[i] == as.numeric(cat_val)), 
                     R = B)
    
    var_prop_boot <- var(boot_prop$t)
    
    cat("Categoría", cat_val, "(", categorias[as.character(cat_val)], "):\n")
    cat("  Var(proporción) bootstrap:", round(var_prop_boot, 6), "\n")
    
    # Guardar resultados
    resultados_eficiencia <- rbind(resultados_eficiencia, data.frame(
      Categoria = cat_val,
      Descripcion = categorias[as.character(cat_val)],
      Var_bootstrap = var_prop_boot
    ))
  }
  
  cat("\n=== Tabla de Eficiencia ===\n")
  print(resultados_eficiencia)
  
  cat("\nComentario (Eficiencia):\n")
  cat("Para variables cualitativas, la eficiencia se evalúa comparando las varianzas\n")
  cat("de los estimadores de proporción para diferentes categorías.\n")
  cat("Categorías con menor varianza son más eficientes en sus estimaciones.\n")
  
} else {
  cat("No hay suficientes datos para evaluar eficiencia.\n")
}

# Resumen final y sintaxis
cat("\n=== Resumen del Análisis ===\n")
cat("Variable analizada:", varname_real, "\n")
cat("Tipo: Cualitativa nominal (6 categorías)\n")
cat("Descripción: Tipo de vivienda ocupada por el hogar\n")
cat("Total observaciones:", length(x), "\n")
cat("Observaciones válidas:", length(x_nm), "\n")
cat("Porcentaje missing:", round(sum(is.na(x))/length(x)*100, 2), "%\n")

if (length(x_nm) > 0) {
  freq_table <- table(x_nm)
  cat("\nDistribución de categorías:\n")
  for (i in seq_along(freq_table)) {
    cat_val <- names(freq_table)[i]
    prop_val <- as.numeric(freq_table[i]) / length(x_nm)
    cat("  ", cat_val, "(", categorias[cat_val], "): ", 
        round(prop_val, 4), " (", round(prop_val*100, 2), "%)\n")
  }
}

cat("\n--- Sintaxis clave usada en este análisis ---\n")
cat("1) Limpieza categórica: clean_categorical(...)\n")
cat("2) Descriptivos: table(), prop.table() para frecuencias y proporciones\n")
cat("3) Gráficas: geom_bar(), coord_polar() para barras y pie charts\n")
cat("4) IC proporciones: binom.confint() (exacto y asintótico)\n")
cat("5) Bootstrap: boot() para IC y bias de proporciones\n")
cat("6) Consistencia: submuestreo comparando var(proporción) vs n\n")
cat("7) Eficiencia: comparar var_bootstrap(proporción)\n")
cat("\nArchivos generados en:", out_dir, "\n")
