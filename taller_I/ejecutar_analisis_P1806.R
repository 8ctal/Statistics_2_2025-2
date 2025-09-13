  # Script para ejecutar el análisis completo de P1806
  # Primero combina los datos y luego ejecuta el análisis dinámico

  cat("=== ANÁLISIS COMPLETO DE P1806 ===\n")
  cat("Variable: P1806 - ¿Cuál sería la remuneración o el salario mensual más bajo por el que aceptaría?\n")
  cat("Archivo fuente: No ocupados.csv\n\n")

  # Paso 1: Combinar los datos de P1806
  cat("PASO 1: Combinando datos de P1806 con función de limpieza mejorada...\n")
  tryCatch({
    source("combinar_datos_P1806.R")
    cat("✅ Datos combinados exitosamente\n\n")
  }, error = function(e) {
    stop(paste("Error al combinar datos:", e$message))
  })

  # Verificar algunos valores después de la limpieza
  cat("Verificando valores después de la limpieza:\n")
  if (file.exists("Combinado_cuantitativo_P1806.csv")) {
    datos_limpios <- read.csv("Combinado_cuantitativo_P1806.csv")
    cat("Primeros 10 valores de P1806:\n")
    print(head(datos_limpios$P1806, 10))
    cat("Resumen estadístico:\n")
    print(summary(datos_limpios$P1806))
    cat("\n")
  }

  # Paso 2: Ejecutar el análisis dinámico
  cat("PASO 2: Ejecutando análisis dinámico...\n")

  # Configurar variables para el análisis dinámico
  variable_name <- "P1806"
  data_file <- "Combinado_cuantitativo_P1806.csv"

  # Verificar que el archivo existe
  if (!file.exists(data_file)) {
    stop(paste("El archivo", data_file, "no existe. Ejecutar primero combinar_datos_P1806.R"))
  }

  # Modificar temporalmente el análisis dinámico
  analisis_content <- readLines("analisis_variable_cuantitativa_dinamico.R")
  analisis_content[5] <- paste0('variable_name <- "', variable_name, '"  # <- CAMBIAR SOLO ESTO')
  analisis_content[6] <- paste0('data_file <- "', data_file, '"  # <- Y ESTO SI ES NECESARIO')

  # Crear archivo temporal
  writeLines(analisis_content, "analisis_P1806_temp.R")

  # Ejecutar el análisis
  tryCatch({
    source("analisis_P1806_temp.R")
    cat("✅ Análisis completado exitosamente\n\n")
  }, error = function(e) {
    stop(paste("Error en el análisis:", e$message))
  })

  # Limpiar archivo temporal
  file.remove("analisis_P1806_temp.R")

  cat("=== ANÁLISIS COMPLETADO ===\n")
  cat("Resultados guardados en: analisis_P1806_cuantitativa/\n")
  cat("Archivos generados:\n")
  cat("- Gráficos en: analisis_P1806_cuantitativa/plots/\n")
  cat("- Gráfico de consistencia: analisis_P1806_cuantitativa/consistencia_var_vs_n.png\n")
  cat("- Datos combinados: Combinado_P1806.csv\n")
