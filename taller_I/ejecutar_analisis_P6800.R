# Script para ejecutar el análisis completo de P6800
# Primero combina los datos y luego ejecuta el análisis dinámico

cat("=== ANÁLISIS COMPLETO DE P6800 ===\n")
cat("Variable: P6800 - ¿cuántas horas a la semana trabaja normalmente ... En ese trabajo?\n")
cat("Archivo fuente: Ocupados.csv\n")
cat("Rango esperado: 1 - 126 horas\n\n")

# Paso 1: Combinar los datos de P6800
cat("PASO 1: Combinando datos de P6800 con función de limpieza mejorada...\n")
tryCatch({
  source("Combinados/scripts/combinar_datos_P6800.R")
  cat("✅ Datos combinados exitosamente\n\n")
}, error = function(e) {
  stop(paste("Error al combinar datos:", e$message))
})

# Verificar algunos valores después de la limpieza
cat("Verificando valores después de la limpieza:\n")
if (file.exists("Combinados/Combinado_cuantitativo_P6800.csv")) {
  datos_limpios <- read.csv("Combinados/Combinado_cuantitativo_P6800.csv")
  cat("Primeros 10 valores de P6800:\n")
  print(head(datos_limpios$P6800, 10))
  cat("Resumen estadístico:\n")
  print(summary(datos_limpios$P6800))
  cat("\n")
}

# Paso 2: Ejecutar el análisis dinámico
cat("PASO 2: Ejecutando análisis dinámico...\n")

# Configurar variables para el análisis dinámico
variable_name <- "P6800"
data_file <- "Combinados/Combinado_cuantitativo_P6800.csv"

# Verificar que el archivo existe
if (!file.exists(data_file)) {
  stop(paste("El archivo", data_file, "no existe. Ejecutar primero combinar_datos_P6800.R"))
}

# Modificar temporalmente el análisis dinámico
analisis_content <- readLines("scripts_analisis/analisis_variable_cuantitativa_dinamico.R")
analisis_content[5] <- paste0('variable_name <- "', variable_name, '"  # <- CAMBIAR SOLO ESTO')
analisis_content[6] <- paste0('data_file <- "', data_file, '"  # <- Y ESTO SI ES NECESARIO')

# Crear archivo temporal
writeLines(analisis_content, "scripts_analisis/analisis_P6800_temp.R")

# Ejecutar el análisis
tryCatch({
  source("scripts_analisis/analisis_P6800_temp.R")
  cat("✅ Análisis completado exitosamente\n\n")
}, error = function(e) {
  stop(paste("Error en el análisis:", e$message))
})

# Limpiar archivo temporal
file.remove("scripts_analisis/analisis_P6800_temp.R")

cat("=== ANÁLISIS COMPLETADO ===\n")
cat("Resultados guardados en: analisis_P6800_cuantitativa/\n")
cat("Archivos generados:\n")
cat("- Gráficos en: analisis_P6800_cuantitativa/plots/\n")
cat("- Gráfico de consistencia: analisis_P6800_cuantitativa/consistencia_var_vs_n.png\n")
cat("- Datos combinados: Combinados/Combinado_P6800.csv\n")
