# Script para ejecutar la combinación de datos de P6800
# Este script combina los datos de P6800 desde los archivos "Ocupados.csv" 
# de los 3 meses: Julio_2024, Agosto_2024, Septiembre_2024

cat("=== EJECUTANDO COMBINACIÓN DE DATOS P6800 ===\n")
cat("Variable: P6800 - ¿cuántas horas a la semana trabaja normalmente ... En ese trabajo?\n")
cat("Archivo fuente: Ocupados.csv\n")
cat("Meses: Julio_2024, Agosto_2024, Septiembre_2024\n")
cat("Agregación: DIRECTORIO (registro individual)\n")
cat("Rango esperado: 1 - 126 horas\n\n")

# Ejecutar el script de combinación
source("Combinados/scripts/combinar_datos_P6800.R")

cat("\n=== COMBINACIÓN COMPLETADA ===\n")
cat("Archivos generados:\n")
cat("- Combinados/Combinado_P6800.csv (datos completos con mes)\n")
cat("- Combinados/Combinado_cuantitativo_P6800.csv (solo P6800 para análisis)\n\n")

cat("Para usar en el análisis dinámico:\n")
cat("1. Cambiar en scripts_analisis/analisis_variable_cuantitativa_dinamico.R:\n")
cat("   variable_name <- 'P6800'\n")
cat("   data_file <- 'Combinados/Combinado_cuantitativo_P6800.csv'\n\n")

cat("2. O ejecutar: ejecutar_analisis_P6800.R\n")
