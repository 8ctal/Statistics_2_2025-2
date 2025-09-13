# Script para ejecutar la combinación de datos de P6500
# Este script combina los datos de P6500 desde los archivos "Ocupados.csv" 
# de los 3 meses: Julio_2024, Agosto_2024, Septiembre_2024

cat("=== EJECUTANDO COMBINACIÓN DE DATOS P6500 ===\n")
cat("Variable: P6500 - Antes de descuentos ¿cuánto ganó ... El mes pasado en este empleo?\n")
cat("Archivo fuente: Ocupados.csv\n")
cat("Meses: Julio_2024, Agosto_2024, Septiembre_2024\n")
cat("Agregación: DIRECTORIO (registro individual)\n\n")

# Ejecutar el script de combinación
source("Combinados/scripts/combinar_datos_P6500.R")

cat("\n=== COMBINACIÓN COMPLETADA ===\n")
cat("Archivos generados:\n")
cat("- Combinados/Combinado_P6500.csv (datos completos con mes)\n")
cat("- Combinados/Combinado_cuantitativo_P6500.csv (solo P6500 para análisis)\n\n")

cat("Para usar en el análisis dinámico:\n")
cat("1. Cambiar en scripts_analisis/analisis_variable_cuantitativa_dinamico.R:\n")
cat("   variable_name <- 'P6500'\n")
cat("   data_file <- 'Combinados/Combinado_cuantitativo_P6500.csv'\n\n")

cat("2. O ejecutar: ejecutar_analisis_P6500.R\n")
