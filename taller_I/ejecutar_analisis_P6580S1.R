# Script principal para ejecutar análisis completo de P6580S1
# 1. Combinar datos de los 3 meses
# 2. Ejecutar análisis estadístico completo

# Instalar paquetes necesarios
packs <- c("dplyr","ggplot2","MASS","boot","moments","DescTools","readr","stringr","gridExtra","binom")
to_install <- packs[!(packs %in% rownames(installed.packages()))]
if (length(to_install)) install.packages(to_install)

cat("=== INICIANDO ANÁLISIS COMPLETO DE P6580S1 ===\n")
cat("Variable: P6580S1 - Bonificación mensual\n")
cat("Fecha:", Sys.time(), "\n\n")

# Paso 1: Combinar datos
cat("PASO 1: Combinando datos de los 3 meses...\n")
tryCatch({
  source("combinar_datos_P6580S1.R")
  cat("✅ Datos combinados exitosamente\n\n")
}, error = function(e) {
  stop(paste("❌ Error al combinar datos:", e$message))
})

# Verificar que se creó el archivo
if (!file.exists("Combinado_cuantitativo.csv")) {
  stop("❌ No se pudo crear el archivo Combinado_cuantitativo.csv")
}

# Paso 2: Ejecutar análisis estadístico
cat("PASO 2: Ejecutando análisis estadístico...\n")
tryCatch({
  source("analisis_P6580S1_cuantitativa.R")
  cat("✅ Análisis estadístico completado exitosamente\n\n")
}, error = function(e) {
  stop(paste("❌ Error en el análisis estadístico:", e$message))
})

# Resumen final
cat("=== RESUMEN FINAL ===\n")
cat("✅ Archivos generados:\n")
cat("  - Combinado_cuantitativo.csv (datos combinados)\n")
cat("  - Combinado_P6580S1.csv (datos con metadatos)\n")
cat("  - analisis_P6580S1_cuantitativa/ (carpeta con resultados)\n")
cat("    - plots/ (gráficos)\n")
cat("    - consistencia_var_vs_n.png\n")

cat("\n📊 Análisis completado para variable P6580S1 (Bonificación mensual)\n")
cat("🎯 Tipo: Variable cuantitativa continua\n")
cat("📈 Métodos aplicados: Descriptivos, IC, Bootstrap, Consistencia, Eficiencia\n")

cat("\n=== FIN DEL ANÁLISIS ===\n")
cat("Fecha de finalización:", Sys.time(), "\n")
