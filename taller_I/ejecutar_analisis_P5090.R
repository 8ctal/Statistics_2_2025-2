# Script principal para ejecutar análisis completo de P5090
# 1. Combinar datos de los 3 meses
# 2. Ejecutar análisis estadístico completo


packs <- c("dplyr","ggplot2","MASS","boot","moments","DescTools","readr","stringr","gridExtra","ggplot2","binom","boot")
to_install <- packs[!(packs %in% rownames(installed.packages()))]
if (length(to_install)) install.packages(to_install)

cat("=== INICIANDO ANÁLISIS COMPLETO DE P5090 ===\n")
cat("Fecha:", Sys.time(), "\n\n")

# Paso 1: Combinar datos
cat("PASO 1: Combinando datos de los 3 meses...\n")
tryCatch({
  source("combinar_datos_P5090.R")
  cat("✅ Datos combinados exitosamente\n\n")
}, error = function(e) {
  stop(paste("❌ Error al combinar datos:", e$message))
})

# Verificar que se creó el archivo
if (!file.exists("Combinado.csv")) {
  stop("❌ No se pudo crear el archivo Combinado.csv")
}

# Paso 2: Ejecutar análisis estadístico
cat("PASO 2: Ejecutando análisis estadístico...\n")
tryCatch({
  source("analisis_V4650_cualitativa.R")
  cat("✅ Análisis estadístico completado exitosamente\n\n")
}, error = function(e) {
  stop(paste("❌ Error en el análisis estadístico:", e$message))
})

# Resumen final
cat("=== RESUMEN FINAL ===\n")
cat("✅ Archivos generados:\n")
cat("  - Combinado.csv (datos combinados)\n")
cat("  - Combinado_P5090.csv (datos con metadatos)\n")
cat("  - analisis_P5090_cualitativa/ (carpeta con resultados)\n")
cat("    - plots/ (gráficos)\n")
cat("    - consistencia_var_vs_n_cualitativa.png\n")

cat("\n📊 Análisis completado para variable P5090 (Tipo de vivienda)\n")
cat("🎯 Categorías analizadas: 6 tipos de vivienda\n")
cat("📈 Métodos aplicados: Descriptivos, IC, Bootstrap, Consistencia, Eficiencia\n")

cat("\n=== FIN DEL ANÁLISIS ===\n")
cat("Fecha de finalización:", Sys.time(), "\n")
