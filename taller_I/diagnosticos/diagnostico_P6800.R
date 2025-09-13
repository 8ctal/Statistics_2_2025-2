# Script de diagnóstico para P6800
# Verificar qué está pasando con los valores

library(readr)
library(dplyr)

# Función simple de limpieza
clean_simple <- function(x) {
  x <- as.character(x)
  x[ x %in% c("", "NA", NA) ] <- NA_character_
  x <- trimws(x)
  suppressWarnings(as.numeric(x))
}

# Verificar el archivo actual si existe
if (file.exists("Combinados/Combinado_P6800.csv")) {
  cat("=== DIAGNÓSTICO DEL ARCHIVO ACTUAL ===\n")
  datos_actuales <- read.csv("Combinados/Combinado_P6800.csv")
  
  cat("Primeros 20 valores de P6800:\n")
  print(head(datos_actuales$P6800, 20))
  
  cat("\nResumen estadístico:\n")
  print(summary(datos_actuales$P6800))
  
  cat("\nValores únicos en los primeros 50 registros:\n")
  print(unique(datos_actuales$P6800[1:50]))
  
  # Verificar si hay valores problemáticos
  cat("\nValores menores a 1 (posiblemente problemáticos):\n")
  valores_pequenos <- datos_actuales$P6800[datos_actuales$P6800 < 1]
  print(unique(valores_pequenos))
  
  cat("\nCantidad de valores menores a 1:", length(valores_pequenos), "\n")
  
  # Verificar valores extremos
  cat("\nValores mayores a 126:\n")
  valores_grandes <- datos_actuales$P6800[datos_actuales$P6800 > 126]
  print(unique(valores_grandes))
  
  cat("\nCantidad de valores mayores a 126:", length(valores_grandes), "\n")
  
  # Verificar distribución de horas
  cat("\nDistribución de horas por rangos:\n")
  cat("1-20 horas:", sum(datos_actuales$P6800 >= 1 & datos_actuales$P6800 <= 20, na.rm = TRUE), "\n")
  cat("21-40 horas:", sum(datos_actuales$P6800 >= 21 & datos_actuales$P6800 <= 40, na.rm = TRUE), "\n")
  cat("41-60 horas:", sum(datos_actuales$P6800 >= 41 & datos_actuales$P6800 <= 60, na.rm = TRUE), "\n")
  cat("61-80 horas:", sum(datos_actuales$P6800 >= 61 & datos_actuales$P6800 <= 80, na.rm = TRUE), "\n")
  cat("81-126 horas:", sum(datos_actuales$P6800 >= 81 & datos_actuales$P6800 <= 126, na.rm = TRUE), "\n")
}

# Verificar un archivo CSV original si existe
ruta_base <- "C:/UIS/Statistics/Statistics_II/datasets"
ruta_julio <- file.path(ruta_base, "Julio_2024", "CSV", "Ocupados.CSV")

if (file.exists(ruta_julio)) {
  cat("\n=== DIAGNÓSTICO DEL ARCHIVO ORIGINAL ===\n")
  
  # Leer como carácter
  archivo_original <- read_delim(ruta_julio, delim = ";", 
                                col_types = cols(.default = "c"),
                                locale = locale(encoding = "UTF-8"))
  
  if ("P6800" %in% names(archivo_original)) {
    cat("Primeros 20 valores originales de P6800:\n")
    print(head(archivo_original$P6800, 20))
    
    cat("\nTipo de datos:", class(archivo_original$P6800), "\n")
    
    cat("\nValores únicos en los primeros 50 registros:\n")
    print(unique(archivo_original$P6800[1:50]))
    
    # Aplicar limpieza simple
    valores_limpios <- clean_simple(archivo_original$P6800)
    
    cat("\nPrimeros 20 valores después de limpieza simple:\n")
    print(head(valores_limpios, 20))
    
    cat("\nResumen después de limpieza simple:\n")
    print(summary(valores_limpios))
    
    # Verificar rango esperado
    cat("\nValores dentro del rango esperado (1 - 126):\n")
    valores_rango <- valores_limpios[valores_limpios >= 1 & valores_limpios <= 126 & !is.na(valores_limpios)]
    cat("Cantidad:", length(valores_rango), "\n")
    cat("Porcentaje del total:", round(100 * length(valores_rango) / length(valores_limpios[!is.na(valores_limpios)]), 2), "%\n")
    
    # Verificar distribución
    cat("\nDistribución de horas (muestra original):\n")
    cat("1-20 horas:", sum(valores_rango >= 1 & valores_rango <= 20), "\n")
    cat("21-40 horas:", sum(valores_rango >= 21 & valores_rango <= 40), "\n")
    cat("41-60 horas:", sum(valores_rango >= 41 & valores_rango <= 60), "\n")
    cat("61-80 horas:", sum(valores_rango >= 61 & valores_rango <= 80), "\n")
    cat("81-126 horas:", sum(valores_rango >= 81 & valores_rango <= 126), "\n")
    
  } else {
    cat("La variable P6800 no se encontró en el archivo original\n")
    cat("Variables disponibles:", paste(names(archivo_original)[1:20], collapse = ", "), "\n")
  }
} else {
  cat("No se encontró el archivo original en:", ruta_julio, "\n")
}

cat("\n=== RECOMENDACIONES ===\n")
cat("1. P6800 representa horas semanales de trabajo\n")
cat("2. Rango esperado: 1 - 126 horas\n")
cat("3. Verificar que los valores estén dentro del rango lógico\n")
cat("4. Considerar análisis por rangos de horas (tiempo parcial, tiempo completo, etc.)\n")
cat("5. Comparar con P6500 (salarios) para análisis de productividad\n")
