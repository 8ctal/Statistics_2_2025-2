# Script de diagnóstico para P6500
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
if (file.exists("Combinados/Combinado_P6500.csv")) {
  cat("=== DIAGNÓSTICO DEL ARCHIVO ACTUAL ===\n")
  datos_actuales <- read.csv("Combinados/Combinado_P6500.csv")
  
  cat("Primeros 20 valores de P6500:\n")
  print(head(datos_actuales$P6500, 20))
  
  cat("\nResumen estadístico:\n")
  print(summary(datos_actuales$P6500))
  
  cat("\nValores únicos en los primeros 50 registros:\n")
  print(unique(datos_actuales$P6500[1:50]))
  
  # Verificar si hay valores problemáticos
  cat("\nValores menores a 1000 (posiblemente problemáticos):\n")
  valores_pequenos <- datos_actuales$P6500[datos_actuales$P6500 < 1000]
  print(unique(valores_pequenos))
  
  cat("\nCantidad de valores menores a 1000:", length(valores_pequenos), "\n")
  
  # Verificar valores extremos
  cat("\nValores mayores a 10,000,000:\n")
  valores_grandes <- datos_actuales$P6500[datos_actuales$P6500 > 10000000]
  print(unique(valores_grandes))
  
  cat("\nCantidad de valores mayores a 10,000,000:", length(valores_grandes), "\n")
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
  
  if ("P6500" %in% names(archivo_original)) {
    cat("Primeros 20 valores originales de P6500:\n")
    print(head(archivo_original$P6500, 20))
    
    cat("\nTipo de datos:", class(archivo_original$P6500), "\n")
    
    cat("\nValores únicos en los primeros 50 registros:\n")
    print(unique(archivo_original$P6500[1:50]))
    
    # Aplicar limpieza simple
    valores_limpios <- clean_simple(archivo_original$P6500)
    
    cat("\nPrimeros 20 valores después de limpieza simple:\n")
    print(head(valores_limpios, 20))
    
    cat("\nResumen después de limpieza simple:\n")
    print(summary(valores_limpios))
    
    # Verificar rango esperado
    cat("\nValores dentro del rango esperado (100 - 25,000,000):\n")
    valores_rango <- valores_limpios[valores_limpios >= 100 & valores_limpios <= 25000000 & !is.na(valores_limpios)]
    cat("Cantidad:", length(valores_rango), "\n")
    cat("Porcentaje del total:", round(100 * length(valores_rango) / length(valores_limpios[!is.na(valores_limpios)]), 2), "%\n")
    
  } else {
    cat("La variable P6500 no se encontró en el archivo original\n")
    cat("Variables disponibles:", paste(names(archivo_original)[1:20], collapse = ", "), "\n")
  }
} else {
  cat("No se encontró el archivo original en:", ruta_julio, "\n")
}

cat("\n=== RECOMENDACIONES ===\n")
cat("1. P6500 representa salarios antes de descuentos\n")
cat("2. Rango esperado: 100 - 25,000,000 pesos colombianos\n")
cat("3. Verificar que los valores estén dentro del rango lógico\n")
cat("4. Comparar con P6580S1 (bonificaciones) para consistencia\n")
