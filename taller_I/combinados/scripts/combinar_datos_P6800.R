# Script para combinar datos de P6800 - ¿cuántas horas a la semana trabaja normalmente ... En ese trabajo?
# Combinar archivos de 3 meses usando DIRECTORIO como campo de agregación
# Buscar en archivos "Ocupados.csv" de cada mes
# Tomar todos los registros de cada mes sin agregar por hogar

library(readr)
library(dplyr)

# Función para cargar y procesar un mes específico
cargar_mes_P6800 <- function(nombre_mes, ruta_base) {
  ruta_mes <- file.path(ruta_base, nombre_mes, "CSV")
  
  # Verificar que la ruta existe
  if (!dir.exists(ruta_mes)) {
    stop(paste("La ruta no existe:", ruta_mes))
  }
  
  # Cargar archivo de Ocupados (donde está P6800)
  tryCatch({
    # Intentar leer como texto primero para preservar el formato original
    ocupados <- read_delim(file.path(ruta_mes, "Ocupados.CSV"),
                          delim = ";", escape_double = FALSE, trim_ws = FALSE, 
                          locale = locale(encoding = "UTF-8"),
                          col_types = cols(.default = "c"))  # Leer todo como carácter
  }, error = function(e) {
    stop(paste("Error al cargar archivo Ocupados para", nombre_mes, ":", e$message))
  })
  
  # Verificar que P6800 existe en el archivo
  if (!"P6800" %in% names(ocupados)) {
    # Buscar variaciones del nombre
    posibles_nombres <- c("P6800", "p6800", "P6800A1", "p6800a1")
    nombre_encontrado <- NULL
    
    for (nom in posibles_nombres) {
      if (nom %in% names(ocupados)) {
        nombre_encontrado <- nom
        break
      }
    }
    
    if (is.null(nombre_encontrado)) {
      cat("Variables disponibles en", nombre_mes, ":", paste(names(ocupados)[1:20], collapse = ", "), "\n")
      stop(paste("No se encontró la variable P6800 en", nombre_mes))
    } else {
      # Renombrar la variable encontrada a P6800
      ocupados <- ocupados %>% rename(P6800 = !!sym(nombre_encontrado))
      cat("Variable", nombre_encontrado, "renombrada a P6800 en", nombre_mes, "\n")
    }
  }
  
  # Verificar que DIRECTORIO existe
  if (!"DIRECTORIO" %in% names(ocupados)) {
    stop(paste("No se encontró la variable DIRECTORIO en", nombre_mes))
  }
  
  # Función simple de limpieza numérica (solo manejar valores faltantes y espacios)
  clean_numeric2 <- function(x) {
    x <- as.character(x)
    x[ x %in% c("", "NA", NA) ] <- NA_character_
    x <- trimws(x)  # Solo remover espacios en blanco
    # Convertir directamente a numérico sin modificar el formato
    suppressWarnings(as.numeric(x))
  }
  
  # Seleccionar solo las columnas necesarias y limpiar datos
  # Para P6800, simplemente tomamos todos los registros de cada mes
  resumen_mes <- ocupados %>%
    mutate(P6800_clean = clean_numeric2(P6800)) %>%
    select(DIRECTORIO, P6800_clean) %>%
    rename(P6800 = P6800_clean) %>%
    mutate(mes = nombre_mes)
  
  # Mostrar algunos ejemplos de limpieza para diagnóstico
  cat("Ejemplos de valores originales en", nombre_mes, ":\n")
  ejemplos_originales <- head(ocupados %>% select(P6800), 10)
  print(ejemplos_originales)
  
  cat("Ejemplos después de limpieza:\n")
  ejemplos_limpios <- head(ocupados %>% select(P6800) %>% mutate(P6800_clean = clean_numeric2(P6800)), 10)
  print(ejemplos_limpios)
  
  # Mostrar tipos de datos
  cat("Tipo de datos original:", class(ocupados$P6800[1]), "\n")
  cat("Valores únicos en los primeros 20 registros:\n")
  print(unique(ocupados$P6800[1:20]))
  
  cat("Procesado", nombre_mes, "- Registros:", nrow(resumen_mes), "\n")
  return(resumen_mes)
}

# Configuración de rutas
ruta_base <- "C:/UIS/Statistics/Statistics_II/datasets"

# Verificar que la ruta base existe
if (!dir.exists(ruta_base)) {
  stop(paste("La ruta base no existe:", ruta_base))
}

cat("=== Combinando datos de P6800 (Horas semanales de trabajo) ===\n")
cat("Variable: P6800 - ¿cuántas horas a la semana trabaja normalmente ... En ese trabajo?\n")
cat("Archivo fuente: Ocupados.csv\n")
cat("Rango esperado: 1 - 126 horas\n")
cat("Ruta base:", ruta_base, "\n\n")

# Cargar datos de los 3 meses
tryCatch({
  cat("Cargando datos de Julio 2024...\n")
  base_julio <- cargar_mes_P6800("Julio_2024", ruta_base)
  
  cat("Cargando datos de Agosto 2024...\n")
  base_agosto <- cargar_mes_P6800("Agosto_2024", ruta_base)
  
  cat("Cargando datos de Septiembre 2024...\n")
  base_septiembre <- cargar_mes_P6800("Septiembre_2024", ruta_base)
  
  cat("Datos cargados exitosamente.\n")
}, error = function(e) {
  stop(paste("Error al cargar los datos:", e$message))
})

# Consolidar todos los meses
tryCatch({
  base_combinada <- bind_rows(
    base_julio,
    base_agosto,
    base_septiembre
  )
  
  cat("Datos consolidados exitosamente.\n")
  cat("Total de registros:", nrow(base_combinada), "\n")
}, error = function(e) {
  stop(paste("Error al consolidar los datos:", e$message))
})

# Verificar la estructura de los datos
cat("\n=== Verificación de datos ===\n")
cat("Estructura de la base combinada:\n")
str(base_combinada)

cat("\nDistribución por mes:\n")
print(table(base_combinada$mes))

cat("\nResumen de P6800:\n")
print(summary(base_combinada$P6800))

cat("\nPrimeras filas:\n")
print(head(base_combinada, 10))

# Limpiar y preparar datos finales
# Para P6800, filtrar valores dentro del rango esperado (1-126 horas)
base_final <- base_combinada %>%
  filter(!is.na(P6800) & P6800 >= 1 & P6800 <= 126)  # Eliminar valores missing y fuera del rango

cat("\n=== Datos finales ===\n")
cat("Registros después de limpieza:", nrow(base_final), "\n")
cat("Resumen final de P6800:\n")
print(summary(base_final$P6800))

# Guardar archivo combinado
write_csv(base_final, "./Combinado_P6800.csv")

cat("\n✅ Archivo combinado guardado en: Combinado_P6800.csv\n")
cat("Total de registros:", nrow(base_final), "\n")
cat("Distribución por mes:\n")
print(table(base_final$mes))

# Crear también un archivo con solo P6800 para compatibilidad con el análisis dinámico
combinado_simple <- base_final %>% select(P6800)
write_csv(combinado_simple, "./Combinado_cuantitativo_P6800.csv")

cat("✅ Archivo simple guardado en: Combinado_cuantitativo_P6800.csv (solo P6800)\n")

# Información adicional sobre la variable
cat("\n=== Información sobre P6800 ===\n")
cat("Variable: P6800 - ¿cuántas horas a la semana trabaja normalmente ... En ese trabajo?\n")
cat("Archivo fuente: Ocupados.csv\n")
cat("Agregación: Todos los registros de cada mes, usando DIRECTORIO como campo de agregación\n")
cat("Meses procesados: Julio_2024, Agosto_2024, Septiembre_2024\n")
cat("Rango esperado: 1 - 126 horas\n")
cat("Filtro aplicado: Solo valores entre 1 y 126 horas\n")
