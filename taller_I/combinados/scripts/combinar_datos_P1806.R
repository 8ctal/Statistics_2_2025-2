# Script para combinar datos de P1806 - Remuneración/salario más bajo aceptado
# Combinar archivos de 3 meses usando DIRECTORIO como campo de agregación
# Buscar en archivos "No ocupados.csv" de cada mes
# Tomar todos los registros de cada mes sin agregar por hogar

library(readr)
library(dplyr)

# Función para cargar y procesar un mes específico
cargar_mes_P1806 <- function(nombre_mes, ruta_base) {
  ruta_mes <- file.path(ruta_base, nombre_mes, "CSV")
  
  # Verificar que la ruta existe
  if (!dir.exists(ruta_mes)) {
    stop(paste("La ruta no existe:", ruta_mes))
  }
  
  # Cargar archivo de No ocupados (donde está P1806)
  tryCatch({
    # Intentar leer como texto primero para preservar el formato original
    no_ocupados <- read_delim(file.path(ruta_mes, "No ocupados.CSV"),
                             delim = ";", escape_double = FALSE, trim_ws = FALSE, 
                             locale = locale(encoding = "UTF-8"),
                             col_types = cols(.default = "c"))  # Leer todo como carácter
  }, error = function(e) {
    stop(paste("Error al cargar archivo No ocupados para", nombre_mes, ":", e$message))
  })
  
  # Verificar que P1806 existe en el archivo
  if (!"P1806" %in% names(no_ocupados)) {
    # Buscar variaciones del nombre
    posibles_nombres <- c("P1806", "p1806", "P1806A1", "p1806a1")
    nombre_encontrado <- NULL
    
    for (nom in posibles_nombres) {
      if (nom %in% names(no_ocupados)) {
        nombre_encontrado <- nom
        break
      }
    }
    
    if (is.null(nombre_encontrado)) {
      cat("Variables disponibles en", nombre_mes, ":", paste(names(no_ocupados)[1:20], collapse = ", "), "\n")
      stop(paste("No se encontró la variable P1806 en", nombre_mes))
    } else {
      # Renombrar la variable encontrada a P1806
      no_ocupados <- no_ocupados %>% rename(P1806 = !!sym(nombre_encontrado))
      cat("Variable", nombre_encontrado, "renombrada a P1806 en", nombre_mes, "\n")
    }
  }
  
  # Verificar que DIRECTORIO existe
  if (!"DIRECTORIO" %in% names(no_ocupados)) {
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
  # Para P1806, simplemente tomamos todos los registros de cada mes
  resumen_mes <- no_ocupados %>%
    mutate(P1806_clean = clean_numeric2(P1806)) %>%
    select(DIRECTORIO, P1806_clean) %>%
    rename(P1806 = P1806_clean) %>%
    mutate(mes = nombre_mes)
  
  # Mostrar algunos ejemplos de limpieza para diagnóstico
  cat("Ejemplos de valores originales en", nombre_mes, ":\n")
  ejemplos_originales <- head(no_ocupados %>% select(P1806), 10)
  print(ejemplos_originales)
  
  cat("Ejemplos después de limpieza:\n")
  ejemplos_limpios <- head(no_ocupados %>% select(P1806) %>% mutate(P1806_clean = clean_numeric2(P1806)), 10)
  print(ejemplos_limpios)
  
  # Mostrar tipos de datos
  cat("Tipo de datos original:", class(no_ocupados$P1806[1]), "\n")
  cat("Valores únicos en los primeros 20 registros:\n")
  print(unique(no_ocupados$P1806[1:20]))
  
  cat("Procesado", nombre_mes, "- Registros:", nrow(resumen_mes), "\n")
  return(resumen_mes)
}

# Configuración de rutas
ruta_base <- "C:/UIS/Statistics/Statistics_II/datasets"

# Verificar que la ruta base existe
if (!dir.exists(ruta_base)) {
  stop(paste("La ruta base no existe:", ruta_base))
}

cat("=== Combinando datos de P1806 (Salario más bajo aceptado) ===\n")
cat("Ruta base:", ruta_base, "\n\n")

# Cargar datos de los 3 meses
tryCatch({
  cat("Cargando datos de Julio 2024...\n")
  base_julio <- cargar_mes_P1806("Julio_2024", ruta_base)
  
  cat("Cargando datos de Agosto 2024...\n")
  base_agosto <- cargar_mes_P1806("Agosto_2024", ruta_base)
  
  cat("Cargando datos de Septiembre 2024...\n")
  base_septiembre <- cargar_mes_P1806("Septiembre_2024", ruta_base)
  
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

cat("\nResumen de P1806:\n")
print(summary(base_combinada$P1806))

cat("\nPrimeras filas:\n")
print(head(base_combinada, 10))

# Limpiar y preparar datos finales
base_final <- base_combinada %>%
  filter(!is.na(P1806) & P1806 >= 0)  # Eliminar valores missing y negativos

cat("\n=== Datos finales ===\n")
cat("Registros después de limpieza:", nrow(base_final), "\n")
cat("Resumen final de P1806:\n")
print(summary(base_final$P1806))

# Guardar archivo combinado
write_csv(base_final, "Combinado_P1806.csv")

cat("\n✅ Archivo combinado guardado en: Combinado_P1806.csv\n")
cat("Total de hogares:", nrow(base_final), "\n")
cat("Distribución por mes:\n")
print(table(base_final$mes))

# Crear también un archivo con solo P1806 para compatibilidad con el análisis dinámico
combinado_simple <- base_final %>% select(P1806)
write_csv(combinado_simple, "Combinado_cuantitativo_P1806.csv")

cat("✅ Archivo simple guardado en: Combinado_cuantitativo_P1806.csv (solo P1806)\n")

# Información adicional sobre la variable
cat("\n=== Información sobre P1806 ===\n")
cat("Variable: P1806 - ¿Cuál sería la remuneración o el salario mensual más bajo por el que aceptaría?\n")
cat("Archivo fuente: No ocupados.csv\n")
cat("Agregación: Todos los registros de cada mes, usando DIRECTORIO como campo de agregación\n")
cat("Meses procesados: Julio_2024, Agosto_2024, Septiembre_2024\n")
