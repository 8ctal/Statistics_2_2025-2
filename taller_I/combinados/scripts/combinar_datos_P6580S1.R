# Script para combinar datos de P6580S1 - Bonificación mensual
# Combinar archivos de 3 meses usando DIRECTORIO como nivel de agregación

library(readr)
library(dplyr)

# Función para cargar y procesar un mes específico
cargar_mes_P6580S1 <- function(nombre_mes, ruta_base) {
  ruta_mes <- file.path(ruta_base, nombre_mes, "CSV")
  
  # Verificar que la ruta existe
  if (!dir.exists(ruta_mes)) {
    stop(paste("La ruta no existe:", ruta_mes))
  }
  
  # Cargar archivo de ocupados (donde está P6580S1)
  tryCatch({
    ocupados <- read_delim(file.path(ruta_mes, "Ocupados.CSV"),
                          delim = ";", escape_double = FALSE, trim_ws = TRUE, 
                          locale = locale(encoding = "UTF-8"))
  }, error = function(e) {
    stop(paste("Error al cargar archivo ocupados para", nombre_mes, ":", e$message))
  })
  
  # Verificar que P6580S1 existe en el archivo
  if (!"P6580S1" %in% names(ocupados)) {
    # Buscar variaciones del nombre
    posibles_nombres <- c("P6580S1", "p6580s1", "P6580S1A1", "p6580s1a1")
    nombre_encontrado <- NULL
    
    for (nom in posibles_nombres) {
      if (nom %in% names(ocupados)) {
        nombre_encontrado <- nom
        break
      }
    }
    
    if (is.null(nombre_encontrado)) {
      stop(paste("No se encontró la variable P6580S1 en", nombre_mes, 
                 ". Variables disponibles:", paste(names(ocupados)[1:10], collapse = ", ")))
    } else {
      # Renombrar la variable encontrada a P6580S1
      ocupados <- ocupados %>% rename(P6580S1 = !!sym(nombre_encontrado))
      cat("Variable", nombre_encontrado, "renombrada a P6580S1 en", nombre_mes, "\n")
    }
  }
  
  # Verificar que DIRECTORIO existe
  if (!"DIRECTORIO" %in% names(ocupados)) {
    stop(paste("No se encontró la variable DIRECTORIO en", nombre_mes))
  }
  
  # Función robusta de limpieza numérica (monedas, separadores, etc.)
  clean_numeric2 <- function(x) {
    x <- as.character(x)
    x[ x %in% c("", "NA", NA) ] <- NA_character_
    x <- trimws(x)
    x <- gsub("\\s+", "", x)
    x <- gsub("[^0-9\\,\\.-]", "", x)
    both <- grepl("\\.", x) & grepl(",", x)
    if (any(both)) { x[both] <- gsub("\\.", "", x[both]); x[both] <- gsub(",", ".", x[both]) }
    only_comma <- grepl(",", x) & !grepl("\\.", x)
    x[only_comma] <- gsub(",", ".", x[only_comma])
    suppressWarnings(as.numeric(x))
  }
  
  # Seleccionar solo las columnas necesarias y agregar por DIRECTORIO
  # Para P6580S1, tomamos la suma por hogar (total de bonificaciones)
  resumen_mes <- ocupados %>%
    mutate(P6580S1_clean = clean_numeric2(P6580S1)) %>%
    group_by(DIRECTORIO) %>%
    summarise(
      P6580S1 = sum(P6580S1_clean, na.rm = TRUE),  # Suma de bonificaciones por hogar
      .groups = 'drop'
    ) %>%
    mutate(mes = nombre_mes)
  
  cat("Procesado", nombre_mes, "- Registros:", nrow(resumen_mes), "\n")
  return(resumen_mes)
}

# Configuración de rutas
ruta_base <- "C:/UIS/Statistics/Statistics_II/datasets"

# Verificar que la ruta base existe
if (!dir.exists(ruta_base)) {
  stop(paste("La ruta base no existe:", ruta_base))
}

cat("=== Combinando datos de P6580S1 (Bonificación mensual) ===\n")
cat("Ruta base:", ruta_base, "\n\n")

# Cargar datos de los 3 meses
tryCatch({
  cat("Cargando datos de Julio 2024...\n")
  base_julio <- cargar_mes_P6580S1("Julio_2024", ruta_base)
  
  cat("Cargando datos de Agosto 2024...\n")
  base_agosto <- cargar_mes_P6580S1("Agosto_2024", ruta_base)
  
  cat("Cargando datos de Septiembre 2024...\n")
  base_septiembre <- cargar_mes_P6580S1("Septiembre_2024", ruta_base)
  
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

cat("\nResumen de P6580S1:\n")
print(summary(base_combinada$P6580S1))

cat("\nPrimeras filas:\n")
print(head(base_combinada, 10))

# Limpiar y preparar datos finales
base_final <- base_combinada %>%
  filter(!is.na(P6580S1) & P6580S1 >= 0)  # Eliminar valores missing y negativos

cat("\n=== Datos finales ===\n")
cat("Registros después de limpieza:", nrow(base_final), "\n")
cat("Resumen final de P6580S1:\n")
print(summary(base_final$P6580S1))

# Guardar archivo combinado
write_csv(base_final, "Combinado_P6580S1.csv")

cat("\n✅ Archivo combinado guardado en: Combinado_P6580S1.csv\n")
cat("Total de hogares:", nrow(base_final), "\n")
cat("Distribución por mes:\n")
print(table(base_final$mes))

# Crear también un archivo con solo P6580S1 para compatibilidad
combinado_simple <- base_final %>% select(P6580S1)
write_csv(combinado_simple, "Combinado_cuantitativo.csv")

cat("✅ Archivo simple guardado en: Combinado_cuantitativo.csv (solo P6580S1)\n")
