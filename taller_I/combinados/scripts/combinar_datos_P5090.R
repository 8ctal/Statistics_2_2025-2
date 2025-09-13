# Script para combinar datos de P5090 (V4650) - Tipo de vivienda
# Combinar archivos de 3 meses usando DIRECTORIO como nivel de agregación

library(readr)
library(dplyr)

# Función para cargar y procesar un mes específico
cargar_mes_P5090 <- function(nombre_mes, ruta_base) {
  ruta_mes <- file.path(ruta_base, nombre_mes, "CSV")
  
  # Verificar que la ruta existe
  if (!dir.exists(ruta_mes)) {
    stop(paste("La ruta no existe:", ruta_mes))
  }
  
  # Cargar archivo de características generales (donde está P5090)
  tryCatch({
    general <- read_delim(file.path(ruta_mes, "Datos del hogar y la vivienda.CSV"),
                         delim = ";", escape_double = FALSE, trim_ws = TRUE, 
                         locale = locale(encoding = "UTF-8"))
  }, error = function(e) {
    stop(paste("Error al cargar archivo general para", nombre_mes, ":", e$message))
  })
  
  # Verificar que P5090 existe en el archivo
  if (!"P5090" %in% names(general)) {
    # Buscar variaciones del nombre
    posibles_nombres <- c("P5090", "p5090", "V4650", "v4650")
    nombre_encontrado <- NULL
    
    for (nom in posibles_nombres) {
      if (nom %in% names(general)) {
        nombre_encontrado <- nom
        break
      }
    }
    
    if (is.null(nombre_encontrado)) {
      stop(paste("No se encontró la variable P5090 en", nombre_mes, 
                 ". Variables disponibles:", paste(names(general)[1:10], collapse = ", ")))
    } else {
      # Renombrar la variable encontrada a P5090
      general <- general %>% rename(P5090 = !!sym(nombre_encontrado))
      cat("Variable", nombre_encontrado, "renombrada a P5090 en", nombre_mes, "\n")
    }
  }
  
  # Verificar que DIRECTORIO existe
  if (!"DIRECTORIO" %in% names(general)) {
    stop(paste("No se encontró la variable DIRECTORIO en", nombre_mes))
  }
  
  # Seleccionar solo las columnas necesarias y agregar por DIRECTORIO
  # Para P5090, tomamos el valor más frecuente por hogar (moda)
  resumen_mes <- general %>%
    group_by(DIRECTORIO) %>%
    summarise(
      P5090 = names(sort(table(P5090), decreasing = TRUE))[1],  # Moda
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

cat("=== Combinando datos de P5090 (Tipo de vivienda) ===\n")
cat("Ruta base:", ruta_base, "\n\n")

# Cargar datos de los 3 meses
tryCatch({
  cat("Cargando datos de Julio 2024...\n")
  base_julio <- cargar_mes_P5090("Julio_2024", ruta_base)
  
  cat("Cargando datos de Agosto 2024...\n")
  base_agosto <- cargar_mes_P5090("Agosto_2024", ruta_base)
  
  cat("Cargando datos de Septiembre 2024...\n")
  base_septiembre <- cargar_mes_P5090("Septiembre_2024", ruta_base)
  
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

cat("\nDistribución de P5090:\n")
print(table(base_combinada$P5090))

cat("\nPrimeras filas:\n")
print(head(base_combinada, 10))

# Limpiar y preparar datos finales
base_final <- base_combinada %>%
  mutate(
    P5090 = as.character(P5090),
    P5090 = case_when(
      P5090 == "1" ~ "1",
      P5090 == "2" ~ "2", 
      P5090 == "3" ~ "3",
      P5090 == "4" ~ "4",
      P5090 == "5" ~ "5",
      P5090 == "6" ~ "6",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(P5090))  # Eliminar valores missing

cat("\n=== Datos finales ===\n")
cat("Registros después de limpieza:", nrow(base_final), "\n")
cat("Distribución final de P5090:\n")
print(table(base_final$P5090))

# Guardar archivo combinado
write_csv(base_final, "Combinado_P5090.csv")

cat("\n✅ Archivo combinado guardado en: Combinado_P5090.csv\n")
cat("Total de hogares:", nrow(base_final), "\n")
cat("Distribución por mes:\n")
print(table(base_final$mes))

# Crear también un archivo con solo P5090 para compatibilidad
combinado_simple <- base_final %>% select(P5090)
write_csv(combinado_simple, "Combinado.csv")

cat("✅ Archivo simple guardado en: Combinado.csv (solo P5090)\n")
