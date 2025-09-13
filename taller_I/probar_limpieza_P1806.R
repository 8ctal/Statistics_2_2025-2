# Script para probar la función de limpieza de P1806
# Verificar que los valores se están leyendo correctamente

# Función mejorada de limpieza numérica
clean_numeric2 <- function(x) {
  x <- as.character(x)
  x[ x %in% c("", "NA", NA) ] <- NA_character_
  x <- trimws(x)
  x <- gsub("\\s+", "", x)
  
  # Remover caracteres no numéricos excepto comas y puntos
  x <- gsub("[^0-9\\,\\.]", "", x)
  
  # Detectar patrones de separadores de miles y decimales
  # Patrón 1: 1,234,567.89 (separador de miles con comas, decimal con punto)
  pattern_thousands_comma <- grepl("^[0-9]{1,3}(,[0-9]{3})*\\.[0-9]+$", x)
  
  # Patrón 2: 1.234.567,89 (separador de miles con puntos, decimal con coma)
  pattern_thousands_dot <- grepl("^[0-9]{1,3}(\\.[0-9]{3})*,[0-9]+$", x)
  
  # Patrón 3: 1234567,89 (solo decimal con coma)
  pattern_decimal_comma <- grepl("^[0-9]+,[0-9]+$", x) & !grepl("\\.", x)
  
  # Patrón 4: 1234567.89 (solo decimal con punto)
  pattern_decimal_dot <- grepl("^[0-9]+\\.[0-9]+$", x) & !grepl(",", x)
  
  # Aplicar limpieza según el patrón detectado
  x[pattern_thousands_comma] <- gsub(",", "", x[pattern_thousands_comma])  # Remover comas de miles
  x[pattern_thousands_dot] <- gsub("\\.", "", x[pattern_thousands_dot])    # Remover puntos de miles
  x[pattern_thousands_dot] <- gsub(",", ".", x[pattern_thousands_dot])     # Cambiar coma decimal a punto
  x[pattern_decimal_comma] <- gsub(",", ".", x[pattern_decimal_comma])     # Cambiar coma decimal a punto
  
  # Para valores sin separadores, mantener como están
  suppressWarnings(as.numeric(x))
}

# Casos de prueba
casos_prueba <- c(
  "500000",      # Valor simple
  "1,300,000",   # Separador de miles con comas
  "1.300.000",   # Separador de miles con puntos
  "1,300,000.50", # Miles con comas y decimal con punto
  "1.300.000,50", # Miles con puntos y decimal con coma
  "500,50",      # Decimal con coma
  "500.50",      # Decimal con punto
  "7858654,505", # El caso problemático
  "1300000",     # Sin separadores
  "NA",          # Valor faltante
  "",            # Vacío
  "abc123def"    # Con caracteres no numéricos
)

cat("=== PRUEBA DE LIMPIEZA DE DATOS ===\n")
cat("Probando diferentes formatos de valores numéricos:\n\n")

resultados <- data.frame(
  Original = casos_prueba,
  Limpio = clean_numeric2(casos_prueba),
  stringsAsFactors = FALSE
)

print(resultados)

cat("\n=== CASOS ESPECÍFICOS PARA P1806 ===\n")
cat("El valor problemático '7858654,505' debería convertirse a 7858654.505\n")
cat("Valor original: 7858654,505\n")
cat("Valor limpio:", clean_numeric2("7858654,505"), "\n")

cat("\nSi el valor debería ser 500000, verificar el formato original en el CSV.\n")
cat("Posibles causas:\n")
cat("1. El CSV tiene formato incorrecto\n")
cat("2. La función read_delim está interpretando mal los separadores\n")
cat("3. El valor original en el Excel es diferente\n")
