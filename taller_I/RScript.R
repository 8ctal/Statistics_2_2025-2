# 1. Carga de datos
datos <- read.csv("GEIH_2024_jul_ago_sep.csv")

# 2. Agregación por directorio
library(dplyr)
agr <- datos %>%
  group_by(DIRECTORIO) %>%
  summarize(ingreso_prom = mean(ING_LAB, na.rm=TRUE),
            ocupados_prom = mean(OCUPADOS, na.rm=TRUE),
            edad_prom = mean(EDAD, na.rm=TRUE),
            area = first(AREA) )

# 3. Estadísticas descriptivas
library(psych)
describe(agr$ingreso_prom)

# 4. Visualización
library(ggplot2)
ggplot(agr, aes(x=ingreso_prom)) + geom_histogram()

# 5. Estimadores
media_muestra <- mean(agr$ingreso_prom)
fit <- fitdistr(agr$ingreso_prom, "normal")
ci <- t.test(agr$ingreso_prom)$conf.int

# 6. Bootstrap para sesgo
library(boot)
boot_mean <- function(data, i) mean(data[i])
b <- boot(agr$ingreso_prom, boot_mean, R=1000)
bias <- mean(b$t) - mean(agr$ingreso_prom)

# 7. Resumen para informe
summary_table <- data.frame(
  Variable = c("Ingreso promedio", "Ocupados promedio", "Edad promedio"),
  Media = c(mean(agr$ingreso_prom), mean(agr$ocupados_prom), mean(agr$edad_prom)),
  Mediana = c(median(agr$ingreso_prom), median(agr$ocupados_prom), median(agr$edad_prom)),
  Sesgo = c(bias, NA, NA)
)


library(readr)
library(dplyr)
library(plyr)
library(stringr)
getwd()

# ---------------------------
# Función para cargar un mes
# ---------------------------
cargar_mes <- function(nombre_mes, ruta_base) {
  ruta_mes <- file.path(ruta_base, nombre_mes, "CSV")
  
  # Cargar archivos
  general   <- read_delim(file.path(ruta_mes, "Características generales, seguridad social en salud y educación.CSV"),
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
  ocupados  <- read_delim(file.path(ruta_mes, "Ocupados.CSV"),
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
  migracion <- read_delim(file.path(ruta_mes, "Migración.CSV"),
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
  
  # Unificar el tipo de dato (si hace falta)
  ocupados$OFICIO_C8 <- as.character(ocupados$OFICIO_C8)
  
  # Crear lista de resúmenes como en tu ejemplo
  base_mes <- plyr::join_all(
    list(
      general %>%
        group_by(DIRECTORIO) %>%
        summarise(
          integrantes = n(),
          edad = mean(P6040),
          .groups = 'drop'
        ),
      ocupados %>%
        group_by(DIRECTORIO) %>%
        summarise(
          ocupados = n(),
          .groups = 'drop'
        ),
      general %>%
        filter(P6040 > 14) %>%
        group_by(DIRECTORIO) %>%
        dplyr::summarise(
          en_edad = n(),
          .groups = 'drop'
        ),
      migracion %>%
        filter(P3374 == 3) %>%
        group_by(DIRECTORIO) %>%
        summarise(
          migrantes = n(),
          .groups = 'drop'
        )
    ),
    by = 'DIRECTORIO', type = "left", match = "all"
  )
  
  # Variables derivadas
  base_mes <- base_mes %>%
    mutate(
      tasa_ocupacion = if_else(is.na(ocupados/en_edad), 0, ocupados/en_edad),
      migrante = factor(if_else(is.na(migrantes), "No", "Si"))
    )
  
  return(base_mes)
}

# ---------------------------
# Ejemplo de uso
# ---------------------------
ruta_base <- "C:/UIS/Statistics/Statistics_II/datasets"

base_julio   =  cargar_mes("Julio_2024", ruta_base)
base_agosto  =  cargar_mes("Agosto_2024", ruta_base)
base_sept    =  cargar_mes("Septiembre_2024", ruta_base)

# Consolidar todo
base_total = bind_rows(
  base_julio   %>% dplyr::mutate(mes = "Julio"),
  base_agosto  %>% dplyr::mutate(mes = "Agosto"),
  base_sept    %>% dplyr::mutate(mes = "Septiembre")
)

summary(base_total)
head(base_total)


ruta_base <- "C:/UIS/Statistics/Statistics_II/datasets"

test <- read_delim(
  file.path(ruta_base, "Septiembre_2024", "CSV", "Ocupados.CSV"),
  delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE
)

glimpse(test)
