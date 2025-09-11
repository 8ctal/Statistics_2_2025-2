#### Importación de Datos
library(readr)
# Datos generales
general_septiembre <- read_delim("C:/UIS/Statistics/Statistics_II/datasets/Septiembre_2024/CSV/Características generales, seguridad social en salud y educación.CSV", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)
general_agosto <- read_delim("C:/UIS/Statistics/Statistics_II/datasets/Agosto_2024/CSV/Características generales, seguridad social en salud y educación.CSV", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)
# Datos ocupacion
ocupados_septiembre <- read_delim("C:/UIS/Statistics/Statistics_II/datasets/Septiembre_2024/CSV/Ocupados.CSV", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)
ocupados_agosto <- read_delim("C:/UIS/Statistics/Statistics_II/datasets/Agosto_2024/CSV/Ocupados.CSV", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)
# Datos migracion
migracion_septiembre <- read_delim("C:/UIS/Statistics/Statistics_II/datasets/Septiembre_2024/CSV/Migración.CSV", 
                                  delim = ";", escape_double = FALSE, trim_ws = TRUE)
migracion_agosto <- read_delim("C:/UIS/Statistics/Statistics_II/datasets/Agosto_2024/CSV/Migración.CSV", 
                                  delim = ";", escape_double = FALSE, trim_ws = TRUE)

#### Procesamiento

library(dplyr)

general <- bind_rows(
  general_septiembre,
  general_agosto
)

ocupados <- bind_rows(
  ocupados_septiembre,
  ocupados_agosto
)

ocupados_agosto$OFICIO_C8 <- as.character(ocupados_agosto$OFICIO_C8)

migracion <- bind_rows(
  migracion_septiembre,
  migracion_agosto
)

base <- plyr::join_all(
list(
general %>%
  group_by(DIRECTORIO) %>%
  summarise(
    integrantes = n(),
    edad = mean(P6040),
    .groups = 'drop'
  )
,
ocupados %>%
  group_by(DIRECTORIO) %>%
  summarise(
    ocupados = n(),
    .groups = 'drop'
  ),
general %>%
  filter(P6040 > 14) %>% # La ley permite que los mayores de 14 trabajen
  group_by(DIRECTORIO) %>%
  summarise(
    en_edad = n()
  )
,
migracion %>%
  filter(P3374 == 3) %>%
  group_by(DIRECTORIO) %>%
  summarise(
    migrantes = n()
  )
)
,
by = 'DIRECTORIO', type = "left", match = "all"
)

base %>% summary()

base <- base %>%
  mutate(
    tasa_ocupacion = if_else(is.na(ocupados/en_edad), 0, ocupados/en_edad) ,
    migrante = factor(if_else(is.na(migrantes), "No", "Si"))
  )

