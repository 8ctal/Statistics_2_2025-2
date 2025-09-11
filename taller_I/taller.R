    # Instalar si faltan
    packs <- c("dplyr","ggplot2","MASS","boot","moments","DescTools","readr","stringr")
    to_install <- packs[!(packs %in% rownames(installed.packages()))]
    if (length(to_install)) install.packages(to_install)

    library(dplyr)
    library(ggplot2)
    library(MASS)
    library(boot)
    library(moments)
    library(DescTools)
    library(readr)
    library(stringr)
    getwd()
    # ---------------------------
    # ---------------------------
    # Función para cargar un mes
    # ---------------------------
    cargar_mes <- function(nombre_mes, ruta_base) {
    ruta_mes <- file.path(ruta_base, nombre_mes, "CSV")
    
    # Verificar que la ruta existe
    if (!dir.exists(ruta_mes)) {
        stop(paste("La ruta no existe:", ruta_mes))
    }
    
    # Cargar archivos con manejo de errores
    tryCatch({
        general   <- read_delim(file.path(ruta_mes, "Características generales, seguridad social en salud y educación.CSV"),
                                delim = ";", escape_double = FALSE, trim_ws = TRUE, 
                                locale = locale(encoding = "UTF-8"))
        ocupados  <- read_delim(file.path(ruta_mes, "Ocupados.CSV"),
                                delim = ";", escape_double = FALSE, trim_ws = TRUE,
                                locale = locale(encoding = "UTF-8"))
        migracion <- read_delim(file.path(ruta_mes, "Migración.CSV"),
                                delim = ";", escape_double = FALSE, trim_ws = TRUE,
                                locale = locale(encoding = "UTF-8"))
    }, error = function(e) {
        stop(paste("Error al cargar archivos para", nombre_mes, ":", e$message))
    })
    
    # Verificar problemas de parsing
    if (nrow(problems(general)) > 0) {
        cat("Problemas de parsing en archivo general:\n")
        print(problems(general))
        cat("Continuando con los datos disponibles...\n")
    }
    if (nrow(problems(ocupados)) > 0) {
        cat("Problemas de parsing en archivo ocupados:\n")
        print(problems(ocupados))
        cat("Continuando con los datos disponibles...\n")
    }
    if (nrow(problems(migracion)) > 0) {
        cat("Problemas de parsing en archivo migración:\n")
        print(problems(migracion))
        cat("Continuando con los datos disponibles...\n")
    }
    
    # Verificar que las columnas necesarias existen
    required_cols_general <- c("DIRECTORIO", "P6040")
    required_cols_ocupados <- c("DIRECTORIO")
    required_cols_migracion <- c("DIRECTORIO", "P3374")
    
    if (!all(required_cols_general %in% names(general))) {
        stop(paste("Columnas faltantes en archivo general:", 
                   setdiff(required_cols_general, names(general))))
    }
    if (!all(required_cols_ocupados %in% names(ocupados))) {
        stop(paste("Columnas faltantes en archivo ocupados:", 
                   setdiff(required_cols_ocupados, names(ocupados))))
    }
    if (!all(required_cols_migracion %in% names(migracion))) {
        stop(paste("Columnas faltantes en archivo migración:", 
                   setdiff(required_cols_migracion, names(migracion))))
    }
    
    # Verificar que tenemos datos
    if (nrow(general) == 0) {
        stop("El archivo general está vacío")
    }
    if (nrow(ocupados) == 0) {
        warning("El archivo ocupados está vacío")
    }
    if (nrow(migracion) == 0) {
        warning("El archivo migración está vacío")
    }
    
    # Unificar el tipo de dato (si hace falta)
    if ("OFICIO_C8" %in% names(ocupados)) {
        ocupados$OFICIO_C8 <- as.character(ocupados$OFICIO_C8)
    }
    
    # Crear resúmenes usando dplyr (evitando conflicto con plyr)
    tryCatch({
        # Resumen de general
        resumen_general <- general %>%
            group_by(DIRECTORIO) %>%
            summarise(
                integrantes = n(),
                edad = mean(P6040, na.rm = TRUE),
                .groups = 'drop'
            )
        
        # Resumen de ocupados
        resumen_ocupados <- ocupados %>%
            group_by(DIRECTORIO) %>%
            summarise(
                ocupados = n(),
                .groups = 'drop'
            )
        
        # Resumen de personas en edad de trabajar
        resumen_edad_trabajo <- general %>%
            filter(P6040 > 14) %>%
            group_by(DIRECTORIO) %>%
            summarise(
                en_edad = n(),
                .groups = 'drop'
            )
        
        # Resumen de migrantes
        resumen_migrantes <- migracion %>%
            filter(P3374 == 3) %>%
            group_by(DIRECTORIO) %>%
            summarise(
                migrantes = n(),
                .groups = 'drop'
            )
        
        # Unir todos los resúmenes usando left_join
        base_mes <- resumen_general %>%
            left_join(resumen_ocupados, by = "DIRECTORIO") %>%
            left_join(resumen_edad_trabajo, by = "DIRECTORIO") %>%
            left_join(resumen_migrantes, by = "DIRECTORIO")
            
    }, error = function(e) {
        stop(paste("Error al procesar los datos para", nombre_mes, ":", e$message))
    })
    
    # Variables derivadas
    tryCatch({
        base_mes <- base_mes %>%
            mutate(
            tasa_ocupacion = if_else(is.na(ocupados/en_edad) | en_edad == 0, 0, ocupados/en_edad),
            migrante = factor(if_else(is.na(migrantes), "No", "Si"))
            )
    }, error = function(e) {
        stop(paste("Error al crear variables derivadas para", nombre_mes, ":", e$message))
    })
    
    # Verificar que se crearon las variables correctamente
    if (nrow(base_mes) == 0) {
        warning(paste("No se encontraron datos para", nombre_mes))
    }
    
    return(base_mes)
    }

    # ---------------------------
    # Ejemplo de uso
    # ---------------------------
    ruta_base <- "C:/UIS/Statistics/Statistics_II/datasets"

    # Cargar datos con manejo de errores
    tryCatch({
        cat("Cargando datos de Julio 2024...\n")
        base_julio   =  cargar_mes("Julio_2024", ruta_base)
        cat("Cargando datos de Agosto 2024...\n")
        base_agosto  =  cargar_mes("Agosto_2024", ruta_base)
        cat("Cargando datos de Septiembre 2024...\n")
        base_sept    =  cargar_mes("Septiembre_2024", ruta_base)
        cat("Datos cargados exitosamente.\n")
    }, error = function(e) {
        stop(paste("Error al cargar los datos:", e$message))
    })

    # Consolidar todo
    tryCatch({
        base_total = bind_rows(
        base_julio   %>% dplyr::mutate(mes = "Julio"),
        base_agosto  %>% dplyr::mutate(mes = "Agosto"),
        base_sept    %>% dplyr::mutate(mes = "Septiembre")
        )
    }, error = function(e) {
        stop(paste("Error al consolidar los datos:", e$message))
    })
    
    # Verificar que se consolidaron los datos correctamente
    if (nrow(base_total) == 0) {
        stop("No se pudieron consolidar los datos. Verifique que los archivos existen y tienen el formato correcto.")
    }
    
    cat(paste("Datos consolidados exitosamente. Total de registros:", nrow(base_total), "\n"))

    summary(base_total)
    head(base_total)

    # Nos aseguramos de que existan las columnas necesarias
    required_cols <- c("integrantes","edad","tasa_ocupacion","migrante","mes")
    missing_cols <- setdiff(required_cols, names(base_total))
    if (length(missing_cols) > 0) {
        stop(paste("Columnas faltantes en base_total:", paste(missing_cols, collapse = ", ")))
    }

    # Coerciones prudentes
    tryCatch({
        base_total <- base_total %>%
        mutate(
            integrantes = as.numeric(integrantes),
            edad        = as.numeric(edad),
            tasa_ocupacion = as.numeric(tasa_ocupacion),
            migrante    = factor(migrante, levels = c("No","Si")),
            mes         = as.character(mes)
        )
    }, error = function(e) {
        stop(paste("Error al convertir tipos de datos:", e$message))
    })
    
    # Verificar que las coerciones funcionaron
    if (any(is.na(base_total$integrantes)) && !all(is.na(base_total$integrantes))) {
        warning("Algunos valores de 'integrantes' no se pudieron convertir a numérico")
    }
    if (any(is.na(base_total$edad)) && !all(is.na(base_total$edad))) {
        warning("Algunos valores de 'edad' no se pudieron convertir a numérico")
    }
    if (any(is.na(base_total$tasa_ocupacion)) && !all(is.na(base_total$tasa_ocupacion))) {
        warning("Algunos valores de 'tasa_ocupacion' no se pudieron convertir a numérico")
    }
    
    cat("Tipos de datos convertidos exitosamente.\n")

    resumen_var <- function(x) {
    tryCatch({
        tibble::tibble(
            n        = sum(!is.na(x)),
            NA_total = sum(is.na(x)),
            media    = mean(x, na.rm = TRUE),
            mediana  = median(x, na.rm = TRUE),
            moda     = tryCatch(DescTools::Mode(x, na.rm = TRUE), error = function(e) NA),
            var      = var(x, na.rm = TRUE),
            sd       = sd(x, na.rm = TRUE),
            min      = min(x, na.rm = TRUE),
            q1       = quantile(x, 0.25, na.rm = TRUE),
            q3       = quantile(x, 0.75, na.rm = TRUE),
            max      = max(x, na.rm = TRUE),
            IQR      = IQR(x, na.rm = TRUE),
            CV       = sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE),
            asimetria= moments::skewness(x, na.rm = TRUE),
            curtosis = moments::kurtosis(x, na.rm = TRUE),
            p10      = quantile(x, 0.10, na.rm = TRUE),
            p90      = quantile(x, 0.90, na.rm = TRUE)
        )
    }, error = function(e) {
        stop(paste("Error al calcular resumen de variable:", e$message))
    })
    }

    cat("Calculando resúmenes descriptivos...\n")
    desc_integrantes   <- resumen_var(base_total$integrantes)
    desc_edad          <- resumen_var(base_total$edad)
    desc_tasa_ocup     <- resumen_var(base_total$tasa_ocupacion)
    cat("Resúmenes descriptivos calculados exitosamente.\n")

    # Resumen por mes (útil para comparar estacionalidad)
    tryCatch({
        desc_por_mes <- base_total %>%
        group_by(mes) %>%
        summarise(
            n = dplyr::n(),
            media_integrantes = mean(integrantes, na.rm=TRUE),
            media_edad        = mean(edad, na.rm=TRUE),
            media_tasa_ocup   = mean(tasa_ocupacion, na.rm=TRUE),
            prop_migrante_si  = mean(migrante == "Si", na.rm=TRUE)
        )
    }, error = function(e) {
        stop(paste("Error al calcular resumen por mes:", e$message))
    })
    cat("Mostrando resúmenes descriptivos:\n")
    print("Resumen de integrantes:")
    print(desc_integrantes)
    print("Resumen de edad:")
    print(desc_edad)
    print("Resumen de tasa de ocupación:")
    print(desc_tasa_ocup)
    print("Resumen por mes:")
    print(desc_por_mes)

    # Histograma + densidad: integrantes
    cat("Generando gráficos...\n")
    tryCatch({
        p1 <- ggplot(base_total, aes(x = integrantes)) +
        geom_histogram(bins = 30) +
        geom_density(linewidth = 1) +
        labs(title = "Distribución de integrantes por hogar", x = "Integrantes", y = "Frecuencia")
        print(p1)
    }, error = function(e) {
        warning(paste("Error al generar gráfico de integrantes:", e$message))
    })

    # Histograma + densidad: edad promedio
    tryCatch({
        p2 <- ggplot(base_total, aes(x = edad)) +
        geom_histogram(bins = 30) +
        geom_density(linewidth = 1) +
        labs(title = "Distribución de edad promedio por hogar", x = "Edad (años)", y = "Frecuencia")
        print(p2)
    }, error = function(e) {
        warning(paste("Error al generar gráfico de edad:", e$message))
    })

    # Histograma + densidad: tasa de ocupación
    tryCatch({
        p3 <- ggplot(base_total, aes(x = tasa_ocupacion)) +
        geom_histogram(bins = 30) +
        geom_density(linewidth = 1) +
        labs(title = "Distribución de la tasa de ocupación por hogar", x = "Tasa (0–1)", y = "Frecuencia")
        print(p3)
    }, error = function(e) {
        warning(paste("Error al generar gráfico de tasa de ocupación:", e$message))
    })

    # Boxplots por mes
    tryCatch({
        p4 <- ggplot(base_total, aes(x = mes, y = integrantes)) +
        geom_boxplot() +
        labs(title = "Integrantes por hogar, por mes", x = "Mes", y = "Integrantes")
        print(p4)
    }, error = function(e) {
        warning(paste("Error al generar boxplot de integrantes:", e$message))
    })

    tryCatch({
        p5 <- ggplot(base_total, aes(x = mes, y = edad)) +
        geom_boxplot() +
        labs(title = "Edad promedio del hogar, por mes", x = "Mes", y = "Edad (años)")
        print(p5)
    }, error = function(e) {
        warning(paste("Error al generar boxplot de edad:", e$message))
    })

    tryCatch({
        p6 <- ggplot(base_total, aes(x = mes, y = tasa_ocupacion)) +
        geom_boxplot() +
        labs(title = "Tasa de ocupación del hogar, por mes", x = "Mes", y = "Tasa")
        print(p6)
    }, error = function(e) {
        warning(paste("Error al generar boxplot de tasa de ocupación:", e$message))
    })

    # Barras: variable cualitativa migrante (global y por mes)
    tryCatch({
        p7 <- ggplot(base_total, aes(x = migrante)) +
        geom_bar() +
        labs(title = "Hogares con condición 'migrante'", x = "Categoría", y = "Conteo")
        print(p7)
    }, error = function(e) {
        warning(paste("Error al generar gráfico de barras de migrante:", e$message))
    })

    tryCatch({
        p8 <- ggplot(base_total, aes(x = mes, fill = migrante)) +
        geom_bar(position = "fill") +
        scale_y_continuous(labels = scales::percent) +
        labs(title = "Proporción de hogares migrantes por mes", x = "Mes", y = "Proporción")
        print(p8)
    }, error = function(e) {
        warning(paste("Error al generar gráfico de proporción de migrantes:", e$message))
    })
    
    # --- Analogía (media muestral)
    cat("Calculando estimadores por analogía...\n")
    media_integrantes <- mean(base_total$integrantes, na.rm = TRUE)
    media_edad        <- mean(base_total$edad, na.rm = TRUE)
    media_tasa        <- mean(base_total$tasa_ocupacion, na.rm = TRUE)
    cat("Estimadores por analogía calculados exitosamente.\n")

# --- MLE asumiendo Normal (para media y sigma)
x1 <- base_total$integrantes[is.finite(base_total$integrantes)]
x2 <- base_total$edad[is.finite(base_total$edad)]
x3 <- base_total$tasa_ocupacion[is.finite(base_total$tasa_ocupacion)]

# Verificar que tenemos datos suficientes
if (length(x1) < 2) stop("No hay suficientes datos para integrantes")
if (length(x2) < 2) stop("No hay suficientes datos para edad")
if (length(x3) < 2) stop("No hay suficientes datos para tasa de ocupación")

fit_int <- MASS::fitdistr(x1, "normal")
fit_edad<- MASS::fitdistr(x2, "normal")
fit_tasa<- MASS::fitdistr(x3, "normal")  # nota: tasa en [0,1], Normal es aproximación

mle_int_mu <- fit_int$estimate["mean"]; mle_int_sd <- fit_int$estimate["sd"]
mle_edad_mu<- fit_edad$estimate["mean"]; mle_edad_sd<- fit_edad$estimate["sd"]
mle_tasa_mu<- fit_tasa$estimate["mean"]; mle_tasa_sd<- fit_tasa$estimate["sd"]

# --- Intervalos de confianza (t)
ci_int  <- t.test(base_total$integrantes)$conf.int
ci_edad <- t.test(base_total$edad)$conf.int
ci_tasa <- t.test(base_total$tasa_ocupacion)$conf.int

list(
  analogia = c(integrantes=media_integrantes, edad=media_edad, tasa=media_tasa),
  mle_mu   = c(integrantes=mle_int_mu,      edad=mle_edad_mu,  tasa=mle_tasa_mu),
  ci_t     = list(integrantes=ci_int, edad=ci_edad, tasa=ci_tasa)
)

tab_mig <- table(base_total$migrante, useNA = "no")
n_tot   <- sum(tab_mig)
k_si    <- as.integer(tab_mig["Si"])
p_hat   <- k_si / n_tot   # estimador por analogía y MLE binomial

# IC exacto (Clopper–Pearson)
ci_prop_exact <- binom.test(k_si, n_tot)$conf.int

# IC aproximado (prop.test)
ci_prop_wald  <- prop.test(k_si, n_tot)$conf.int

list(
  n = n_tot, k_si = k_si, p_hat = p_hat,
  ci_exact = ci_prop_exact,
  ci_wald  = ci_prop_wald
)

set.seed(123)

boot_mean <- function(d, i) mean(d[i], na.rm = TRUE)

# Bootstrap para media de integrantes, edad y tasa
tryCatch({
    b_int  <- boot(x1, boot_mean, R = 2000)
    b_edad <- boot(x2, boot_mean, R = 2000)
    b_tasa <- boot(x3, boot_mean, R = 2000)
}, error = function(e) {
    stop(paste("Error en el análisis bootstrap:", e$message))
})

sesgo_int  <- mean(b_int$t)  - mean(x1, na.rm=TRUE)
sesgo_edad <- mean(b_edad$t) - mean(x2, na.rm=TRUE)
sesgo_tasa <- mean(b_tasa$t) - mean(x3, na.rm=TRUE)

EE_int  <- sd(b_int$t)
EE_edad <- sd(b_edad$t)
EE_tasa <- sd(b_tasa$t)

# IC bootstrap percentil (robusto, útil para tasa)
tryCatch({
    ci_boot_int  <- boot.ci(b_int,  type="perc")
    ci_boot_edad <- boot.ci(b_edad, type="perc")
    ci_boot_tasa <- boot.ci(b_tasa, type="perc")
}, error = function(e) {
    warning(paste("Error al calcular intervalos de confianza bootstrap:", e$message))
    ci_boot_int <- ci_boot_edad <- ci_boot_tasa <- NULL
})

list(
  sesgos = c(integrantes = sesgo_int, edad = sesgo_edad, tasa = sesgo_tasa),
  EE     = c(integrantes = EE_int,  edad = EE_edad,  tasa = EE_tasa),
  IC_bootstrap = list(integrantes = ci_boot_int, edad = ci_boot_edad, tasa = ci_boot_tasa)
)
boot_prop <- function(df, i) {
  d <- df[i, ]
  mean(d$migrante == "Si", na.rm = TRUE)
}

tryCatch({
    b_mig <- boot(base_total, boot_prop, R = 2000)
    sesgo_prop <- mean(b_mig$t) - p_hat
    EE_prop    <- sd(b_mig$t)
    ci_boot_prop <- boot.ci(b_mig, type = "perc")
}, error = function(e) {
    warning(paste("Error en bootstrap para proporción:", e$message))
    sesgo_prop <- EE_prop <- 0
    ci_boot_prop <- NULL
})

list(
  p_hat = p_hat,
  sesgo = sesgo_prop,
  EE    = EE_prop,
  IC_bootstrap = ci_boot_prop
)

tabla_final <- tibble::tibble(
  Variable = c("Integrantes (pers)","Edad promedio (años)","Tasa de ocupación (0–1)","Proporción migrante (Si)"),
  Media    = c(mean(x1), mean(x2), mean(x3), p_hat),
  Mediana  = c(median(x1), median(x2), median(x3), NA_real_),
  Sesgo    = c(sesgo_int, sesgo_edad, sesgo_tasa, sesgo_prop)
)

# Mostrar la tabla final
print("Tabla final de resultados:")
print(tabla_final)


    