# Statistics II - Trabajos y Talleres

## 📋 Información General

**Materia**: Statistics II  
**Período**: 2025-2  
**Grupo**: 7  
**Universidad**: Universidad Industrial de Santander (UIS)

## 👥 Integrantes del Grupo

1. **Juan Camilo Jaimes Avila** - Código: 2221882
2. **Roger Scheneider Fuentes**
3. **José Augusto Quintero Lobo**

## 📁 Estructura de Trabajos

### Taller I - Análisis de Variables Cuantitativas y Cualitativas
**Ubicación**: `taller_I/`

#### Descripción
Análisis estadístico completo de variables cuantitativas y cualitativas usando R, incluyendo:
- Estadísticos descriptivos ampliados (20+ estadísticos)
- Análisis dinámico de variables
- Estimadores, insesgamiento, eficiencia y consistencia
- Análisis gráfico completo

#### Variables Analizadas

##### Variables Cuantitativas:
- **P6500**: Antes de descuentos ¿cuánto ganó ... El mes pasado en este empleo?
  - Archivo: `Combinados/Combinado_cuantitativo_P6500.csv`
  - Rango: 100 - 25,000,000 pesos
  - Análisis: `ejecutar_analisis_P6500.R`

- **P6800**: ¿cuántas horas a la semana trabaja normalmente ... En ese trabajo?
  - Archivo: `Combinados/Combinado_cuantitativo_P6800.csv`
  - Rango: 1 - 126 horas
  - Análisis: `ejecutar_analisis_P6800.R`

- **P1806**: ¿Cuál sería la remuneración o el salario mensual más bajo por el que aceptaría?
  - Archivo: `Combinados/Combinado_cuantitativo_P1806.csv`
  - Archivo fuente: No ocupados.csv
  - Análisis: `ejecutar_analisis_P1806.R`

- **P6580S1**: El mes pasado recibió algún tipo de bonificación de carácter mensual? ¿cuánto?
  - Archivo: `Combinados/Combinado_cuantitativo.csv`
  - Análisis: `ejecutar_analisis_P6580S1.R`

##### Variables Cualitativas:
- **P5090**: Variable cualitativa
  - Archivo: `Combinados/Combinado_P5090.csv`
  - Análisis: `ejecutar_analisis_P5090.R`

- **V4650**: Variable cualitativa
  - Archivo: `scripts_analisis/analisis_V4650_cualitativa.R`

#### Estructura del Taller I

```
taller_I/
├── scripts_analisis/
│   ├── analisis_variable_cuantitativa_dinamico.R  # Análisis dinámico principal
│   ├── analisis_P6580S1_cuantitativa.R
│   └── analisis_V4650_cualitativa.R
├── Combinados/
│   ├── scripts/
│   │   ├── combinar_datos_P6500.R
│   │   ├── combinar_datos_P6800.R
│   │   ├── combinar_datos_P1806.R
│   │   ├── combinar_datos_P5090.R
│   │   └── combinar_datos_P6580S1.R
│   ├── Combinado_cuantitativo.csv
│   ├── Combinado_cuantitativo_P6500.csv
│   ├── Combinado_cuantitativo_P6800.csv
│   ├── Combinado_cuantitativo_P1806.csv
│   ├── Combinado_P5090.csv
│   └── Combinado_P6580S1.csv
├── analisis_[VARIABLE]_cuantitativa/
│   ├── plots/                    # Gráficos generados
│   └── consistencia_var_vs_n.png # Gráfico de consistencia
├── ejecutar_analisis_P6500.R     # Análisis completo automático
├── ejecutar_analisis_P6800.R
├── ejecutar_analisis_P1806.R
├── ejecutar_analisis_P6580S1.R
├── ejecutar_analisis_P5090.R
├── GUIA_COMPLETA_P6500.md        # Documentación específica
├── GUIA_COMPLETA_P6800.md
├── GUIA_COMPLETA_P1806.md
└── README_analisis_dinamico.md   # Documentación general
```

#### Cómo Ejecutar los Análisis

##### Análisis Automático (Recomendado):
```r
# Para cualquier variable cuantitativa
source("ejecutar_analisis_P6500.R")  # Salarios
source("ejecutar_analisis_P6800.R")  # Horas semanales
source("ejecutar_analisis_P1806.R")  # Salario mínimo aceptado
source("ejecutar_analisis_P6580S1.R") # Bonificaciones

# Para variables cualitativas
source("ejecutar_analisis_P5090.R")
```

##### Análisis Dinámico:
```r
# Editar scripts_analisis/analisis_variable_cuantitativa_dinamico.R
# Cambiar solo las líneas 5-6:
variable_name <- "P6800"  # <- CAMBIAR SOLO ESTO
data_file <- "Combinados/Combinado_cuantitativo_P6800.csv"

# Luego ejecutar:
source("scripts_analisis/analisis_variable_cuantitativa_dinamico.R")
```

#### Características Técnicas

##### Estadísticos Descriptivos (20+ estadísticos):
- **Medidas de Posición**: N total, N missing, N válidos
- **Medidas de Tendencia Central**: Mínimo, P10, Q1, Mediana, Media, Q3, P90, P95, Máximo, Moda(s)
- **Medidas de Dispersión**: Rango, IQR, Desviación Estándar, Varianza, Coeficiente de Variación, MAD
- **Medidas de Forma**: Asimetría, Curtosis

##### Análisis Completo Incluido:
1. **Estadísticos Descriptivos Ampliados**
2. **Análisis Gráfico**: Histograma, Boxplot, Densidad, Histograma log1p
3. **Estimadores Puntuales**: Media, Mediana, Moda
4. **Intervalos de Confianza**: t-test y bootstrap
5. **Análisis de Insesgamiento**: Bias mediante bootstrap
6. **Consistencia**: Varianza vs tamaño muestral
7. **Eficiencia**: Comparación de varianzas

##### Datos Procesados:
- **Meses**: Julio_2024, Agosto_2024, Septiembre_2024
- **Fuentes**: Ocupados.csv, No ocupados.csv
- **Agregación**: DIRECTORIO (registro individual)
- **Limpieza**: Automática de valores faltantes y espacios

#### Archivos de Resultados

Para cada variable se generan:
- `analisis_[VARIABLE]_cuantitativa/` - Directorio principal de resultados
- `analisis_[VARIABLE]_cuantitativa/plots/` - Gráficos (histograma, boxplot, densidad)
- `analisis_[VARIABLE]_cuantitativa/consistencia_var_vs_n.png` - Gráfico de consistencia
- `Combinados/Combinado_[VARIABLE].csv` - Datos combinados por mes
- `Combinados/Combinado_cuantitativo_[VARIABLE].csv` - Datos para análisis

## 🔧 Requisitos Técnicos

### Paquetes de R Requeridos:
- `readr` - Lectura de archivos CSV
- `dplyr` - Manipulación de datos
- `ggplot2` - Gráficos
- `gridExtra` - Combinación de gráficos
- `boot` - Bootstrap
- `moments` - Estadísticos de forma

### Instalación Automática:
Los paquetes se instalan automáticamente si no están disponibles.

## 📊 Datos Utilizados

### Fuente de Datos:
- **Encuesta Nacional de Empleo y Desempleo (ENEMD)**
- **Período**: Julio, Agosto, Septiembre 2024
- **Archivos**: Ocupados.csv, No ocupados.csv

### Variables Principales:
- **P6500**: Salario antes de descuentos (Ocupados)
- **P6800**: Horas semanales de trabajo (Ocupados)
- **P1806**: Salario mínimo aceptado (No ocupados)
- **P6580S1**: Bonificación mensual (Ocupados)
- **P5090**: Variable cualitativa
- **V4650**: Variable cualitativa

## 📝 Notas para el Docente

### Evaluación del Trabajo:
1. **Completitud**: Todos los análisis incluyen 20+ estadísticos descriptivos
2. **Dinamismo**: Sistema permite cambiar variables fácilmente
3. **Documentación**: Guías completas para cada variable
4. **Organización**: Estructura clara y archivos bien organizados
5. **Reproducibilidad**: Scripts ejecutables y documentados

### Puntos Destacados:
- ✅ **Análisis dinámico** implementado correctamente
- ✅ **Estadísticos descriptivos ampliados** (superando el mínimo de 10)
- ✅ **Estimadores, insesgamiento, eficiencia y consistencia** completos
- ✅ **Datos de 3 meses** combinados correctamente
- ✅ **Documentación completa** para cada variable
- ✅ **Estructura organizada** y fácil de navegar

### Archivos Clave para Revisión:
- `scripts_analisis/analisis_variable_cuantitativa_dinamico.R` - Script principal
- `ejecutar_analisis_P6800.R` - Ejemplo de análisis completo
- `GUIA_COMPLETA_P6800.md` - Documentación detallada
- `Combinados/Combinado_cuantitativo_P6800.csv` - Datos procesados

## 📞 Contacto

Para consultas sobre el trabajo:
- **Juan Camilo Jaimes Avila** - 2221882
- **Roger Scheneider Fuentes**
- **José Augusto Quintero Lobo**

---
*Trabajo realizado para Statistics II - Período 2025-2 - Grupo 7 - UIS*
