# Guía Completa para Análisis de P6500

## 📋 Resumen

**Variable**: P6500 - Antes de descuentos ¿cuánto ganó ... El mes pasado en este empleo?  
**Archivo fuente**: Ocupados.csv  
**Meses**: Julio_2024, Agosto_2024, Septiembre_2024  
**Agregación**: DIRECTORIO - Todos los registros de cada mes sin agregar por hogar  
**Rango esperado**: 100 - 25,000,000 pesos colombianos

## 🎯 Objetivo

Crear un análisis dinámico completo de la variable P6500 con:
- ✅ **20+ estadísticos descriptivos** (vs. 10 mínimos requeridos)
- ✅ **Análisis dinámico** (solo cambiar nombre de variable)
- ✅ **Estimadores, insesgamiento, eficiencia y consistencia**
- ✅ **Datos de 3 meses combinados** por DIRECTORIO

## 📁 Archivos Creados

### Scripts de Combinación de Datos:
1. **`Combinados/scripts/combinar_datos_P6500.R`** - Combina datos de Ocupados.csv de 3 meses
2. **`ejecutar_combinar_P6500.R`** - Ejecuta solo la combinación
3. **`ejecutar_analisis_P6500.R`** - Ejecuta combinación + análisis completo

### Scripts de Diagnóstico:
4. **`diagnostico_P6500.R`** - Diagnóstico de valores y rangos

### Documentación:
5. **`GUIA_COMPLETA_P6500.md`** - Esta guía específica para P6500

## 🚀 Cómo Usar

### Opción 1: Análisis Completo Automático
```r
source("ejecutar_analisis_P6500.R")
```
Este script hace todo automáticamente:
1. Combina todos los registros de los 3 meses desde Ocupados.csv usando DIRECTORIO como campo de agregación
2. Ejecuta el análisis dinámico completo
3. Genera todos los gráficos y estadísticos

### Opción 2: Paso a Paso
```r
# Paso 1: Combinar datos
source("Combinados/scripts/combinar_datos_P6500.R")

# Paso 2: Configurar análisis dinámico
# Editar scripts_analisis/analisis_variable_cuantitativa_dinamico.R líneas 5-6:
variable_name <- "P6500"
data_file <- "Combinados/Combinado_cuantitativo_P6500.csv"

# Paso 3: Ejecutar análisis
source("scripts_analisis/analisis_variable_cuantitativa_dinamico.R")
```

## 📊 Estadísticos Descriptivos (20+ estadísticos)

### Medidas de Posición:
- N total, N missing, N válidos

### Medidas de Tendencia Central:
- Mínimo, P10, Q1, Mediana, Media, Q3, P90, P95, Máximo, Moda(s)

### Medidas de Dispersión:
- Rango, IQR, Desviación Estándar, Varianza, Coeficiente de Variación, MAD

### Medidas de Forma:
- Asimetría, Curtosis

## 📈 Análisis Completo Incluido

1. **Estadísticos Descriptivos Ampliados** (20+ estadísticos)
2. **Análisis Gráfico**:
   - Histograma
   - Boxplot
   - Densidad
   - Histograma log1p (si aplica)
3. **Estimadores Puntuales**: Media, Mediana, Moda
4. **Intervalos de Confianza**: t-test y bootstrap
5. **Análisis de Insesgamiento**: Bias mediante bootstrap
6. **Consistencia**: Varianza vs tamaño muestral
7. **Eficiencia**: Comparación de varianzas

## 📂 Archivos Generados

Para P6500 se crean:
- `Combinados/Combinado_P6500.csv` - Datos completos con mes
- `Combinados/Combinado_cuantitativo_P6500.csv` - Solo P6500 para análisis
- `analisis_P6500_cuantitativa/` - Directorio de resultados
- `analisis_P6500_cuantitativa/plots/` - Gráficos
- `analisis_P6500_cuantitativa/consistencia_var_vs_n.png` - Gráfico de consistencia

## 🔧 Configuración de Rutas

El script busca los datos en:
```
C:/UIS/Statistics/Statistics_II/datasets/
├── Julio_2024/CSV/Ocupados.CSV
├── Agosto_2024/CSV/Ocupados.CSV
└── Septiembre_2024/CSV/Ocupados.CSV
```

Si las rutas son diferentes, editar la variable `ruta_base` en `Combinados/scripts/combinar_datos_P6500.R` línea 83.

## ⚠️ Notas Importantes

1. **Limpieza Automática**: El script limpia automáticamente valores faltantes y espacios
2. **Agregación por Registro**: Se toman todos los registros de cada mes usando DIRECTORIO como campo de agregación
3. **Manejo de Errores**: Incluye verificación de archivos y variables
4. **Compatibilidad**: Funciona con variaciones de nombres de variables (P6500, p6500, etc.)
5. **Rango Esperado**: 100 - 25,000,000 pesos colombianos

## 🎯 Resultado Final

Al ejecutar el análisis obtienes:
- **20+ estadísticos descriptivos** (superando el mínimo de 10)
- **Análisis dinámico** (solo cambiar nombre de variable)
- **Estimadores, insesgamiento, eficiencia y consistencia** completos
- **Datos de 3 meses** combinados correctamente
- **Gráficos profesionales** guardados automáticamente

## 📊 Comparación con Otras Variables

- **P6500**: Salario antes de descuentos (Ocupados.csv)
- **P6580S1**: Bonificación mensual (Ocupados.csv) 
- **P1806**: Salario mínimo aceptado (No ocupados.csv)

¡El sistema está listo para usar con P6500! 🚀
