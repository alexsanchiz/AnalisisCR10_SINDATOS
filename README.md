# Sistema de Análisis Estadístico en máquina IS

DATOS ELIMINADOS PARA PODER PUBLICAR

## Descripción General

Sistema avanzado de análisis estadístico para control de calidad en fabricación de botellas de vidrio utilizando máquinas IS (Individual Section). El sistema procesa datos de sensores con exactamente 24 variables para analizar factores que influyen en defectos de fabricación mediante GLMM independientes por modelo con 13 interacciones extendidas y diagnósticos avanzados completos.

## Pipeline de Ejecución del Sistema

El sistema está organizado en **14 scripts numerados secuencialmente** que conforman un pipeline completo de análisis. La ejecución sigue un orden estricto para garantizar la integridad de los datos y la correcta propagación de resultados entre módulos.

### 000_EJECUTAR_TODO.R - Orquestador Principal
**Función**: Script maestro que ejecuta todo el pipeline de análisis de forma automatizada.

**Orden de Ejecución Implementado**:
1. **Inicialización** (00_): Configuración del entorno y librerías
2. **Carga de Datos** (01_, 01B_): Procesamiento de datos principales y de estirado
3. **Análisis Estadístico Base** (02_, 02B_): GLMM básicos y correlación estirado
4. **Gráficos Básicos** (03_): Visualizaciones fundamentales del análisis
5. **Comparación de Sistemas** (04_): Análisis automático vs manual
6. **Análisis Segmentado Avanzado** (05_): GLMM por modelo con interacciones extendidas
7. **Análisis Temporal Avanzado** (06_): Efectos de lags temporales en estirado
8. **Calibración de Umbrales** (07_): Optimización de umbrales por tipo de defecto
9. **Gráficos Extendidos** (08_): Visualizaciones avanzadas y resumen ejecutivo

**Control de Errores**: Manejo robusto de errores con continuación del pipeline, logging detallado de cada etapa, y generación de reportes incluso con datos parciales.

**Salidas**: Workspace completo guardado, reportes integrados por timestamp, y resumen ejecutivo final.

---

## Módulos del Sistema por Orden de Ejecución

### 00_INICIALIZAR.R - Configuración del Entorno
**Propósito**: Preparar el entorno R con todas las dependencias necesarias para el análisis estadístico avanzado.

**Funcionalidades**:
- Instalación automática de 15+ librerías especializadas (lme4, glmmTMB, mgcv, DHARMa, pROC, etc.)
- Configuración de directorios de trabajo con detección automática de rutas
- Definición de variables globales y constantes del sistema
- Configuración de parámetros para modelos GLMM y diagnósticos
- Validación de compatibilidad del entorno R

**Variables Críticas Definidas**: 24 variables esperadas organizadas en 5 grupos funcionales.

### 01_CARGAR_DATOS.R - Carga de Datos Principales
**Propósito**: Cargar y procesar los datos principales de la máquina IS con validación exhaustiva de estructura.

**Funcionalidades**:
- Carga inteligente con detección automática de encoding y separadores
- Mapeo automático de variables (N→FH, conversión de nombres)
- Validación de las 24 variables obligatorias con reporte de faltantes
- Procesamiento de variables defecto binarias (1=detectado, 0=no detectado)
- Conversión automática de tipos de datos y formatos temporales
- Estadísticas descriptivas inmediatas y validación de integridad

**Salida**: Dataset principal validado y estandarizado listo para análisis.

### 01B_CARGAR_DATOS_ESTIRADO.R - Carga de Datos de Estirado
**Propósito**: Cargar y procesar datos del sistema de estirado para análisis comparativo.

**Funcionalidades**:
- Carga específica de archivos ESTIRADOS.csv con estructura FH;RECHAZO;MODELO
- Conversión de porcentajes de rechazo a formato numérico estándar
- Sincronización temporal con datos principales (correlación hora a hora)
- Validación de modelos y coherencia temporal
- Preparación para análisis de correlación cruzada

**Integración**: Datos preparados para correlación con defectos en módulos posteriores.

### 02_EJECUTAR_ANALISIS.R - Análisis Estadístico Base
**Propósito**: Realizar el análisis estadístico fundamental con GLMM, PCA y diagnósticos básicos.

**Funcionalidades**:
- **Análisis Exploratorio**: Estadísticas descriptivas por sección y modelo
- **Análisis de Colinealidad**: VIF para detectar multicolinealidad entre predictores
- **GLMM Base**: Modelos mixtos con efectos aleatorios por sección para cada defecto
- **Diagnósticos de Modelos**: ROC, AUC, matrices de confusión, residuos de Pearson
- **DHARMa**: Diagnósticos avanzados para validación de modelos binomiales
- **Efectos Marginales**: Análisis de probabilidades predichas
- **PCA Avanzado**: Análisis de componentes principales con biplots
- **Correlaciones**: Matrices Pearson y Spearman con tests de significancia
- **Validación Cruzada**: 5-fold CV con métricas especializadas para datos desbalanceados

**Salida**: Modelos GLMM ajustados, métricas de desempeño, y diagnósticos completos.

### 02B_ANALISIS_CORRELACION_ESTIRADO.R - Correlación Estirado-Defectos
**Propósito**: Analizar la correlación entre datos de estirado y aparición de defectos.

**Funcionalidades**:
- Integración temporal de datos de estirado con defectos (sincronización horaria)
- Análisis de correlación cruzada con diferentes horizontes temporales
- Detección de efectos retardados del proceso de estirado
- Comparación estadística entre sistema automático (8h) vs manual (1h)
- Análisis de significancia de correlaciones encontradas

**Reconocimiento de Limitaciones**: Temporal mismatch documentado entre frecuencias de muestreo.

### 03_CREAR_GRAFICOS.R - Visualizaciones Básicas
**Propósito**: Generar el conjunto básico de visualizaciones del análisis estadístico.

**Funcionalidades**:
- **7 Gráficos Especializados** en PDF multipágina:
  1. Distribución de defectos por sección y modelo
  2. Curvas ROC con AUC para cada tipo de defecto
  3. Matrices de confusión optimizadas
  4. Efectos marginales de variables significativas
  5. PCA: Biplots y scree plots
  6. Correlaciones: Matrices con códigos de significancia
  7. Validación cruzada: Boxplots de métricas de robustez

**Calidad**: Gráficos profesionales con escalas optimizadas y leyendas explicativas.

### 04_GRAFICOS_COMPARACION_SISTEMAS.R - Comparación Automático vs Manual
**Propósito**: Visualizar la comparación entre el sistema automático de estirado y el sistema manual de defectos.

**Funcionalidades**:
- Gráficos de correlación temporal entre sistemas
- Análisis de tendencias comparativas por turno
- Visualización de lags temporales detectados
- Representación de limitaciones conocidas del sistema
- Recomendaciones visuales para mejora de sincronización

**Enfoque**: Reconoce y documenta las limitaciones inherentes a la diferencia de frecuencias.

### 05_ANALISIS_SEGMENTADO_AVANZADO.R - GLMM por Modelo con Interacciones Extendidas
**Propósito**: Implementar GLMM independientes por modelo con las 13 interacciones extendidas solicitadas y diagnósticos avanzados completos.

**Funcionalidades Principales**:
- **GLMM Independientes por Modelo**: Un modelo separado para cada tipo de botella
- **Random Intercepts**: Efectos aleatorios para SECCION y CAVIDAD por modelo
- **13 Interacciones Extendidas Implementadas**:
  - Q_S_CAZOS × (LONGITUD, XY, VERTICALIDAD, ASIMETRIA, OVALIDAD) - 5 interacciones
  - T_CAZOS × (LONGITUD, VERTICALIDAD, ASIMETRIA, OVALIDAD) - 4 interacciones  
  - T_S_TIJERAS × (VERTICALIDAD, LONGITUD, ASIMETRIA, OVALIDAD) - 4 interacciones
- **Splines mgcv**: s(Q_S_CAZOS,k=5), s(LONGITUD,k=5), s(T_S_TIJERAS,k=4) para efectos no lineales
- **Test de No Linealidad**: Comparación GAM vs lineal con test Chi-cuadrado
- **Diagnósticos Avanzados Completos**: DHARMa con bootstrap (n=100), leverage, Cook's distance
- **Métricas por Modelo y Defecto**: AUC, Accuracy, Precision, Recall, F1-Score independientes
- **Optimización**: bobyqa con 25,000 iteraciones máximas para convergencia robusta

**Ventajas del Enfoque por Modelo**:
- Efectos específicos por tipo de botella detectados
- Interacciones modelo-dependientes identificadas
- Calibración optimizada para cada modelo
- Mayor sensibilidad a efectos sutiles

### 06_ANALISIS_ESTIRADO_AVANZADO.R - Efectos Temporales con Lags
**Propósito**: Analizar efectos retardados del proceso de estirado con diferentes horizontes temporales.

**Funcionalidades**:
- **Análisis de Lags**: Efectos retardados de 1h, 2h, 4h, 8h del estirado
- **Detección de No Linealidades**: Splines para relaciones complejas temporales
- **Correlación Dinámica**: Análisis de correlación variable en el tiempo
- **Identificación de Ventanas Críticas**: Períodos de mayor influencia del estirado
- **Modelos Temporales**: GLMM con estructura temporal de correlación

**Aplicación**: Optimización de control de proceso con predicción temporal.

### 07_CALIBRACION_UMBRALES.R - Optimización de Umbrales por Defecto
**Propósito**: Calibrar umbrales óptimos específicos para cada tipo de defecto según diferentes criterios.

**Funcionalidades**:
- **Múltiples Criterios de Optimización**:
  - Youden Index: Maximiza sensibilidad + especificidad
  - Precision-optimized: Minimiza falsos positivos
  - F1-optimized: Balance precision-recall
  - Recall-constrained: Garantiza detección mínima
- **Curvas de Reliability**: Análisis Hosmer-Lemeshow, Brier Score
- **Calibración por Caso de Uso**: Umbrales específicos según aplicación
- **Análisis Costo-Beneficio**: Recomendaciones basadas en implicaciones operativas

**Salida**: Umbrales optimizados por defecto con justificación estadística.

### 08_GRAFICOS_AVANZADOS_EXTENDIDOS.R - Visualizaciones Especializadas
**Propósito**: Generar visualizaciones especializadas para GLMM por modelo, interacciones y diagnósticos avanzados.

**Funcionalidades**:
- **Gráficos Especializados Adicionales**:
  - Métricas GLMM por modelo (distribución AUC, accuracy por modelo)
  - Efectos no lineales y diagnósticos (términos de suavizado significativos)
  - Interacciones por modelo (frecuencia, significancia vs magnitud)
  - Distribución de coeficientes de interacción
  - Precision vs Recall por modelo
- **Resumen Ejecutivo Visual**: Página integrada con hallazgos principales
- **Diagnósticos Visuales**: Leverage, Cook's distance, residuos DHARMa
- **Comparación de Modelos**: AUC GAM vs lineales por defecto

**Integración**: Complementa visualizaciones básicas con análisis específicos del nuevo enfoque por modelo.

---

## Estructura de Datos y Arquitectura

### Formato de Entrada Requerido
**24 Variables Obligatorias** organizadas en 5 grupos funcionales:

1. **Variables Estructurales (4)**: FH, MODELO, SECCION, CAVIDAD
2. **Variables de Defecto (4)**: CLARO, PLIEGUEPEQ, PLIEGUEGR, DEFECTOS  
3. **Variables de Proceso (6)**: Q_S_CAZOS, Q_S_TIJERAS, Q_CAZO, T_CAZOS, T_S_TIJERAS, T_S_CAZOS
4. **Variables Geométricas (6)**: XPOS, YPOS, XY, LONGITUD, DIAMETRO, VELOCIDAD
5. **Variables de Calidad (4)**: OVALIDAD, ASIMETRIA, VERTICALIDAD, GROSOR

### Archivos de Salida por Módulo

**Reportes Estadísticos**:
- `analisis_avanzado_[archivo]_[timestamp].txt`: Resultados GLMM completos por modelo
- `analisis_segmentado_[archivo]_[timestamp].txt`: Interacciones y diagnósticos por modelo  
- `correlacion_estirado_[timestamp].txt`: Análisis temporal de correlaciones
- `resumen_ejecutivo_[timestamp].txt`: Hallazgos principales integrados

**Visualizaciones**:
- `graficos_avanzados_[archivo]_[timestamp].pdf`: 7 gráficos básicos especializados
- `graficos_extendidos_[archivo]_[timestamp].pdf`: Visualizaciones GLMM por modelo
- `comparacion_sistemas_[timestamp].pdf`: Análisis automático vs manual

**Gráficos Individuales**:
- `graficos_png/PNG_[archivo]_[timestamp]/`: Directorio con gráficos PNG individuales
- `graficos_latex/LATEX_[archivo]_[timestamp]/`: Directorio con figuras LaTeX optimizadas
- `referencias_latex.tex`: Plantilla de código LaTeX para PNG (en carpeta PNG)
- `figuras_maestro.tex`: Código LaTeX completo para figuras PDF (en carpeta LaTeX)

## Ventajas del Enfoque por Modelo

### Precisión Localizada
- Efectos específicos por tipo de modelo de botella
- Interacciones que varían entre modelos detectadas  
- Calibración optimizada para cada modelo
- Reducción de confusión entre modelos diferentes

### Robustez Técnica
- Modelos independientes = mayor robustez ante fallas
- Parámetros optimizados específicamente por modelo
- Validación cruzada independiente posible
- Diagnósticos especializados por tipo de botella

### Detección Mejorada
- Interacciones modelo-específicas identificadas
- Efectos locales vs globales separados
- Patrones ocultos en datos agregados revelados
- Mayor sensibilidad a efectos sutiles

## Casos de Uso Específicos

### Optimización por Modelo
- Identificar modelos con mayor variabilidad entre secciones
- Detectar interacciones críticas específicas por modelo  
- Optimizar parámetros de proceso por tipo de botella
- Calibrar umbrales de alerta por modelo

### Control de Proceso Avanzado
- Ajustes de caudal específicos por modelo y longitud
- Control de temperatura personalizado por tipo de botella
- Monitoreo de interacciones críticas en tiempo real
- Alertas tempranas basadas en modelos predictivos

## Validación del Sistema

**Datos Reales**: Probado con datasets de 25,000+ observaciones de producción real
**Robustez**: Análisis de componentes principales explica 82-83% de varianza
**Convergencia**: GLMM con optimización bobyqa para casos complejos
**Diagnósticos**: Validación DHARMa con bootstrap para modelos binomiales raros

## Requisitos Técnicos

### Entorno R
- **R versión 4.0 o superior**
- **Paquetes especializados**: lme4, glmmTMB, mgcv, DHARMa, pROC, caret, car, corrplot
- **Memoria**: Mínimo 8GB RAM recomendado para datasets grandes
- **Almacenamiento**: 500MB para resultados completos

### Estructura de Archivos
```
proyecto/
├── 000_EJECUTAR_TODO.R          # Orquestador principal
├── 00_INICIALIZAR.R             # Configuración entorno
├── 01_CARGAR_DATOS.R            # Carga datos principales  
├── 01B_CARGAR_DATOS_ESTIRADO.R  # Carga datos estirado
├── 02_EJECUTAR_ANALISIS.R       # Análisis estadístico base
├── 02B_ANALISIS_CORRELACION_ESTIRADO.R  # Correlación estirado
├── 03_CREAR_GRAFICOS.R          # Visualizaciones básicas
├── 04_GRAFICOS_COMPARACION_SISTEMAS.R   # Comparación sistemas
├── 05_ANALISIS_SEGMENTADO_AVANZADO.R    # GLMM por modelo
├── 06_ANALISIS_ESTIRADO_AVANZADO.R      # Análisis temporal
├── 07_CALIBRACION_UMBRALES.R    # Optimización umbrales
├── 08_GRAFICOS_AVANZADOS_EXTENDIDOS.R   # Visualizaciones avanzadas
├── 09_GRAFICOS_INDIVIDUALES_PNG.R      # Gráficos PNG individuales
├── 10_EXTRACTOR_GRAFICOS_LATEX.R       # Figuras LaTeX optimizadas
├── datos/                       # Archivos CSV entrada
├── resultados/                  # Reportes estadísticos
└── graficos/                    # Visualizaciones (PDF, PNG, LaTeX)
```

## Ejecución del Sistema

### Comando Principal
```r
source('000_EJECUTAR_TODO.R')
```

### Parámetros de Configuración
- **Archivos de entrada**: Detección automática en directorio `datos/` o rutas relativas
- **Validación cruzada**: 5-fold por defecto, configurable según casos positivos
- **Umbrales**: Múltiples criterios de optimización disponibles
- **Gráficos**: Generación automática en alta resolución

### Monitoreo de Ejecución
- **Logging detallado**: Progreso por módulo con timestamps
- **Manejo de errores**: Continuación del pipeline ante fallas parciales
- **Validación de resultados**: Verificación automática de integridad de salidas

**Versión**: 2025-08-11 - GLMM por Modelo con Interacciones Extendidas Completas  
**Estado**: Sistema completamente operativo con pipeline de 12 módulos secuenciales
