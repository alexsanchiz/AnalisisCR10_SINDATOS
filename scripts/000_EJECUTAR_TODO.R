# ============================================================================
# SCRIPT PRINCIPAL: EJECUTAR ANALISIS COMPLETO
# Sistema de Analisis Estadistico - Control de Calidad IS
# ============================================================================

cat("SISTEMA DE ANALISIS ESTADISTICO - CONTROL DE CALIDAD IS\n")
cat("=======================================================\n")
cat("Iniciando analisis completo automatizado...\n\n")

# ============================================================================
# PASO 1: CARGAR TODOS LOS MODULOS
# ============================================================================

cat("PASO 1: CARGANDO MODULOS DEL SISTEMA\n")
cat("====================================\n")

# Cargar modulos principales en orden
cat("Cargando modulo de inicializacion...\n")
source("00_INICIALIZAR.R")

cat("Cargando modulo de carga de datos...\n")
source("01_CARGAR_DATOS.R")

cat("Cargando modulo de analisis...\n")
source("02_EJECUTAR_ANALISIS.R")

cat("Cargando modulo de graficos...\n")
source("03_CREAR_GRAFICOS.R")

# Cargar modulos de estirado si existen
if(file.exists("01B_CARGAR_DATOS_ESTIRADO.R")) {
  cat("Cargando modulo de datos de estirado...\n")
  source("01B_CARGAR_DATOS_ESTIRADO.R")
}

if(file.exists("02B_ANALISIS_CORRELACION_ESTIRADO.R")) {
  cat("Cargando modulo de correlacion estirado...\n")
  source("02B_ANALISIS_CORRELACION_ESTIRADO.R")
}

if(file.exists("04_GRAFICOS_COMPARACION_SISTEMAS.R")) {
  cat("Cargando modulo de graficos comparacion sistemas...\n")
  source("04_GRAFICOS_COMPARACION_SISTEMAS.R")
}

# Cargar modulos avanzados extendidos
if(file.exists("05_ANALISIS_SEGMENTADO_AVANZADO.R")) {
  cat("Cargando modulo de analisis segmentado avanzado...\n")
  source("05_ANALISIS_SEGMENTADO_AVANZADO.R")
}

if(file.exists("06_ANALISIS_ESTIRADO_AVANZADO.R")) {
  cat("Cargando modulo de analisis estirado avanzado...\n")
  source("06_ANALISIS_ESTIRADO_AVANZADO.R")
}

if(file.exists("07_CALIBRACION_UMBRALES.R")) {
  cat("Cargando modulo de calibracion de umbrales...\n")
  source("07_CALIBRACION_UMBRALES.R")
}

if(file.exists("08_GRAFICOS_AVANZADOS_EXTENDIDOS.R")) {
  cat("Cargando modulo de graficos avanzados extendidos...\n")
  source("08_GRAFICOS_AVANZADOS_EXTENDIDOS.R")
}

# Cargar modulos PNG y LaTeX
if(file.exists("09_GRAFICOS_INDIVIDUALES_PNG.R")) {
  cat("Cargando modulo de graficos individuales PNG...\n")
  source("09_GRAFICOS_INDIVIDUALES_PNG.R")
}

if(file.exists("10_EXTRACTOR_GRAFICOS_LATEX.R")) {
  cat("Cargando modulo extractor de graficos LaTeX...\n")
  source("10_EXTRACTOR_GRAFICOS_LATEX.R")
}

cat("Todos los modulos cargados exitosamente\n")

# ============================================================================
# IDENTIFICAR ARCHIVOS CSV DISPONIBLES
# ============================================================================

cat("\nIDENTIFICANDO ARCHIVOS CSV\n")
cat("==========================\n")

# Buscar archivos CSV en directorio datos
archivos_csv <- NULL
if(dir.exists(DIRECTORIO_DATOS)) {
  archivos_disponibles <- list.files(DIRECTORIO_DATOS, pattern = "\\.csv$", ignore.case = TRUE)
  archivos_csv <- archivos_disponibles[!grepl("^ESTIRADOS\\.csv$", archivos_disponibles, ignore.case = TRUE)]
  cat("Archivos CSV encontrados en datos/:", length(archivos_disponibles), "\n")
  cat("Archivos principales:", length(archivos_csv), "\n")
  for(archivo in archivos_csv) {
    cat("  ", archivo, "\n")
  }
} else {
  cat("Directorio datos/ no encontrado\n")
}

# Si no hay archivos en datos/, buscar en directorio actual
if(length(archivos_csv) == 0) {
  cat("Buscando archivos CSV en directorio actual...\n")
  archivos_disponibles <- list.files(".", pattern = "\\.csv$", ignore.case = TRUE)
  archivos_csv <- archivos_disponibles[!grepl("^ESTIRADOS\\.csv$", archivos_disponibles, ignore.case = TRUE)]
  cat("Archivos CSV encontrados:", length(archivos_csv), "\n")
  for(archivo in archivos_csv) {
    cat("  ", archivo, "\n")
  }
}

if(length(archivos_csv) == 0) {
  cat("ERROR: No se encontraron archivos CSV para procesar\n")
  cat("Coloca los archivos CSV en D:/CR10/datos/ o en el directorio actual\n")
  stop("No hay archivos para procesar")
}

archivos_procesar <- archivos_csv

# ============================================================================
# VERIFICACION DE DATOS DE ESTIRADO
# ============================================================================

cat("\nVERIFICANDO DATOS DE ESTIRADO\n")
cat("===============================\n")

datos_estirado_disponible <- NULL

# La detección del archivo de estirado se hará dinámicamente para cada archivo principal procesado
cat("Detección de archivos de estirado se realizará dinámicamente por archivo\n")

# ============================================================================
# PASO 2: PROCESAMIENTO DE CADA ARCHIVO
# ============================================================================

cat("\nPASO 2: PROCESAMIENTO DE ARCHIVOS\n")
cat("=================================\n")

# Inicializar contenedores de resultados
resultados_analisis <- list()
resultados_graficos <- list()

# Procesar cada archivo CSV
for(archivo_actual in archivos_procesar) {
  cat("\nProcesando archivo:", archivo_actual, "\n")
  cat("", rep("=", nchar(archivo_actual) + 19), "\n")
  
  tryCatch({
    
    # Ejecutar analisis estadistico completo
    cat("Ejecutando analisis estadistico...\n")
    resultado_analisis <- ejecutar_analisis_completo_local(archivo_actual)
    resultados_analisis[[archivo_actual]] <- resultado_analisis
    
    # ==========================================
    # EJECUTAR ANALISIS SEGMENTADO AVANZADO
    # ==========================================
    
    resultado_segmentado <- NULL
    if(!is.null(resultado_analisis$datos) && exists("ejecutar_analisis_segmentado_avanzado")) {
      cat("Ejecutando analisis segmentado avanzado...\n")
      tryCatch({
        resultado_segmentado <- ejecutar_analisis_segmentado_avanzado(resultado_analisis$datos, archivo_actual)
        resultado_analisis$segmentado <- resultado_segmentado
        cat("Analisis segmentado completado\n")
      }, error = function(e) {
        cat("Error en analisis segmentado:", e$message, "\n")
      })
    }
    
    # ==========================================
    # EJECUTAR CALIBRACION DE UMBRALES  
    # ==========================================
    
    resultado_calibracion <- NULL
    if(!is.null(resultado_analisis$roc) && length(resultado_analisis$roc) > 0 && exists("ejecutar_calibracion_umbrales")) {
      cat("Ejecutando calibracion de umbrales...\n")
      tryCatch({
        resultado_calibracion <- ejecutar_calibracion_umbrales(resultado_analisis, resultado_analisis$datos, archivo_actual)
        resultado_analisis$calibracion <- resultado_calibracion
        cat("Calibracion de umbrales completada\n")
      }, error = function(e) {
        cat("Error en calibracion:", e$message, "\n")
      })
    } else {
      cat("Modelos ROC no disponibles - calibracion omitida\n")
    }
    
    # Generar graficos avanzados
    cat("Generando graficos avanzados...\n")
    if(!is.null(resultado_analisis) && !is.null(resultado_analisis$datos)) {
      tryCatch({
        resultado_graficos <- generar_graficos_completos_local(
          resultado_analisis$datos,
          resultado_analisis,
          resultado_analisis$correlaciones
        )
        resultados_graficos[[archivo_actual]] <- resultado_graficos
        cat("Graficos avanzados generados exitosamente\n")
      }, error = function(e) {
        cat("Error generando graficos:", e$message, "\n")
      })
    } else {
      cat("Datos no disponibles para generar graficos\n")
    }
    
    # Esta sección se movió después de cargar datos de estirado
    # para asegurar que datos_estirado_actual esté disponible
    
    # ==========================================
    # CARGAR DATOS DE ESTIRADO DINAMICAMENTE
    # ==========================================
    
    datos_estirado_actual <- NULL
    if(exists("detectar_archivo_estirado") && exists("cargar_datos_estirado_local")) {
      tryCatch({
        archivo_estirado_dinamico <- detectar_archivo_estirado(archivo_actual)
        
        # Buscar archivo en múltiples ubicaciones
        rutas_buscar <- c(
          archivo_estirado_dinamico,
          file.path("datos", archivo_estirado_dinamico),
          file.path(DIRECTORIO_DATOS, archivo_estirado_dinamico)
        )
        
        archivo_encontrado <- NULL
        for(ruta in rutas_buscar) {
          if(file.exists(ruta)) {
            archivo_encontrado <- ruta
            break
          }
        }
        
        if(!is.null(archivo_encontrado)) {
          datos_estirado_actual <- cargar_datos_estirado_local(basename(archivo_encontrado))
          cat("✓ Datos de estirado cargados:", basename(archivo_encontrado), "\n")
        } else {
          cat("⚠ Archivo de estirado no encontrado:", archivo_estirado_dinamico, "\n")
        }
      }, error = function(e) {
        cat("Error cargando datos estirado dinámicos:", e$message, "\n")
      })
    }
    
    # ==========================================
    # EJECUTAR ANALISIS ESTIRADO AVANZADO CON LAGS
    # ==========================================
    
    resultado_estirado_avanzado <- NULL
    if(!is.null(datos_estirado_actual) && !is.null(resultado_analisis$datos) && exists("ejecutar_analisis_estirado_avanzado")) {
      cat("Ejecutando analisis estirado avanzado con lags...\n")
      tryCatch({
        resultado_estirado_avanzado <- ejecutar_analisis_estirado_avanzado(
          resultado_analisis$datos, 
          datos_estirado_actual, 
          archivo_actual
        )
        resultado_analisis$estirado_avanzado <- resultado_estirado_avanzado
        cat("Analisis estirado avanzado completado\n")
      }, error = function(e) {
        cat("Error en analisis estirado avanzado:", e$message, "\n")
      })
    }
    
    # Analisis de correlacion estirado-pliegues si esta disponible
    if(!is.null(datos_estirado_actual) && exists("correlacionar_estirado_pliegues")) {
      cat("Ejecutando analisis de correlacion estirado-pliegues...\n")
      
      tryCatch({
        resultado_correlacion <- correlacionar_estirado_pliegues(
          resultado_analisis$datos, 
          datos_estirado_actual
        )
        
        if(!is.null(resultado_correlacion)) {
          # Guardar resultados de correlacion
          timestamp <- crear_timestamp()
          archivo_correlacion <- file.path(DIRECTORIO_RESULTADOS, 
                                         paste0("correlacion_estirado_", timestamp, ".txt"))
          
          # Crear resumen de correlaciones
          sink(archivo_correlacion)
          cat("ANALISIS DE CORRELACION ESTIRADO-PLIEGUES\n")
          cat("==========================================\n")
          cat("Archivo principal:", archivo_actual, "\n")
          cat("Archivo estirado: ESTIRADOS.csv\n")
          cat("Fecha analisis:", Sys.time(), "\n\n")
          
          cat("CORRELACIONES PRINCIPALES:\n")
          cat("==========================\n")
          if(!is.null(resultado_correlacion$correlacion_plieguepeq)) {
            cat("Estirado vs Pliegue Pequeño:", round(resultado_correlacion$correlacion_plieguepeq, 3), "\n")
          }
          if(!is.null(resultado_correlacion$correlacion_plieguegr)) {
            cat("Estirado vs Pliegue Grande:", round(resultado_correlacion$correlacion_plieguegr, 3), "\n")
          }
          if(!is.null(resultado_correlacion$correlacion_defectos)) {
            cat("Estirado vs Defectos Totales:", round(resultado_correlacion$correlacion_defectos, 3), "\n")
          }
          cat("Observaciones analizadas:", resultado_correlacion$n_observaciones, "\n")
          sink()
          
          cat("Correlaciones estirado-pliegues guardadas:", archivo_correlacion, "\n")
          resultado_analisis$correlacion_estirado <- resultado_correlacion
          
          # Generar graficos especializados de comparacion sistemas
          if(exists("generar_graficos_comparacion_sistemas")) {
            cat("Generando graficos de comparacion sistemas...\n")
            tryCatch({
              graficos_comparacion <- generar_graficos_comparacion_sistemas(
                resultado_correlacion,
                resultado_analisis$datos
              )
              cat("Graficos de comparacion completados\n")
            }, error = function(e) {
              cat("Error en graficos de comparacion:", e$message, "\n")
            })
          }
        }
      }, error = function(e) {
        cat("Error en correlacion estirado-pliegues:", e$message, "\n")
      })
    }
    
    # ==========================================
    # GENERAR GRAFICOS AVANZADOS EXTENDIDOS
    # ==========================================
    
    if(exists("generar_graficos_avanzados_extendidos")) {
      cat("Generando graficos avanzados extendidos...\n")
      tryCatch({
        graficos_extendidos <- generar_graficos_avanzados_extendidos(
          resultado_analisis,
          resultado_segmentado,
          resultado_estirado_avanzado,
          resultado_calibracion,
          resultado_analisis$datos,
          archivo_actual
        )
        cat("Graficos avanzados extendidos completados\n")
      }, error = function(e) {
        cat("Error generando graficos extendidos:", e$message, "\n")
      })
    }
    
    # ==========================================
    # GENERAR GRAFICOS PNG INDIVIDUALES
    # ==========================================
    
    cat("Cargando modulo de graficos PNG individuales...\n")
    tryCatch({
      source("09_GRAFICOS_INDIVIDUALES_PNG.R")
      
      if(exists("generar_graficos_png_individuales")) {
        cat("Generando graficos PNG individuales...\n")
        resultado_png <- generar_graficos_png_individuales(
          datos = resultado_analisis$datos,
          resultado_analisis = resultado_analisis,
          correlaciones = correlaciones,
          resultado_segmentado = resultado_segmentado,
          nombre_archivo = archivo_actual
        )
        
        if(!is.null(resultado_png)) {
          cat("✓ Graficos PNG individuales completados:", resultado_png$total_graficos, "archivos\n")
          resultado_analisis$png_individuales <- resultado_png
        }
      }
    }, error = function(e) {
      cat("Error en graficos PNG individuales:", e$message, "\n")
    })
    
    # ==========================================
    # EXTRACCION PARA LATEX
    # ==========================================
    
    cat("Cargando modulo de extraccion para LaTeX...\n")
    tryCatch({
      source("10_EXTRACTOR_GRAFICOS_LATEX.R")
      
      if(exists("extraer_graficos_para_latex")) {
        cat("Extrayendo graficos para LaTeX...\n")
        resultado_latex <- extraer_graficos_para_latex(
          datos = resultado_analisis$datos,
          resultado_analisis = resultado_analisis,
          correlaciones = correlaciones,
          resultado_segmentado = resultado_segmentado,
          nombre_archivo = archivo_actual
        )
        
        if(!is.null(resultado_latex)) {
          cat("✓ Graficos LaTeX extraidos:", resultado_latex$total_figuras, "figuras\n")
          resultado_analisis$latex_figuras <- resultado_latex
        }
      }
    }, error = function(e) {
      cat("Error en extraccion LaTeX:", e$message, "\n")
    })
    
    cat("Archivo procesado exitosamente\n")
    
  }, error = function(e) {
    cat("Error procesando", archivo_actual, ":", e$message, "\n")
  })
}

# ============================================================================
# PASO 3: RESUMEN EJECUTIVO
# ============================================================================

cat("\nPASO 3: GENERANDO RESUMEN EJECUTIVO\n")
cat("==================================\n")

# Crear resumen ejecutivo consolidado
timestamp <- crear_timestamp()
archivo_resumen <- file.path(DIRECTORIO_RESULTADOS, paste0("resumen_ejecutivo_", timestamp, ".txt"))

sink(archivo_resumen)
cat("RESUMEN EJECUTIVO - ANALISIS COMPLETO\n")
cat("=====================================\n")
cat("Generado:", Sys.time(), "\n")
cat("Archivos procesados:", length(archivos_procesar), "\n")
if(!is.null(datos_estirado_disponible)) {
  cat("Analisis estirado-pliegues: INCLUIDO\n")
}
cat("\n")

for(i in 1:length(archivos_procesar)) {
  archivo <- archivos_procesar[i]
  cat("ARCHIVO", i, ":", archivo, "\n")
  cat("", paste(rep("-", nchar(archivo) + 10), collapse = ""), "\n")
  
  resultado <- resultados_analisis[[archivo]]
  if(!is.null(resultado)) {
    if(!is.null(resultado$datos)) {
      cat("Observaciones:", nrow(resultado$datos), "\n")
    }
    if(!is.null(resultado$pca)) {
      cat("Varianza PCA:", round(resultado$pca$varianza_explicada * 100, 1), "%\n")
      cat("Variables importantes:", paste(head(resultado$pca$variables_importantes, 3), collapse = ", "), "\n")
    }
    if(!is.null(resultado$correlacion_estirado)) {
      cat("CORRELACIONES ESTIRADO-PLIEGUES:\n")
      if(!is.null(resultado$correlacion_estirado$correlacion_plieguepeq)) {
        cat("  Estirado vs Pliegue Pequeño:", round(resultado$correlacion_estirado$correlacion_plieguepeq, 3), "\n")
      }
      if(!is.null(resultado$correlacion_estirado$correlacion_plieguegr)) {
        cat("  Estirado vs Pliegue Grande:", round(resultado$correlacion_estirado$correlacion_plieguegr, 3), "\n")
      }
      if(!is.null(resultado$correlacion_estirado$correlacion_defectos)) {
        cat("  Estirado vs Defectos Totales:", round(resultado$correlacion_estirado$correlacion_defectos, 3), "\n")
      }
    }
  }
  cat("\n")
}

# Resumen de estirado si esta disponible
if(!is.null(datos_estirado_disponible)) {
  cat("DATOS DE ESTIRADO\n")
  cat("=================\n")
  cat("Archivo: ESTIRADOS.csv\n")
  cat("Registros:", nrow(datos_estirado_disponible), "\n")
  if("RECHAZO" %in% names(datos_estirado_disponible)) {
    tasa_promedio <- mean(datos_estirado_disponible$RECHAZO, na.rm = TRUE)
    cat("Tasa promedio estirado:", round(tasa_promedio, 3), "%\n")
  }
  if("MODELO" %in% names(datos_estirado_disponible)) {
    modelos_estirado <- unique(datos_estirado_disponible$MODELO)
    cat("Modelos en estirado:", length(modelos_estirado), "\n")
  }
  cat("\n")
}

sink()

cat("Resumen ejecutivo guardado:", basename(archivo_resumen), "\n")

# ============================================================================
# FINALIZACION
# ============================================================================

cat("\nFINALIZACION DEL ANALISIS\n")
cat("========================\n")

cat("Analisis completo finalizado exitosamente\n")
cat("Archivos procesados:", length(archivos_procesar), "\n")
cat("Resultados guardados en:", DIRECTORIO_RESULTADOS, "\n")
cat("Graficos guardados en:", DIRECTORIO_GRAFICOS, "\n")

if(!is.null(datos_estirado_disponible)) {
  cat("Analisis de correlacion estirado-pliegues incluido\n")
}

cat("\nSistema de analisis completamente operativo en estructura local\n")
cat("Directorio base:", DIRECTORIO_BASE, "\n")

# Guardar workspace para analisis posterior
workspace_file <- file.path(DIRECTORIO_RESULTADOS, paste0("workspace_", timestamp, ".RData"))
save.image(workspace_file)
cat("Workspace guardado:", basename(workspace_file), "\n")

cat("\nAnalisis completado. Todos los archivos disponibles en", DIRECTORIO_BASE, "\n")