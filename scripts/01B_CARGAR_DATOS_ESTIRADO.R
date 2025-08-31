# ============================================================================
# MODULO 01B: CARGA DE DATOS DE ESTIRADO
# Sistema de Analisis Estadistico - Control de Calidad IS
# ============================================================================

cat("MODULO DE DATOS DE ESTIRADO\n")
cat("===========================\n")

# ============================================================================
# DETECCION AUTOMATICA DE ARCHIVO DE ESTIRADO POR PERIODO
# ============================================================================

detectar_archivo_estirado <- function(archivo_principal) {
  cat("\nDETECTANDO ARCHIVO DE ESTIRADO CORRESPONDIENTE\n")
  cat("=============================================\n")
  cat("Archivo principal:", archivo_principal, "\n")
  
  # Extraer periodo del archivo principal
  if(grepl("NOV2024", archivo_principal, ignore.case = TRUE)) {
    archivo_estirado <- "ESTIRADOS_NOV2024.csv"
    cat("Periodo detectado: NOV2024\n")
  } else if(grepl("FEB2025", archivo_principal, ignore.case = TRUE)) {
    archivo_estirado <- "ESTIRADOS_FEB2025.csv"
    cat("Periodo detectado: FEB2025\n")
  } else {
    # Fallback al archivo original unificado
    archivo_estirado <- "ESTIRADOS.csv"
    cat("Periodo no detectado - usando archivo unificado\n")
  }
  
  cat("Archivo de estirado seleccionado:", archivo_estirado, "\n")
  return(archivo_estirado)
}

# ============================================================================
# FUNCION DE CARGA DE DATOS DE ESTIRADO
# ============================================================================

cargar_datos_estirado_local <- function(nombre_archivo) {
  cat("\nCargando datos de estirado:", nombre_archivo, "\n")
  cat("", rep("=", nchar(nombre_archivo) + 26), "\n")
  
  # ==========================================
  # BUSQUEDA DE ARCHIVO
  # ==========================================
  
  rutas_buscar <- c(
    file.path(DIRECTORIO_DATOS, nombre_archivo),     # D:/CR10/datos/
    file.path("datos", nombre_archivo),              # ./datos/
    file.path(".", nombre_archivo),                  # ./
    nombre_archivo                                   # ruta relativa
  )
  
  archivo_encontrado <- NULL
  for(ruta in rutas_buscar) {
    if(file.exists(ruta)) {
      archivo_encontrado <- ruta
      break
    }
  }
  
  if(is.null(archivo_encontrado)) {
    cat("ERROR: Archivo de estirado no encontrado en ninguna ruta:\n")
    for(ruta in rutas_buscar) {
      cat("  ", ruta, "\n")
    }
    return(NULL)
  }
  
  cat("Archivo encontrado en:", archivo_encontrado, "\n")
  
  # ==========================================
  # CARGA DE DATOS
  # ==========================================
  
  datos_estirado <- NULL
  
  tryCatch({
    # Detectar encoding automaticamente
    encoding_detectado <- detectar_encoding(archivo_encontrado)
    cat("Encoding detectado:", encoding_detectado, "\n")
    
    # Cargar con read.csv
    datos_estirado <- read.csv(archivo_encontrado, 
                              sep = ";",           
                              dec = ",",           
                              header = TRUE,
                              stringsAsFactors = FALSE,
                              encoding = encoding_detectado,
                              na.strings = c("", "NA", "NULL"))
    
  }, error = function(e) {
    cat("Error cargando archivo:", e$message, "\n")
    return(NULL)
  })
  
  if(is.null(datos_estirado) || nrow(datos_estirado) == 0) {
    cat("FALLO: No se pudieron cargar datos de estirado\n")
    return(NULL)
  }
  
  cat("Datos de estirado cargados:", nrow(datos_estirado), "filas x", ncol(datos_estirado), "columnas\n")
  
  # ==========================================
  # VALIDACION DE ESTRUCTURA
  # ==========================================
  
  cat("\nVALIDACION DE ESTRUCTURA\n")
  cat("========================\n")
  
  # Verificar estructura esperada
  variables_esperadas <- c("FH", "RECHAZO", "MODELO")
  variables_encontradas <- names(datos_estirado)
  
  cat("Variables esperadas:", length(variables_esperadas), "\n")
  cat("Variables encontradas:", length(variables_encontradas), "\n")
  
  # Verificar variables críticas
  variables_faltantes <- variables_esperadas[!variables_esperadas %in% variables_encontradas]
  if(length(variables_faltantes) > 0) {
    cat("ATENCION - Variables faltantes:\n")
    for(var in variables_faltantes) {
      cat("  ", var, "\n")
    }
  }
  
  cat("Estructura detectada: FH (DD/MM/AAAA HH:MM), RECHAZO (%), MODELO (numerico)\n")
  
  # Mostrar muestra de datos
  cat("\nMuestra de datos:\n")
  if(nrow(datos_estirado) > 0) {
    muestra <- head(datos_estirado, 3)
    for(i in 1:nrow(muestra)) {
      cat("  Fila", i, ":")
      for(var in names(muestra)) {
        cat(" ", var, "=", muestra[i, var])
      }
      cat("\n")
    }
  }
  
  # ==========================================
  # PROCESAMIENTO DE FECHA-HORA
  # ==========================================
  
  if("FH" %in% names(datos_estirado)) {
    cat("\nPROCESANDO FECHA-HORA ESTIRADO\n")
    cat("==============================\n")
    
    # Mostrar muestra de FH original
    muestra_fh <- head(datos_estirado$FH, 3)
    cat("Muestra FH original:\n")
    for(i in 1:length(muestra_fh)) {
      cat("  ", muestra_fh[i], "\n")
    }
    
    # Intentar conversion a POSIXct
    fh_convertida <- FALSE
    
    # Formatos a probar (DD/MM/AAAA HH:MM primero)
    formatos_probar <- c(
      "%d/%m/%Y %H:%M",     # DD/MM/AAAA HH:MM
      "%d/%m/%Y %H:%M:%S",  # DD/MM/AAAA HH:MM:SS
      "%Y-%m-%d %H:%M",     # YYYY-MM-DD HH:MM
      "%Y-%m-%d %H:%M:%S"   # YYYY-MM-DD HH:MM:SS
    )
    
    for(formato in formatos_probar) {
      tryCatch({
        fh_temp <- as.POSIXct(datos_estirado$FH, format = formato)
        
        # Verificar si la conversion fue exitosa
        if(sum(!is.na(fh_temp)) > nrow(datos_estirado) * 0.8) {  # 80% exitoso
          datos_estirado$FH <- fh_temp
          fh_convertida <- TRUE
          cat("FH convertida exitosamente con formato:", formato, "\n")
          cat("Registros convertidos:", sum(!is.na(fh_temp)), "de", nrow(datos_estirado), "\n")
          break
        }
      }, error = function(e) {
        # Continuar con siguiente formato
      })
    }
    
    if(!fh_convertida) {
      cat("ATENCION: No se pudo convertir FH a formato temporal\n")
      cat("FH se mantendra como texto\n")
    }
  }
  
  # ==========================================
  # PROCESAMIENTO DE VARIABLES
  # ==========================================
  
  cat("\nPROCESANDO VARIABLES\n")
  cat("====================\n")
  
  # Convertir RECHAZO a numerico (manejar formato de porcentaje)
  if("RECHAZO" %in% names(datos_estirado)) {
    tryCatch({
      # Limpiar formato de porcentaje: "0,07%" -> 0.07
      rechazo_limpio <- datos_estirado$RECHAZO
      rechazo_limpio <- gsub("%", "", rechazo_limpio)  # Quitar %
      rechazo_limpio <- gsub(",", ".", rechazo_limpio) # Cambiar , por .
      datos_estirado$RECHAZO <- as.numeric(rechazo_limpio)
      cat("RECHAZO convertido a numerico (formato porcentaje procesado)\n")
    }, error = function(e) {
      cat("Error convirtiendo RECHAZO:", e$message, "\n")
    })
  }
  
  # Convertir MODELO a factor (si es texto) o mantener numerico
  if("MODELO" %in% names(datos_estirado)) {
    # Si MODELO es numerico, mantenerlo; si es texto, convertir a factor
    if(is.character(datos_estirado$MODELO) && !all(grepl("^[0-9.]+$", datos_estirado$MODELO))) {
      datos_estirado$MODELO <- as.factor(datos_estirado$MODELO)
      cat("MODELO convertido a factor\n")
    } else {
      datos_estirado$MODELO <- as.numeric(datos_estirado$MODELO)
      cat("MODELO convertido a numerico\n")
    }
  }
  
  # ==========================================
  # ESTADISTICAS BASICAS
  # ==========================================
  
  cat("\nESTADISTICAS BASICAS\n")
  cat("====================\n")
  
  # Estadisticas de rechazo/estirado
  if("RECHAZO" %in% names(datos_estirado)) {
    registros_validos <- sum(!is.na(datos_estirado$RECHAZO))
    tasa_promedio <- mean(datos_estirado$RECHAZO, na.rm = TRUE)
    tasa_min <- min(datos_estirado$RECHAZO, na.rm = TRUE)
    tasa_max <- max(datos_estirado$RECHAZO, na.rm = TRUE)
    
    cat("Registros RECHAZO validos:", registros_validos, "de", nrow(datos_estirado), "\n")
    cat("Tasa estirado:\n")
    cat("   Promedio:", round(tasa_promedio, 3), "%\n")
    cat("   Rango:", round(tasa_min, 3), "% a", round(tasa_max, 3), "%\n")
  }
  
  if("MODELO" %in% names(datos_estirado)) {
    modelos_unicos <- unique(datos_estirado$MODELO)
    cat("Modelos disponibles:", length(modelos_unicos), "(", paste(head(modelos_unicos, 5), collapse = ", "), 
        ifelse(length(modelos_unicos) > 5, "...", ""), ")\n")
  }
  
  # Agregar metadatos
  attr(datos_estirado, "tipo") <- "estirado"
  attr(datos_estirado, "archivo_original") <- nombre_archivo
  attr(datos_estirado, "fecha_carga") <- Sys.time()
  
  return(datos_estirado)
}

cat("MODULO DE DATOS DE ESTIRADO CARGADO\n")
cat("===================================\n")
cat("Funcion disponible: cargar_datos_estirado_local('archivo.csv')\n")
cat("Estructura esperada: FH;RECHAZO;MODELO\n")
cat("FH: DD/MM/AAAA HH:MM, RECHAZO: porcentaje, MODELO: numerico\n")