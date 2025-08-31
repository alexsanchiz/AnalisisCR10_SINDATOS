# ============================================================================
# MODULO 01: CARGA Y MAPEO DE DATOS
# Sistema de Analisis Estadistico - Control de Calidad IS
# ============================================================================

cat("MODULO DE CARGA DE DATOS\n")
cat("========================\n")

# ============================================================================
# FUNCION PRINCIPAL DE CARGA
# ============================================================================

cargar_datos_local <- function(nombre_archivo) {
  cat("\nCargando archivo:", nombre_archivo, "\n")
  cat("", rep("=", nchar(nombre_archivo) + 17), "\n")
  
  # ==========================================
  # BUSQUEDA DE ARCHIVO EN MULTIPLES RUTAS
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
    cat("ERROR: Archivo no encontrado en ninguna ruta:\n")
    for(ruta in rutas_buscar) {
      cat("  ", ruta, "\n")
    }
    return(NULL)
  }
  
  cat("Archivo encontrado en:", archivo_encontrado, "\n")
  
  # ==========================================
  # CARGA DE DATOS CON MANEJO DE ENCODING
  # ==========================================
  
  datos <- NULL
  
  tryCatch({
    # Detectar encoding automaticamente
    encoding_detectado <- detectar_encoding(archivo_encontrado)
    cat("Encoding detectado:", encoding_detectado, "\n")
    
    # Cargar con read.csv base (mas robusto)
    datos <- read.csv(archivo_encontrado, 
                     sep = ";",           # Separador punto y coma
                     dec = ",",           # Decimal coma europea
                     header = TRUE,
                     stringsAsFactors = FALSE,
                     encoding = encoding_detectado,
                     na.strings = c("", "NA", "NULL"))
    
    cat("Datos cargados exitosamente:", nrow(datos), "filas x", ncol(datos), "columnas\n")
    
  }, error = function(e) {
    cat("Error cargando archivo:", e$message, "\n")
    
    # Fallback con readr si esta disponible
    if(require(readr, quietly = TRUE)) {
      cat("Intentando con readr...\n")
      
      tryCatch({
        datos <- read_delim(archivo_encontrado, 
                           delim = ";",
                           locale = locale(decimal_mark = ",", encoding = encoding_detectado),
                           na = c("", "NA", "NULL"))
        datos <- as.data.frame(datos)
        cat("Datos cargados con readr:", nrow(datos), "filas x", ncol(datos), "columnas\n")
      }, error = function(e2) {
        cat("Error tambien con readr:", e2$message, "\n")
        return(NULL)
      })
    }
  })
  
  if(is.null(datos) || nrow(datos) == 0) {
    cat("FALLO: No se pudieron cargar datos\n")
    return(NULL)
  }
  
  # ==========================================
  # MAPEO AUTOMATICO DE VARIABLES
  # ==========================================
  
  cat("\nMAPEO DE VARIABLES\n")
  cat("==================\n")
  
  variables_originales <- names(datos)
  cat("Variables encontradas:", length(variables_originales), "\n")
  cat("Variables:", paste(head(variables_originales, 10), collapse = ", "), 
      ifelse(length(variables_originales) > 10, "...", ""), "\n")
  
  # Mapeo automatico de N a FH si es necesario
  if("N" %in% variables_originales && !"FH" %in% variables_originales) {
    names(datos)[names(datos) == "N"] <- "FH"
    cat("Variable N mapeada a FH\n")
  }
  
  # ==========================================
  # PROCESAMIENTO DE FECHA-HORA
  # ==========================================
  
  if("FH" %in% names(datos)) {
    cat("\nPROCESANDO FECHA-HORA\n")
    cat("=====================\n")
    
    # Mostrar muestra de FH original
    muestra_fh <- head(datos$FH, 5)
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
        fh_temp <- as.POSIXct(datos$FH, format = formato)
        
        # Verificar si la conversion fue exitosa
        if(sum(!is.na(fh_temp)) > nrow(datos) * 0.8) {  # 80% exitoso
          datos$FH <- fh_temp
          fh_convertida <- TRUE
          cat("FH convertida exitosamente con formato:", formato, "\n")
          cat("Registros convertidos:", sum(!is.na(fh_temp)), "de", nrow(datos), "\n")
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
    
    # Mostrar estadisticas temporales si se convirtio
    if(fh_convertida && sum(!is.na(datos$FH)) > 0) {
      rango_temporal <- range(datos$FH, na.rm = TRUE)
      cat("Rango temporal:", rango_temporal[1], "a", rango_temporal[2], "\n")
    }
  }
  
  # ==========================================
  # VALIDACION DE ESTRUCTURA
  # ==========================================
  
  cat("\nVALIDACION DE ESTRUCTURA\n")
  cat("========================\n")
  
  variables_finales <- names(datos)
  variables_criticas <- c("FH", "PLIEGUEPEQ", "PLIEGUEGR", "DEFECTOS")
  
  # Verificar variables criticas
  variables_faltantes <- variables_criticas[!variables_criticas %in% variables_finales]
  if(length(variables_faltantes) > 0) {
    cat("ATENCION - Variables criticas faltantes:\n")
    for(var in variables_faltantes) {
      cat("  ", var, "\n")
    }
  } else {
    cat("Variables criticas presentes: OK\n")
  }
  
  # ==========================================
  # PROCESAMIENTO DE VARIABLES DEFECTO
  # ==========================================
  
  cat("\nPROCESANDO VARIABLES DE DEFECTO\n")
  cat("===============================\n")
  
  # Variables de defecto binarias
  variables_defecto <- c("CLARO", "PLIEGUEPEQ", "PLIEGUEGR")
  
  for(var in variables_defecto) {
    if(var %in% names(datos)) {
      # Convertir a binario (1 = defecto detectado, 0 = no detectado)
      datos[[var]] <- as.numeric(datos[[var]] == 1 | datos[[var]] == "1")
      cat("Variable", var, "convertida a binaria\n")
    }
  }
  
  # Variable DEFECTOS como union logica
  if(all(c("CLARO", "PLIEGUEPEQ", "PLIEGUEGR") %in% names(datos))) {
    datos$DEFECTOS <- as.numeric(
      datos$CLARO == 1 | datos$PLIEGUEPEQ == 1 | datos$PLIEGUEGR == 1
    )
    cat("Variable DEFECTOS calculada como union de defectos individuales\n")
  }
  
  # ==========================================
  # CONVERSION DE TIPOS DE DATOS
  # ==========================================
  
  cat("\nCONVERSION DE TIPOS\n")
  cat("===================\n")
  
  # Variables categoricas
  variables_categoricas <- c("MODELO", "SECCION", "CAVIDAD")
  for(var in variables_categoricas) {
    if(var %in% names(datos)) {
      datos[[var]] <- as.factor(datos[[var]])
      cat("Variable", var, "convertida a factor\n")
    }
  }
  
  # Variables numericas (resto)
  variables_numericas <- setdiff(names(datos), c("FH", variables_categoricas))
  for(var in variables_numericas) {
    if(var %in% names(datos) && !is.numeric(datos[[var]])) {
      tryCatch({
        datos[[var]] <- as.numeric(datos[[var]])
        cat("Variable", var, "convertida a numerico\n")
      }, error = function(e) {
        cat("No se pudo convertir", var, "a numerico\n")
      })
    }
  }
  
  # ==========================================
  # ESTADISTICAS FINALES
  # ==========================================
  
  cat("\nESTADISTICAS FINALES\n")
  cat("====================\n")
  cat("Dataset:", nombre_archivo, "\n")
  cat("Observaciones:", nrow(datos), "\n")
  cat("Variables:", ncol(datos), "\n")
  
  # Estadisticas de defectos si estan disponibles
  if("DEFECTOS" %in% names(datos)) {
    tasa_defectos <- mean(datos$DEFECTOS, na.rm = TRUE) * 100
    cat("Tasa de defectos:", round(tasa_defectos, 3), "%\n")
    
    # Defectos por tipo
    if("PLIEGUEPEQ" %in% names(datos)) {
      tasa_plieguepeq <- mean(datos$PLIEGUEPEQ, na.rm = TRUE) * 100
      cat("Tasa pliegue pequeño:", round(tasa_plieguepeq, 3), "%\n")
    }
    if("PLIEGUEGR" %in% names(datos)) {
      tasa_plieguegr <- mean(datos$PLIEGUEGR, na.rm = TRUE) * 100
      cat("Tasa pliegue grande:", round(tasa_plieguegr, 3), "%\n")
    }
  }
  
  # Agregar metadatos
  attr(datos, "archivo_original") <- nombre_archivo
  attr(datos, "fecha_carga") <- Sys.time()
  attr(datos, "variables_originales") <- variables_originales
  
  cat("Datos cargados y procesados exitosamente\n")
  return(datos)
}

# ============================================================================
# MENSAJE DE CARGA DEL MODULO
# ============================================================================

cat("MODULO DE CARGA DE DATOS OPERATIVO\n")
cat("===================================\n")
cat("Funcion disponible: cargar_datos_local('archivo.csv')\n")
cat("Mapeo automatico N -> FH implementado\n")
cat("Procesamiento de defectos binarios habilitado\n")
cat("Soporte para formato DD/MM/AAAA HH:MM\n")