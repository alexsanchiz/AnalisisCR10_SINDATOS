# ============================================================================
# MODULO 02B: ANALISIS DE CORRELACION ESTIRADO-PLIEGUES
# Sistema de Analisis Estadistico - Control de Calidad IS
# ============================================================================

cat("MODULO DE CORRELACION ESTIRADO-PLIEGUES\n")
cat("========================================\n")

# ============================================================================
# FUNCION PRINCIPAL DE CORRELACION
# ============================================================================

correlacionar_estirado_pliegues <- function(datos_principales, datos_estirado) {
  cat("\nCORRELACIONANDO ESTIRADO CON PLIEGUES\n")
  cat("=====================================\n")
  
  # ==========================================
  # VALIDACIONES INICIALES
  # ==========================================
  
  if(is.null(datos_principales) || is.null(datos_estirado)) {
    cat("Error: Uno de los datasets esta vacio\n")
    return(NULL)
  }
  
  # Verificar variables criticas
  vars_requeridas_principales <- c("FH", "PLIEGUEPEQ", "PLIEGUEGR", "DEFECTOS")
  vars_requeridas_estirado <- c("FH", "RECHAZO")
  
  if(!all(vars_requeridas_principales %in% names(datos_principales))) {
    cat("Error: Variables faltantes en datos principales\n")
    return(NULL)
  }
  
  if(!all(vars_requeridas_estirado %in% names(datos_estirado))) {
    cat("Error: Variables faltantes en datos de estirado\n")
    return(NULL)
  }
  
  cat("Datos principales:", nrow(datos_principales), "registros\n")
  cat("Datos estirado:", nrow(datos_estirado), "registros\n")
  
  # ==========================================
  # VERIFICAR FORMATO TEMPORAL
  # ==========================================
  
  fh_principal_temporal <- inherits(datos_principales$FH, "POSIXct")
  fh_estirado_temporal <- inherits(datos_estirado$FH, "POSIXct")
  
  cat("FH principal temporal:", fh_principal_temporal, "\n")
  cat("FH estirado temporal:", fh_estirado_temporal, "\n")
  
  if(!fh_principal_temporal || !fh_estirado_temporal) {
    cat("Error: Variables FH deben estar en formato temporal POSIXct\n")
    return(NULL)
  }
  
  # ============================
  # PREPARAR DATOS PARA MERGE
  # ============================
  
  # Preparar datos principales para merge por fecha y hora
  if(fh_principal_temporal) {
    datos_principales$fecha <- as.Date(datos_principales$FH)
    datos_principales$hora <- as.numeric(format(datos_principales$FH, "%H"))
    datos_principales$fecha_hora <- as.POSIXct(paste(datos_principales$fecha, sprintf("%02d:00", datos_principales$hora)), format = "%Y-%m-%d %H:%M")
    
    cat("Preparados datos principales por fecha-hora\n")
  }
  
  # Preparar datos estirado para merge
  if(fh_estirado_temporal) {
    datos_estirado$fecha <- as.Date(datos_estirado$FH) 
    datos_estirado$hora <- as.numeric(format(datos_estirado$FH, "%H"))
    datos_estirado$fecha_hora <- as.POSIXct(paste(datos_estirado$fecha, sprintf("%02d:00", datos_estirado$hora)), format = "%Y-%m-%d %H:%M")
    
    cat("Preparados datos estirado por fecha-hora\n")
  }
  
  # ===============================
  # MERGE DIRECTO POR FECHA-HORA
  # ===============================
  
  # Merge directo por fecha-hora exacta
  datos_combinados <- NULL
  
  if(fh_principal_temporal && fh_estirado_temporal) {
    # Merge por fecha_hora exacta
    datos_combinados <- merge(
      datos_principales,
      datos_estirado[, c("fecha_hora", "RECHAZO", "MODELO")],
      by = "fecha_hora",
      all.x = TRUE,
      suffixes = c("_principal", "_estirado")
    )
    
    cat("Merge completado por fecha-hora exacta:", nrow(datos_combinados), "registros\n")
    
    # Verificar cobertura
    registros_con_estirado <- sum(!is.na(datos_combinados$RECHAZO))
    cobertura <- registros_con_estirado / nrow(datos_combinados) * 100
    cat("Cobertura de datos estirado:", registros_con_estirado, "de", nrow(datos_combinados), 
        "(", round(cobertura, 1), "%)\n")
  }

  
  # ===============================
  # ANALISIS DE CORRELACIONES
  # ===============================
  
  resultados_correlacion <- list()
  
  if(!is.null(datos_combinados) && nrow(datos_combinados) > 10) {
    # Extraer variables para correlacion (datos ya combinados hora a hora)
    datos_correlacion <- data.frame(
      plieguepeq = datos_combinados$PLIEGUEPEQ,
      plieguegr = datos_combinados$PLIEGUEGR,
      defectos = datos_combinados$DEFECTOS,
      rechazo_estirado = datos_combinados$RECHAZO
    )
    
    # Filtrar solo casos completos
    datos_correlacion <- datos_correlacion[complete.cases(datos_correlacion), ]
    
    if(nrow(datos_correlacion) > 5) {
      cat("\nCORRELACIONES ESTIRADO-PLIEGUES:\n")
      cat("=================================\n")
      
      # Correlaciones principales
      cor_estirado_plieguepeq <- cor(datos_correlacion$rechazo_estirado, datos_correlacion$plieguepeq, use = "complete.obs")
      cor_estirado_plieguegr <- cor(datos_correlacion$rechazo_estirado, datos_correlacion$plieguegr, use = "complete.obs")
      cor_estirado_defectos <- cor(datos_correlacion$rechazo_estirado, datos_correlacion$defectos, use = "complete.obs")
      
      cat("Estirado vs Pliegue Pequeño:", round(cor_estirado_plieguepeq, 3), "\n")
      cat("Estirado vs Pliegue Grande:", round(cor_estirado_plieguegr, 3), "\n")
      cat("Estirado vs Defectos Totales:", round(cor_estirado_defectos, 3), "\n")
      cat("Observaciones analizadas:", nrow(datos_correlacion), "\n")
      
      # ===============================
      # ANALISIS AVANZADO: COMPARACION DETECCION AUTOMATICA VS MANUAL
      # ===============================
      
      cat("\nCOMPARACION DETECCION AUTOMATICA vs MANUAL\n")
      cat("==========================================\n")
      cat("IMPORTANTE: Datos estirado son tasas promedio por turno (8h)\n")
      cat("Datos defectos son deteccion manual hora a hora por operarios\n")
      cat("Limitacion: Comparacion imprecisa por diferencia temporal\n\n")
      
      # Estadisticas descriptivas de ambos sistemas
      cat("SISTEMA AUTOMATICO (Estirado):\n")
      cat("  Media:", round(mean(datos_correlacion$rechazo_estirado, na.rm = TRUE), 3), "%\n")
      cat("  Mediana:", round(median(datos_correlacion$rechazo_estirado, na.rm = TRUE), 3), "%\n")
      cat("  SD:", round(sd(datos_correlacion$rechazo_estirado, na.rm = TRUE), 3), "%\n")
      cat("  Min:", round(min(datos_correlacion$rechazo_estirado, na.rm = TRUE), 3), "%\n")
      cat("  Max:", round(max(datos_correlacion$rechazo_estirado, na.rm = TRUE), 3), "%\n")
      
      cat("\nSISTEMA MANUAL (Operarios):\n")
      tasa_manual <- mean(datos_correlacion$defectos, na.rm = TRUE) * 100
      cat("  Tasa defectos totales:", round(tasa_manual, 3), "%\n")
      cat("  Tasa pliegue pequeño:", round(mean(datos_correlacion$plieguepeq, na.rm = TRUE) * 100, 3), "%\n")
      cat("  Tasa pliegue grande:", round(mean(datos_correlacion$plieguegr, na.rm = TRUE) * 100, 3), "%\n")
      
      # Comparacion directa cuando ambos sistemas detectan problemas
      ambos_detectan <- sum(datos_correlacion$rechazo_estirado > 0 & datos_correlacion$defectos == 1, na.rm = TRUE)
      solo_automatico <- sum(datos_correlacion$rechazo_estirado > 0 & datos_correlacion$defectos == 0, na.rm = TRUE)
      solo_manual <- sum(datos_correlacion$rechazo_estirado == 0 & datos_correlacion$defectos == 1, na.rm = TRUE)
      ambos_ok <- sum(datos_correlacion$rechazo_estirado == 0 & datos_correlacion$defectos == 0, na.rm = TRUE)
      
      cat("\nCONCORDANCIA ENTRE SISTEMAS:\n")
      cat("  Ambos detectan problemas:", ambos_detectan, "casos\n")
      cat("  Solo automatico detecta:", solo_automatico, "casos\n")
      cat("  Solo manual detecta:", solo_manual, "casos\n")
      cat("  Ambos sistemas OK:", ambos_ok, "casos\n")
      
      concordancia <- (ambos_detectan + ambos_ok) / nrow(datos_correlacion) * 100
      cat("  Concordancia total:", round(concordancia, 1), "%\n")
      
      # Test de correlacion con significancia
      if(require(stats, quietly = TRUE)) {
        test_cor_defectos <- cor.test(datos_correlacion$rechazo_estirado, datos_correlacion$defectos)
        test_cor_plieguepeq <- cor.test(datos_correlacion$rechazo_estirado, datos_correlacion$plieguepeq)
        test_cor_plieguegr <- cor.test(datos_correlacion$rechazo_estirado, datos_correlacion$plieguegr)
        
        cat("\nTESTS DE SIGNIFICANCIA:\n")
        cat("  Estirado vs Defectos: r =", round(test_cor_defectos$estimate, 3), 
            ", p =", round(test_cor_defectos$p.value, 4), "\n")
        cat("  Estirado vs Pliegue Peq: r =", round(test_cor_plieguepeq$estimate, 3), 
            ", p =", round(test_cor_plieguepeq$p.value, 4), "\n")
        cat("  Estirado vs Pliegue Gr: r =", round(test_cor_plieguegr$estimate, 3), 
            ", p =", round(test_cor_plieguegr$p.value, 4), "\n")
      }
      
      # Guardar resultados expandidos
      resultados_correlacion <- list(
        correlacion_plieguepeq = cor_estirado_plieguepeq,
        correlacion_plieguegr = cor_estirado_plieguegr,
        correlacion_defectos = cor_estirado_defectos,
        datos_correlacion = datos_correlacion,
        n_observaciones = nrow(datos_correlacion),
        estadisticas_automatico = list(
          media = mean(datos_correlacion$rechazo_estirado, na.rm = TRUE),
          mediana = median(datos_correlacion$rechazo_estirado, na.rm = TRUE),
          sd = sd(datos_correlacion$rechazo_estirado, na.rm = TRUE)
        ),
        estadisticas_manual = list(
          tasa_defectos = tasa_manual,
          tasa_plieguepeq = mean(datos_correlacion$plieguepeq, na.rm = TRUE) * 100,
          tasa_plieguegr = mean(datos_correlacion$plieguegr, na.rm = TRUE) * 100
        ),
        concordancia = list(
          ambos_detectan = ambos_detectan,
          solo_automatico = solo_automatico,
          solo_manual = solo_manual,
          ambos_ok = ambos_ok,
          porcentaje_concordancia = concordancia
        )
      )
      
      # Analisis por rango de horas
      if("hora" %in% names(datos_combinados)) {
        cat("\nANALISIS POR RANGO DE HORAS:\n")
        cat("============================\n")
        
        # Definir turnos aproximados basados en hora
        datos_combinados$turno_estimado <- ifelse(datos_combinados$hora >= 6 & datos_combinados$hora < 14, "MAÑANA",
                                                 ifelse(datos_combinados$hora >= 14 & datos_combinados$hora < 22, "TARDE", "NOCHE"))
        
        turnos_disponibles <- unique(datos_combinados$turno_estimado)
        turnos_disponibles <- turnos_disponibles[!is.na(turnos_disponibles)]
        
        for(turno in turnos_disponibles) {
          datos_turno <- datos_combinados[datos_combinados$turno_estimado == turno & !is.na(datos_combinados$RECHAZO), ]
          
          if(nrow(datos_turno) > 3) {
            tasa_estirado_turno <- mean(datos_turno$RECHAZO, na.rm = TRUE)
            tasa_pliegues_turno <- mean(datos_turno$PLIEGUEPEQ, na.rm = TRUE) * 100
            
            cat("Turno", turno, ":\n")
            cat("   Estirado promedio:", round(tasa_estirado_turno, 3), "%\n")
            cat("   Pliegues promedio:", round(tasa_pliegues_turno, 3), "%\n")
            cat("   Registros:", nrow(datos_turno), "\n")
          }
        }
      }
      
    } else {
      cat("Datos insuficientes para correlacion (", nrow(datos_correlacion), "casos completos)\n")
      cat("CAUSA: Fechas de datos ESTIRADOS no coinciden con datos principales\n")
      cat("SOLUCION: Necesita archivo ESTIRADOS.csv del mismo periodo temporal\n")
    }
  } else {
    cat("No se pudieron combinar los datos para correlacion\n")
  }
  
  return(resultados_correlacion)
}

# ============================================================================
# MENSAJE DE CARGA DEL MODULO
# ============================================================================

cat("MODULO DE CORRELACION ESTIRADO-PLIEGUES OPERATIVO\n")
cat("==================================================\n")
cat("Funcion disponible: correlacionar_estirado_pliegues(datos_principales, datos_estirado)\n")
cat("Correlaciona tasas de estirado hora a hora con defectos de pliegues\n")
cat("Calcula correlaciones: Estirado vs Pliegue Pequeño/Grande/Total\n")