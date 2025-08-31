# ============================================================================
# MODULO 06: ANALISIS ESTIRADO AVANZADO CON LAGS Y NO LINEALIDADES
# Sistema de Analisis Estadistico - Control de Calidad IS
# ============================================================================
# Análisis con efectos retardados (lags) y relaciones no lineales del estirado
# ============================================================================

cat("MODULO DE ANALISIS ESTIRADO AVANZADO\n")
cat("====================================\n")

# ============================================================================
# FUNCION PRINCIPAL DE ANALISIS ESTIRADO CON LAGS
# ============================================================================

ejecutar_analisis_estirado_avanzado <- function(datos_principales, datos_estirado, nombre_archivo) {
  cat("\nEJECUTANDO ANALISIS ESTIRADO AVANZADO\n")
  cat("=====================================\n")
  cat("Archivo principal:", nombre_archivo, "\n")
  cat("Datos principales:", nrow(datos_principales), "observaciones\n")
  cat("Datos estirado:", nrow(datos_estirado), "observaciones\n\n")
  
  # Verificar paquetes necesarios
  paquetes_estirado <- c("dplyr", "splines", "mgcv", "forecast")
  
  for(paquete in paquetes_estirado) {
    if(!require(paquete, character.only = TRUE, quietly = TRUE)) {
      cat("Instalando paquete:", paquete, "\n")
      install.packages(paquete, dependencies = TRUE)
      library(paquete, character.only = TRUE)
    }
  }
  
  # Preparar datos temporales
  datos_principales$FH_hora <- as.POSIXct(trunc(datos_principales$FH, "hour"))
  datos_estirado$FH_hora <- as.POSIXct(trunc(datos_estirado$FH, "hour"))
  
  # Variables defecto
  vars_defecto <- c("CLARO", "PLIEGUEPEQ", "PLIEGUEGR", "DEFECTOS")
  
  # ==========================================
  # CREAR DATASET CON LAGS
  # ==========================================
  
  cat("CREANDO DATASET CON LAGS TEMPORALES\n")
  cat("====================================\n")
  
  # Agregar datos principales por hora
  datos_hora <- datos_principales %>%
    group_by(FH_hora, MODELO, SECCION) %>%
    summarise(
      across(all_of(vars_defecto), ~ mean(.x, na.rm = TRUE)),
      n_obs = n(),
      .groups = "drop"
    )
  
  # Crear lags del estirado (1h, 2h, 4h, 8h hacia atrás)
  datos_estirado_lags <- datos_estirado %>%
    arrange(FH_hora) %>%
    mutate(
      estirado_lag1 = lag(RECHAZO, 1),  # 1 hora atrás
      estirado_lag2 = lag(RECHAZO, 2),  # 2 horas atrás
      estirado_lag4 = lag(RECHAZO, 4),  # 4 horas atrás
      estirado_lag8 = lag(RECHAZO, 8),  # 8 horas atrás
      estirado_promedio_2h = (RECHAZO + lag(RECHAZO, 1)) / 2,
      estirado_promedio_4h = (RECHAZO + lag(RECHAZO, 1) + lag(RECHAZO, 2) + lag(RECHAZO, 3)) / 4,
      estirado_tendencia = RECHAZO - lag(RECHAZO, 1),  # Cambio respecto hora anterior
      estirado_volatilidad = abs(estirado_tendencia)   # Volatilidad
    )
  
  # Merge con lags
  datos_completos <- merge(datos_hora, datos_estirado_lags, 
                          by = c("FH_hora", "MODELO"), all.x = TRUE)
  
  # Filtrar datos completos
  datos_analisis <- datos_completos[complete.cases(datos_completos[, c("RECHAZO", "estirado_lag1", "estirado_lag2")]), ]
  
  cat("Datos con lags creados:", nrow(datos_analisis), "observaciones\n")
  
  if(nrow(datos_analisis) < 50) {
    cat("ADVERTENCIA: Datos insuficientes para análisis de lags\n")
    return(NULL)
  }
  
  # ==========================================
  # ANALISIS DE CORRELACIONES CON LAGS
  # ==========================================
  
  cat("\nANALISIS DE CORRELACIONES CON LAGS\n")
  cat("===================================\n")
  
  correlaciones_lags <- list()
  
  for(var_defecto in vars_defecto) {
    if(var_defecto %in% names(datos_analisis)) {
      
      cat("Analizando correlaciones para:", var_defecto, "\n")
      
      # Correlaciones con diferentes lags
      cor_simultaneo <- cor(datos_analisis[[var_defecto]], datos_analisis$RECHAZO, use = "complete.obs")
      cor_lag1 <- cor(datos_analisis[[var_defecto]], datos_analisis$estirado_lag1, use = "complete.obs")
      cor_lag2 <- cor(datos_analisis[[var_defecto]], datos_analisis$estirado_lag2, use = "complete.obs")
      cor_lag4 <- cor(datos_analisis[[var_defecto]], datos_analisis$estirado_lag4, use = "complete.obs")
      cor_lag8 <- cor(datos_analisis[[var_defecto]], datos_analisis$estirado_lag8, use = "complete.obs")
      
      # Correlaciones con promedios móviles
      cor_prom2h <- cor(datos_analisis[[var_defecto]], datos_analisis$estirado_promedio_2h, use = "complete.obs")
      cor_prom4h <- cor(datos_analisis[[var_defecto]], datos_analisis$estirado_promedio_4h, use = "complete.obs")
      
      # Correlaciones con tendencia y volatilidad
      cor_tendencia <- cor(datos_analisis[[var_defecto]], datos_analisis$estirado_tendencia, use = "complete.obs")
      cor_volatilidad <- cor(datos_analisis[[var_defecto]], datos_analisis$estirado_volatilidad, use = "complete.obs")
      
      cat("  Simultaneo:", sprintf("%6.3f", cor_simultaneo), "\n")
      cat("  Lag 1h:    ", sprintf("%6.3f", cor_lag1), "\n")
      cat("  Lag 2h:    ", sprintf("%6.3f", cor_lag2), "\n")
      cat("  Lag 4h:    ", sprintf("%6.3f", cor_lag4), "\n")
      cat("  Lag 8h:    ", sprintf("%6.3f", cor_lag8), "\n")
      cat("  Prom 2h:   ", sprintf("%6.3f", cor_prom2h), "\n")
      cat("  Prom 4h:   ", sprintf("%6.3f", cor_prom4h), "\n")
      cat("  Tendencia: ", sprintf("%6.3f", cor_tendencia), "\n")
      cat("  Volatilidad:", sprintf("%6.3f", cor_volatilidad), "\n")
      
      # Identificar mejor lag
      correlaciones_lag <- c(cor_simultaneo, cor_lag1, cor_lag2, cor_lag4, cor_lag8)
      nombres_lag <- c("Simultaneo", "Lag1h", "Lag2h", "Lag4h", "Lag8h")
      mejor_lag_idx <- which.max(abs(correlaciones_lag))
      mejor_lag <- nombres_lag[mejor_lag_idx]
      mejor_correlacion <- correlaciones_lag[mejor_lag_idx]
      
      cat("  MEJOR LAG:", mejor_lag, "- Correlación:", sprintf("%.3f", mejor_correlacion), "\n\n")
      
      correlaciones_lags[[var_defecto]] <- list(
        simultaneo = cor_simultaneo,
        lag1h = cor_lag1,
        lag2h = cor_lag2,
        lag4h = cor_lag4,
        lag8h = cor_lag8,
        promedio_2h = cor_prom2h,
        promedio_4h = cor_prom4h,
        tendencia = cor_tendencia,
        volatilidad = cor_volatilidad,
        mejor_lag = mejor_lag,
        mejor_correlacion = mejor_correlacion
      )
    }
  }
  
  # ==========================================
  # ANALISIS NO LINEAL DEL ESTIRADO
  # ==========================================
  
  cat("ANALISIS NO LINEAL DEL ESTIRADO\n")
  cat("===============================\n")
  
  modelos_nolineales <- list()
  
  for(var_defecto in vars_defecto) {
    if(var_defecto %in% names(datos_analisis) && 
       sum(datos_analisis[[var_defecto]] > 0, na.rm = TRUE) >= 20) {
      
      cat("Ajustando modelos no lineales para:", var_defecto, "\n")
      
      tryCatch({
        # Modelo lineal de referencia
        modelo_lineal <- lm(as.formula(paste0(var_defecto, " ~ RECHAZO + estirado_lag1 + estirado_tendencia")), 
                           data = datos_analisis)
        
        # Modelo con splines
        modelo_splines <- lm(as.formula(paste0(var_defecto, " ~ ns(RECHAZO, df=3) + ns(estirado_lag1, df=3) + estirado_tendencia")), 
                            data = datos_analisis)
        
        # Modelo polinomial
        modelo_poli <- lm(as.formula(paste0(var_defecto, " ~ poly(RECHAZO, 2) + poly(estirado_lag1, 2) + estirado_tendencia")), 
                         data = datos_analisis)
        
        # Comparar modelos
        anova_splines <- anova(modelo_lineal, modelo_splines)
        anova_poli <- anova(modelo_lineal, modelo_poli)
        
        p_splines <- anova_splines$`Pr(>F)`[2]
        p_poli <- anova_poli$`Pr(>F)`[2]
        
        # R² de modelos
        r2_lineal <- summary(modelo_lineal)$r.squared
        r2_splines <- summary(modelo_splines)$r.squared
        r2_poli <- summary(modelo_poli)$r.squared
        
        cat("  R² Lineal:   ", sprintf("%.4f", r2_lineal), "\n")
        cat("  R² Splines:  ", sprintf("%.4f", r2_splines), "- p-value:", sprintf("%.4f", p_splines), "\n")
        cat("  R² Polinomial:", sprintf("%.4f", r2_poli), "- p-value:", sprintf("%.4f", p_poli), "\n")
        
        # Determinar mejor modelo
        mejor_modelo <- "lineal"
        if(p_splines < 0.05 && r2_splines > r2_lineal) {
          mejor_modelo <- "splines"
        } else if(p_poli < 0.05 && r2_poli > r2_lineal) {
          mejor_modelo <- "polinomial"
        }
        
        cat("  MEJOR MODELO:", mejor_modelo, "\n\n")
        
        modelos_nolineales[[var_defecto]] <- list(
          modelo_lineal = modelo_lineal,
          modelo_splines = modelo_splines,
          modelo_poli = modelo_poli,
          r2_lineal = r2_lineal,
          r2_splines = r2_splines,
          r2_poli = r2_poli,
          p_splines = p_splines,
          p_poli = p_poli,
          mejor_modelo = mejor_modelo
        )
        
      }, error = function(e) {
        cat("  Error ajustando modelos para", var_defecto, ":", e$message, "\n")
      })
    }
  }
  
  # ==========================================
  # ANALISIS POR SEGMENTOS TEMPORALES
  # ==========================================
  
  cat("ANALISIS POR SEGMENTOS TEMPORALES\n")
  cat("==================================\n")
  
  # Crear segmentos por día de semana y hora
  datos_analisis$dia_semana <- weekdays(datos_analisis$FH_hora)
  datos_analisis$hora_dia <- hour(datos_analisis$FH_hora)
  datos_analisis$segmento_temporal <- paste0(datos_analisis$dia_semana, "_", 
                                           ifelse(datos_analisis$hora_dia < 12, "AM", "PM"))
  
  segmentos_temporales <- unique(datos_analisis$segmento_temporal)
  analisis_segmentos <- list()
  
  for(segmento in segmentos_temporales) {
    datos_segmento <- datos_analisis[datos_analisis$segmento_temporal == segmento, ]
    
    if(nrow(datos_segmento) >= 10) {
      cat("Analizando segmento:", segmento, "(", nrow(datos_segmento), "obs )\n")
      
      segmento_cors <- list()
      for(var_defecto in vars_defecto) {
        if(var_defecto %in% names(datos_segmento)) {
          cor_seg <- cor(datos_segmento[[var_defecto]], datos_segmento$RECHAZO, use = "complete.obs")
          segmento_cors[[var_defecto]] <- cor_seg
          cat("  ", var_defecto, ":", sprintf("%.3f", cor_seg), "\n")
        }
      }
      
      analisis_segmentos[[segmento]] <- segmento_cors
    }
  }
  
  # ==========================================
  # GUARDAR RESULTADOS
  # ==========================================
  
  timestamp <- crear_timestamp()
  archivo_resultado <- file.path(DIRECTORIO_RESULTADOS, 
                                paste0("analisis_estirado_avanzado_", gsub("\\.csv$", "", nombre_archivo), 
                                      "_", timestamp, ".txt"))
  
  sink(archivo_resultado)
  
  cat("ANALISIS ESTIRADO AVANZADO CON LAGS\n")
  cat("===================================\n")
  cat("Archivo:", nombre_archivo, "\n")
  cat("Fecha:", timestamp, "\n")
  cat("Observaciones analizadas:", nrow(datos_analisis), "\n\n")
  
  cat("CORRELACIONES CON LAGS TEMPORALES:\n")
  cat("===================================\n")
  for(defecto in names(correlaciones_lags)) {
    cat("Defecto:", defecto, "\n")
    cors <- correlaciones_lags[[defecto]]
    cat("  Simultaneo:  ", sprintf("%6.3f", cors$simultaneo), "\n")
    cat("  Lag 1h:      ", sprintf("%6.3f", cors$lag1h), "\n")
    cat("  Lag 2h:      ", sprintf("%6.3f", cors$lag2h), "\n")
    cat("  Lag 4h:      ", sprintf("%6.3f", cors$lag4h), "\n")
    cat("  Lag 8h:      ", sprintf("%6.3f", cors$lag8h), "\n")
    cat("  Promedio 2h: ", sprintf("%6.3f", cors$promedio_2h), "\n")
    cat("  Promedio 4h: ", sprintf("%6.3f", cors$promedio_4h), "\n")
    cat("  Tendencia:   ", sprintf("%6.3f", cors$tendencia), "\n")
    cat("  Volatilidad: ", sprintf("%6.3f", cors$volatilidad), "\n")
    cat("  MEJOR LAG:   ", cors$mejor_lag, "(", sprintf("%.3f", cors$mejor_correlacion), ")\n\n")
  }
  
  cat("ANALISIS NO LINEAL:\n")
  cat("===================\n")
  for(defecto in names(modelos_nolineales)) {
    cat("Defecto:", defecto, "\n")
    modelos <- modelos_nolineales[[defecto]]
    cat("  R² Lineal:    ", sprintf("%.4f", modelos$r2_lineal), "\n")
    cat("  R² Splines:   ", sprintf("%.4f", modelos$r2_splines), "(p=", sprintf("%.4f", modelos$p_splines), ")\n")
    cat("  R² Polinomial:", sprintf("%.4f", modelos$r2_poli), "(p=", sprintf("%.4f", modelos$p_poli), ")\n")
    cat("  MEJOR MODELO: ", modelos$mejor_modelo, "\n\n")
  }
  
  cat("CORRELACIONES POR SEGMENTO TEMPORAL:\n")
  cat("====================================\n")
  for(segmento in names(analisis_segmentos)) {
    cat("Segmento:", segmento, "\n")
    for(defecto in names(analisis_segmentos[[segmento]])) {
      cor_val <- analisis_segmentos[[segmento]][[defecto]]
      cat("  ", defecto, ":", sprintf("%.3f", cor_val), "\n")
    }
    cat("\n")
  }
  
  sink()
  
  cat("Análisis estirado avanzado completado\n")
  cat("Resultados guardados:", basename(archivo_resultado), "\n")
  
  # Devolver resultados
  return(list(
    datos_analisis = datos_analisis,
    correlaciones_lags = correlaciones_lags,
    modelos_nolineales = modelos_nolineales,
    segmentos_temporales = analisis_segmentos
  ))
}

cat("MODULO DE ANALISIS ESTIRADO AVANZADO OPERATIVO\n")
cat("==============================================\n")
cat("Funcionalidades incluidas:\n")
cat("- Análisis con lags temporales (1h, 2h, 4h, 8h)\n")
cat("- Detección de efectos retardados del estirado\n")
cat("- Modelos no lineales (splines, polinomiales)\n")
cat("- Análisis de tendencias y volatilidad\n")
cat("- Segmentación temporal avanzada\n")
cat("- Identificación del lag óptimo por defecto\n")