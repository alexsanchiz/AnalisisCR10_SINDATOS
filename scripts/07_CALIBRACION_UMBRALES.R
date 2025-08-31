# ============================================================================
# MODULO 07: CALIBRACION DE UMBRALES Y CURVAS DE CONFIABILIDAD
# Sistema de Analisis Estadistico - Control de Calidad IS
# ============================================================================
# Calibración de umbrales óptimos y análisis de reliability por tipo de defecto
# ============================================================================

cat("MODULO DE CALIBRACION DE UMBRALES\n")
cat("=================================\n")

# ============================================================================
# FUNCION PRINCIPAL DE CALIBRACION DE UMBRALES
# ============================================================================

ejecutar_calibracion_umbrales <- function(resultado_analisis, datos, nombre_archivo) {
  cat("\nEJECUTANDO CALIBRACION DE UMBRALES\n")
  cat("==================================\n")
  cat("Archivo:", nombre_archivo, "\n")
  cat("Observaciones:", nrow(datos), "\n\n")
  
  # Verificar paquetes necesarios
  paquetes_calibracion <- c("pROC", "caret", "OptimalCutpoints", "CalibrationCurves")
  
  for(paquete in paquetes_calibracion) {
    if(!require(paquete, character.only = TRUE, quietly = TRUE)) {
      cat("Instalando paquete:", paquete, "\n")
      tryCatch({
        install.packages(paquete, dependencies = TRUE)
        library(paquete, character.only = TRUE)
      }, error = function(e) {
        cat("No se pudo instalar", paquete, "- continuando sin él\n")
      })
    }
  }
  
  # Variables defecto disponibles
  vars_defecto <- names(resultado_analisis$roc)
  
  if(length(vars_defecto) == 0) {
    cat("No hay modelos ROC disponibles para calibración\n")
    return(NULL)
  }
  
  resultados_calibracion <- list()
  
  # ==========================================
  # CALIBRACION POR CADA DEFECTO
  # ==========================================
  
  for(var_defecto in vars_defecto) {
    cat("CALIBRANDO UMBRALES PARA:", var_defecto, "\n")
    cat("=======================================\n")
    
    roc_info <- resultado_analisis$roc[[var_defecto]]
    
    if(is.null(roc_info$predicciones) || is.null(roc_info$observado)) {
      cat("Datos insuficientes para calibración de", var_defecto, "\n\n")
      next
    }
    
    predicciones <- roc_info$predicciones
    observado <- roc_info$observado
    
    # Verificar datos válidos
    if(length(predicciones) != length(observado) || 
       sum(!is.na(predicciones)) < 10 || 
       length(unique(observado)) < 2) {
      cat("Datos inadecuados para calibración de", var_defecto, "\n\n")
      next
    }
    
    calibracion_defecto <- list()
    
    # ==========================================
    # CALCULAR MULTIPLES UMBRALES ÓPTIMOS
    # ==========================================
    
    cat("Calculando umbrales óptimos...\n")
    
    # Crear ROC para cálculos
    roc_obj <- roc(observado, predicciones, quiet = TRUE)
    
    # 1. Umbral Youden (Sensibilidad + Especificidad - 1)
    coords_youden <- coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))
    umbral_youden <- coords_youden$threshold
    sens_youden <- coords_youden$sensitivity
    spec_youden <- coords_youden$specificity
    
    cat("  Umbral Youden:", sprintf("%.4f", umbral_youden), 
        "- Sens:", sprintf("%.3f", sens_youden), 
        "- Spec:", sprintf("%.3f", spec_youden), "\n")
    
    # 2. Umbral que maximiza precisión (PPV)
    prevalencia <- mean(observado)
    thresholds <- seq(0.01, 0.99, by = 0.01)
    
    precision_values <- sapply(thresholds, function(th) {
      pred_binary <- as.numeric(predicciones >= th)
      if(sum(pred_binary) == 0) return(0)
      tp <- sum(pred_binary == 1 & observado == 1)
      fp <- sum(pred_binary == 1 & observado == 0)
      if(tp + fp == 0) return(0)
      return(tp / (tp + fp))
    })
    
    umbral_precision <- thresholds[which.max(precision_values)]
    max_precision <- max(precision_values, na.rm = TRUE)
    
    cat("  Umbral Max Precision:", sprintf("%.4f", umbral_precision), 
        "- Precision:", sprintf("%.3f", max_precision), "\n")
    
    # 3. Umbral que maximiza F1-Score
    f1_values <- sapply(thresholds, function(th) {
      pred_binary <- as.numeric(predicciones >= th)
      tp <- sum(pred_binary == 1 & observado == 1)
      fp <- sum(pred_binary == 1 & observado == 0)
      fn <- sum(pred_binary == 0 & observado == 1)
      
      if(tp == 0) return(0)
      precision <- tp / (tp + fp)
      recall <- tp / (tp + fn)
      if(precision + recall == 0) return(0)
      return(2 * precision * recall / (precision + recall))
    })
    
    umbral_f1 <- thresholds[which.max(f1_values)]
    max_f1 <- max(f1_values, na.rm = TRUE)
    
    cat("  Umbral Max F1:", sprintf("%.4f", umbral_f1), 
        "- F1-Score:", sprintf("%.3f", max_f1), "\n")
    
    # 4. Umbral específico para defectos raros (maximiza recall manteniendo precision > 0.1)
    recall_values <- sapply(thresholds, function(th) {
      pred_binary <- as.numeric(predicciones >= th)
      tp <- sum(pred_binary == 1 & observado == 1)
      fn <- sum(pred_binary == 0 & observado == 1)
      if(tp + fn == 0) return(0)
      return(tp / (tp + fn))
    })
    
    # Umbrales con precision > 0.1
    precision_ok <- precision_values >= 0.1
    if(sum(precision_ok, na.rm = TRUE) > 0) {
      umbral_recall_constrained <- thresholds[precision_ok][which.max(recall_values[precision_ok])]
      max_recall_constrained <- max(recall_values[precision_ok], na.rm = TRUE)
    } else {
      umbral_recall_constrained <- umbral_youden
      max_recall_constrained <- recall_values[which(thresholds == umbral_youden)]
    }
    
    cat("  Umbral Max Recall (Prec>0.1):", sprintf("%.4f", umbral_recall_constrained), 
        "- Recall:", sprintf("%.3f", max_recall_constrained), "\n")
    
    # ==========================================
    # ANALISIS DE CALIBRACION (RELIABILITY)
    # ==========================================
    
    cat("Analizando calibración del modelo...\n")
    
    # Crear bins de probabilidad para análisis de calibración
    n_bins <- min(10, floor(sum(observado) / 2))  # Ajustar bins según casos positivos
    n_bins <- max(3, n_bins)  # Mínimo 3 bins
    
    # Crear bins
    prob_bins <- cut(predicciones, breaks = n_bins, include.lowest = TRUE)
    
    # Calcular calibración por bin
    calibracion_bins <- data.frame(
      bin = levels(prob_bins),
      n_obs = as.numeric(table(prob_bins)),
      prob_media = tapply(predicciones, prob_bins, mean, na.rm = TRUE),
      tasa_observada = tapply(observado, prob_bins, mean, na.rm = TRUE),
      n_positivos = tapply(observado, prob_bins, sum, na.rm = TRUE)
    )
    
    # Eliminar bins vacíos
    calibracion_bins <- calibracion_bins[!is.na(calibracion_bins$prob_media), ]
    
    # Calcular Brier Score
    brier_score <- mean((predicciones - observado)^2, na.rm = TRUE)
    
    # Calcular estadístico de Hosmer-Lemeshow
    if(nrow(calibracion_bins) >= 3) {
      chi_square_hl <- sum((calibracion_bins$n_positivos - 
                           calibracion_bins$n_obs * calibracion_bins$prob_media)^2 / 
                          (calibracion_bins$n_obs * calibracion_bins$prob_media * 
                           (1 - calibracion_bins$prob_media)), na.rm = TRUE)
      
      df_hl <- nrow(calibracion_bins) - 2
      p_value_hl <- 1 - pchisq(chi_square_hl, df_hl)
    } else {
      chi_square_hl <- NA
      p_value_hl <- NA
    }
    
    cat("  Brier Score:", sprintf("%.4f", brier_score), "\n")
    if(!is.na(p_value_hl)) {
      cat("  Hosmer-Lemeshow p-value:", sprintf("%.4f", p_value_hl), "\n")
      if(p_value_hl > 0.05) {
        cat("  RESULTADO: Modelo bien calibrado (p > 0.05)\n")
      } else {
        cat("  RESULTADO: Modelo mal calibrado (p <= 0.05)\n")
      }
    }
    
    # ==========================================
    # RECOMENDACION DE UMBRAL SEGÚN CASO DE USO
    # ==========================================
    
    cat("Recomendaciones de umbral según objetivo...\n")
    
    casos_positivos <- sum(observado)
    
    if(casos_positivos < 10) {
      umbral_recomendado <- umbral_recall_constrained
      objetivo_recomendado <- "Detección (pocos casos)"
      cat("  RECOMENDADO: Umbral detección -", sprintf("%.4f", umbral_recomendado), "\n")
      cat("  MOTIVO: Muy pocos casos positivos, priorizar detección\n")
    } else if(casos_positivos < 50) {
      umbral_recomendado <- umbral_f1
      objetivo_recomendado <- "Balance F1"
      cat("  RECOMENDADO: Umbral F1 -", sprintf("%.4f", umbral_recomendado), "\n")
      cat("  MOTIVO: Casos limitados, balancear precision y recall\n")
    } else {
      umbral_recomendado <- umbral_youden
      objetivo_recomendado <- "Balance Youden"
      cat("  RECOMENDADO: Umbral Youden -", sprintf("%.4f", umbral_recomendado), "\n")
      cat("  MOTIVO: Suficientes casos, optimizar balance general\n")
    }
    
    # ==========================================
    # GUARDAR RESULTADOS DEL DEFECTO
    # ==========================================
    
    calibracion_defecto <- list(
      umbrales = list(
        youden = umbral_youden,
        precision = umbral_precision,
        f1 = umbral_f1,
        recall_constrained = umbral_recall_constrained,
        recomendado = umbral_recomendado
      ),
      metricas_umbrales = list(
        youden = list(sens = sens_youden, spec = spec_youden),
        precision = list(precision = max_precision),
        f1 = list(f1 = max_f1),
        recall_constrained = list(recall = max_recall_constrained)
      ),
      calibracion = list(
        bins = calibracion_bins,
        brier_score = brier_score,
        hosmer_lemeshow_pvalue = p_value_hl,
        bien_calibrado = is.na(p_value_hl) || p_value_hl > 0.05
      ),
      recomendacion = list(
        umbral = umbral_recomendado,
        objetivo = objetivo_recomendado,
        casos_positivos = casos_positivos
      )
    )
    
    resultados_calibracion[[var_defecto]] <- calibracion_defecto
    
    cat("\n")
  }
  
  # ==========================================
  # GUARDAR RESULTADOS
  # ==========================================
  
  timestamp <- crear_timestamp()
  archivo_resultado <- file.path(DIRECTORIO_RESULTADOS, 
                                paste0("calibracion_umbrales_", gsub("\\.csv$", "", nombre_archivo), 
                                      "_", timestamp, ".txt"))
  
  sink(archivo_resultado)
  
  cat("CALIBRACION DE UMBRALES Y RELIABILITY\n")
  cat("=====================================\n")
  cat("Archivo:", nombre_archivo, "\n")
  cat("Fecha:", timestamp, "\n")
  cat("Observaciones:", nrow(datos), "\n\n")
  
  for(defecto in names(resultados_calibracion)) {
    cat("DEFECTO:", defecto, "\n")
    cat("=================\n")
    
    cal <- resultados_calibracion[[defecto]]
    
    cat("UMBRALES ÓPTIMOS:\n")
    cat("Youden:           ", sprintf("%.4f", cal$umbrales$youden), "\n")
    cat("Max Precision:    ", sprintf("%.4f", cal$umbrales$precision), "\n")
    cat("Max F1:           ", sprintf("%.4f", cal$umbrales$f1), "\n")
    cat("Max Recall (Prec>0.1):", sprintf("%.4f", cal$umbrales$recall_constrained), "\n")
    cat("RECOMENDADO:      ", sprintf("%.4f", cal$umbrales$recomendado), 
        "(", cal$recomendacion$objetivo, ")\n\n")
    
    cat("MÉTRICAS EN UMBRALES:\n")
    cat("Youden - Sens:", sprintf("%.3f", cal$metricas_umbrales$youden$sens), 
        "Spec:", sprintf("%.3f", cal$metricas_umbrales$youden$spec), "\n")
    cat("Precision - Max:", sprintf("%.3f", cal$metricas_umbrales$precision$precision), "\n")
    cat("F1 - Max:", sprintf("%.3f", cal$metricas_umbrales$f1$f1), "\n")
    cat("Recall Constrained:", sprintf("%.3f", cal$metricas_umbrales$recall_constrained$recall), "\n\n")
    
    cat("ANÁLISIS DE CALIBRACIÓN:\n")
    cat("Brier Score:", sprintf("%.4f", cal$calibracion$brier_score), "\n")
    if(!is.na(cal$calibracion$hosmer_lemeshow_pvalue)) {
      cat("Hosmer-Lemeshow p-value:", sprintf("%.4f", cal$calibracion$hosmer_lemeshow_pvalue), "\n")
      cat("Estado calibración:", ifelse(cal$calibracion$bien_calibrado, "BIEN CALIBRADO", "MAL CALIBRADO"), "\n")
    }
    cat("Casos positivos:", cal$recomendacion$casos_positivos, "\n\n")
    
    cat("BINS DE CALIBRACIÓN:\n")
    for(i in 1:nrow(cal$calibracion$bins)) {
      bin_info <- cal$calibracion$bins[i, ]
      cat("Bin", i, "- Prob media:", sprintf("%.3f", bin_info$prob_media), 
          "Tasa obs:", sprintf("%.3f", bin_info$tasa_observada), 
          "N:", bin_info$n_obs, "\n")
    }
    cat("\n")
  }
  
  sink()
  
  cat("Calibración de umbrales completada\n")
  cat("Resultados guardados:", basename(archivo_resultado), "\n")
  
  return(resultados_calibracion)
}

cat("MODULO DE CALIBRACION OPERATIVO\n")
cat("===============================\n")
cat("Funcionalidades incluidas:\n")
cat("- Múltiples umbrales óptimos (Youden, Precision, F1, Recall)\n")
cat("- Curvas de reliability y calibración\n")
cat("- Análisis Hosmer-Lemeshow\n")
cat("- Brier Score para evaluación\n")
cat("- Recomendaciones específicas por caso de uso\n")
cat("- Ajuste automático para defectos raros\n")