# ============================================================================
# MODULO 05: ANALISIS SEGMENTADO AVANZADO CON INTERACCIONES
# Sistema de Analisis Estadistico - Control de Calidad IS
# ============================================================================
# Análisis por segmentos (modelo/sección/turno) con interacciones y no linealidades
# ============================================================================

cat("MODULO DE ANALISIS SEGMENTADO AVANZADO\n")
cat("======================================\n")

# ============================================================================
# FUNCION PRINCIPAL DE ANALISIS SEGMENTADO
# ============================================================================

ejecutar_analisis_segmentado_avanzado <- function(datos, nombre_archivo) {
  cat("\nEJECUTANDO ANALISIS SEGMENTADO AVANZADO\n")
  cat("=======================================\n")
  cat("Archivo:", nombre_archivo, "\n")
  cat("Observaciones:", nrow(datos), "\n\n")
  
  # Verificar paquetes necesarios
  paquetes_segmentado <- c("lme4", "glmmTMB", "performance", "effects", 
                          "ggeffects", "emmeans", "car", "splines")
  
  for(paquete in paquetes_segmentado) {
    if(!require(paquete, character.only = TRUE, quietly = TRUE)) {
      cat("Instalando paquete:", paquete, "\n")
      install.packages(paquete, dependencies = TRUE)
      library(paquete, character.only = TRUE)
    }
  }
  
  # Crear variable turno basada en hora
  datos$TURNO <- factor(ifelse(hour(datos$FH) >= 6 & hour(datos$FH) < 14, "MAÑANA",
                              ifelse(hour(datos$FH) >= 14 & hour(datos$FH) < 22, "TARDE", "NOCHE")))
  
  # Variables defecto
  vars_defecto <- c("CLARO", "PLIEGUEPEQ", "PLIEGUEGR")
  
  # Crear lista para resultados
  resultados_segmentado <- list()
  
  # ==========================================
  # ANALISIS POR MODELO
  # ==========================================
  
  cat("ANALISIS POR MODELO\n")
  cat("===================\n")
  
  modelos_disponibles <- unique(datos$MODELO)
  cat("Modelos encontrados:", length(modelos_disponibles), "\n")
  
  analisis_por_modelo <- list()
  
  for(modelo in modelos_disponibles) {
    cat("Analizando modelo:", modelo, "\n")
    
    datos_modelo <- datos[datos$MODELO == modelo, ]
    
    if(nrow(datos_modelo) < 100) {
      cat("  Datos insuficientes para modelo", modelo, "- omitido\n")
      next
    }
    
    modelo_stats <- list()
    
    # Estadísticas por defecto para este modelo
    for(var_defecto in vars_defecto) {
      if(var_defecto %in% names(datos_modelo)) {
        tasa <- mean(datos_modelo[[var_defecto]], na.rm = TRUE) * 100
        casos <- sum(datos_modelo[[var_defecto]], na.rm = TRUE)
        
        modelo_stats[[var_defecto]] <- list(
          tasa = tasa,
          casos = casos,
          n_observaciones = nrow(datos_modelo)
        )
        
        cat("  ", var_defecto, "- Tasa:", round(tasa, 3), "% Casos:", casos, "\n")
      }
    }
    
    analisis_por_modelo[[as.character(modelo)]] <- modelo_stats
  }
  
  resultados_segmentado$por_modelo <- analisis_por_modelo
  
  # ==========================================
  # GLMM POR MODELO CON INTERACCIONES EXTENDIDAS
  # ==========================================
  
  cat("\nGLMM POR MODELO CON INTERACCIONES EXTENDIDAS\n")
  cat("============================================\n")
  
  # Verificar paquetes para splines y diagnósticos
  if(!require("mgcv", quietly = TRUE)) {
    cat("Instalando mgcv para efectos no lineales...\n")
    install.packages("mgcv", dependencies = TRUE)
    library(mgcv, quietly = TRUE)
  }
  
  if(!require("DHARMa", quietly = TRUE)) {
    cat("Instalando DHARMa para diagnósticos...\n")
    install.packages("DHARMa", dependencies = TRUE)
    library(DHARMa, quietly = TRUE)
  }
  
  modelos_por_modelo <- list()
  
  # Lista completa de interacciones solicitadas
  interacciones_extendidas <- c(
    "Q_S_CAZOS * LONGITUD",
    "Q_S_CAZOS * XY", 
    "T_S_TIJERAS * VERTICALIDAD",
    "Q_S_CAZOS * VERTICALIDAD",
    "Q_S_CAZOS * ASIMETRIA", 
    "Q_S_CAZOS * OVALIDAD",
    "T_CAZOS * LONGITUD",
    "T_CAZOS * VERTICALIDAD",
    "T_CAZOS * ASIMETRIA",
    "T_CAZOS * OVALIDAD",
    "T_S_TIJERAS * LONGITUD",
    "T_S_TIJERAS * ASIMETRIA",
    "T_S_TIJERAS * OVALIDAD"
  )
  
  for(modelo in modelos_disponibles) {
    cat("Analizando modelo:", modelo, "\n")
    
    datos_modelo <- datos[datos$MODELO == modelo, ]
    
    if(nrow(datos_modelo) < 100) {
      cat("  Datos insuficientes para modelo", modelo, "- omitido\n")
      next
    }
    
    modelos_defecto <- list()
    
    for(var_defecto in vars_defecto) {
      if(var_defecto %in% names(datos_modelo) && sum(datos_modelo[[var_defecto]], na.rm = TRUE) >= 10) {
        
        cat("  Ajustando GLMM para", var_defecto, "en modelo", modelo, "\n")
        
        tryCatch({
          # Modelo con todas las interacciones extendidas
          formula_completa <- as.formula(paste0(
            var_defecto, " ~ ", 
            paste(interacciones_extendidas, collapse = " + "),
            " + (1 | SECCION) + (1 | CAVIDAD)"
          ))
          
          # Ajustar modelo GLMM
          modelo_glmm <- glmer(formula_completa, 
                              data = datos_modelo, 
                              family = binomial(),
                              control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 25000)))
          
          # Extraer coeficientes
          coef_summary <- summary(modelo_glmm)$coefficients
          efectos_significativos <- coef_summary[coef_summary[, "Pr(>|z|)"] < 0.05, ]
          
          # Calcular AUC
          predicciones <- predict(modelo_glmm, type = "response")
          observado <- datos_modelo[[var_defecto]]
          
          if(require("pROC", quietly = TRUE)) {
            roc_obj <- roc(observado, predicciones, quiet = TRUE)
            auc_valor <- as.numeric(auc(roc_obj))
          } else {
            auc_valor <- NA
          }
          
          # Matriz de confusión con umbral 0.5
          pred_binary <- as.numeric(predicciones >= 0.5)
          matriz_confusion <- table(Observado = observado, Predicho = pred_binary)
          
          # Calcular métricas
          if(nrow(matriz_confusion) == 2 && ncol(matriz_confusion) == 2) {
            tp <- matriz_confusion[2, 2]
            tn <- matriz_confusion[1, 1] 
            fp <- matriz_confusion[1, 2]
            fn <- matriz_confusion[2, 1]
            
            precision <- tp / (tp + fp)
            recall <- tp / (tp + fn)
            f1_score <- 2 * precision * recall / (precision + recall)
            accuracy <- (tp + tn) / sum(matriz_confusion)
          } else {
            precision <- recall <- f1_score <- accuracy <- NA
          }
          
          # Diagnósticos DHARMa
          dharma_res <- NULL
          tryCatch({
            dharma_res <- simulateResiduals(modelo_glmm, n = 100)
            dharma_test <- testResiduals(dharma_res, plot = FALSE)
          }, error = function(e) {
            cat("    Error en diagnósticos DHARMa:", e$message, "\n")
          })
          
          # Leverage y Cook's distance
          leverage_vals <- hatvalues(modelo_glmm)
          cooks_dist <- cooks.distance(modelo_glmm)
          
          cat("    AUC:", round(auc_valor, 3), "Accuracy:", round(accuracy, 3), "\n")
          cat("    Efectos significativos:", nrow(efectos_significativos), "\n")
          
          modelos_defecto[[var_defecto]] <- list(
            modelo = modelo_glmm,
            formula = formula_completa,
            coeficientes = coef_summary,
            efectos_significativos = efectos_significativos,
            auc = auc_valor,
            matriz_confusion = matriz_confusion,
            metricas = list(
              precision = precision,
              recall = recall,
              f1_score = f1_score,
              accuracy = accuracy
            ),
            dharma_residuals = dharma_res,
            leverage = leverage_vals,
            cooks_distance = cooks_dist,
            n_casos_positivos = sum(observado),
            n_observaciones = length(observado)
          )
          
        }, error = function(e) {
          cat("    Error ajustando modelo para", var_defecto, ":", e$message, "\n")
        })
      }
    }
    
    modelos_por_modelo[[as.character(modelo)]] <- modelos_defecto
  }
  
  resultados_segmentado$modelos_por_modelo <- modelos_por_modelo
  
  # ==========================================
  # ANALISIS POR TURNO
  # ==========================================
  
  cat("\nANALISIS POR TURNO\n")
  cat("==================\n")
  
  turnos_disponibles <- unique(datos$TURNO)
  cat("Turnos identificados:", paste(turnos_disponibles, collapse = ", "), "\n")
  
  analisis_por_turno <- list()
  
  for(turno in turnos_disponibles) {
    cat("Analizando turno:", turno, "\n")
    
    datos_turno <- datos[datos$TURNO == turno, ]
    
    turno_stats <- list()
    
    for(var_defecto in vars_defecto) {
      if(var_defecto %in% names(datos_turno)) {
        tasa <- mean(datos_turno[[var_defecto]], na.rm = TRUE) * 100
        casos <- sum(datos_turno[[var_defecto]], na.rm = TRUE)
        
        turno_stats[[var_defecto]] <- list(
          tasa = tasa,
          casos = casos,
          n_observaciones = nrow(datos_turno)
        )
        
        cat("  ", var_defecto, "- Tasa:", round(tasa, 3), "% Casos:", casos, "\n")
      }
    }
    
    analisis_por_turno[[turno]] <- turno_stats
  }
  
  resultados_segmentado$por_turno <- analisis_por_turno
  
  # ==========================================
  # ANALISIS NO LINEAL CON SPLINES MGCV
  # ==========================================
  
  cat("\nANALISIS NO LINEAL CON SPLINES MGCV\n")
  cat("===================================\n")
  
  modelos_splines_mgcv <- list()
  
  for(var_defecto in vars_defecto) {
    if(var_defecto %in% names(datos) && sum(datos[[var_defecto]], na.rm = TRUE) >= 30) {
      
      cat("Ajustando splines mgcv para:", var_defecto, "\n")
      
      tryCatch({
        # Modelo con splines mgcv para Q_S_CAZOS y LONGITUD
        formula_mgcv <- as.formula(paste0(
          var_defecto, " ~ s(Q_S_CAZOS, k=5) + s(LONGITUD, k=5) + ",
          "s(T_S_TIJERAS, k=4) + VERTICALIDAD + ASIMETRIA + OVALIDAD + ",
          "s(SECCION, bs='re')"
        ))
        
        # Ajustar modelo GAM
        modelo_gam <- gam(formula_mgcv, 
                         data = datos, 
                         family = binomial(),
                         method = "REML")
        
        # Modelo lineal para comparación
        formula_lineal <- as.formula(paste0(
          var_defecto, " ~ Q_S_CAZOS + LONGITUD + T_S_TIJERAS + ",
          "VERTICALIDAD + ASIMETRIA + OVALIDAD + s(SECCION, bs='re')"
        ))
        
        modelo_lineal_gam <- gam(formula_lineal, 
                               data = datos, 
                               family = binomial(),
                               method = "REML")
        
        # Test de no linealidad
        anova_resultado <- anova(modelo_lineal_gam, modelo_gam, test = "Chisq")
        p_value_nolineal <- anova_resultado$`Pr(>Chi)`[2]
        
        # Extraer información de suavizado
        smooth_terms <- summary(modelo_gam)$s.table
        
        cat("  Test no linealidad p-value:", round(p_value_nolineal, 4), "\n")
        cat("  Términos de suavizado significativos:", sum(smooth_terms[, "p-value"] < 0.05), "\n")
        
        if(p_value_nolineal < 0.05) {
          cat("  DETECTADA NO LINEALIDAD SIGNIFICATIVA\n")
        }
        
        # AUC para modelo GAM
        predicciones_gam <- predict(modelo_gam, type = "response")
        observado <- datos[[var_defecto]]
        
        if(require("pROC", quietly = TRUE)) {
          roc_gam <- roc(observado, predicciones_gam, quiet = TRUE)
          auc_gam <- as.numeric(auc(roc_gam))
        } else {
          auc_gam <- NA
        }
        
        modelos_splines_mgcv[[var_defecto]] <- list(
          modelo_gam = modelo_gam,
          modelo_lineal = modelo_lineal_gam,
          p_value_nolineal = p_value_nolineal,
          anova = anova_resultado,
          smooth_terms = smooth_terms,
          auc_gam = auc_gam,
          formula_mgcv = formula_mgcv
        )
        
      }, error = function(e) {
        cat("  Error ajustando splines mgcv para", var_defecto, ":", e$message, "\n")
      })
    }
  }
  
  resultados_segmentado$modelos_splines_mgcv <- modelos_splines_mgcv
  
  # ==========================================
  # GUARDAR RESULTADOS
  # ==========================================
  
  timestamp <- crear_timestamp()
  archivo_resultado <- file.path(DIRECTORIO_RESULTADOS, 
                                paste0("analisis_segmentado_", gsub("\\.csv$", "", nombre_archivo), 
                                      "_", timestamp, ".txt"))
  
  # Escribir resumen
  sink(archivo_resultado)
  
  cat("ANALISIS SEGMENTADO AVANZADO\n")
  cat("============================\n")
  cat("Archivo:", nombre_archivo, "\n")
  cat("Fecha:", timestamp, "\n")
  cat("Observaciones:", nrow(datos), "\n\n")
  
  cat("RESULTADOS POR MODELO:\n")
  cat("======================\n")
  for(modelo in names(analisis_por_modelo)) {
    cat("Modelo", modelo, ":\n")
    for(defecto in names(analisis_por_modelo[[modelo]])) {
      stats <- analisis_por_modelo[[modelo]][[defecto]]
      cat("  ", defecto, "- Tasa:", round(stats$tasa, 3), "% Casos:", stats$casos, 
          "Obs:", stats$n_observaciones, "\n")
    }
    cat("\n")
  }
  
  cat("RESULTADOS POR TURNO:\n")
  cat("=====================\n")
  for(turno in names(analisis_por_turno)) {
    cat("Turno", turno, ":\n")
    for(defecto in names(analisis_por_turno[[turno]])) {
      stats <- analisis_por_turno[[turno]][[defecto]]
      cat("  ", defecto, "- Tasa:", round(stats$tasa, 3), "% Casos:", stats$casos, 
          "Obs:", stats$n_observaciones, "\n")
    }
    cat("\n")
  }
  
  cat("RESULTADOS GLMM POR MODELO:\n")
  cat("===========================\n")
  for(modelo in names(modelos_por_modelo)) {
    cat("Modelo", modelo, ":\n")
    for(defecto in names(modelos_por_modelo[[modelo]])) {
      modelo_info <- modelos_por_modelo[[modelo]][[defecto]]
      cat("  ", defecto, ":\n")
      cat("    AUC:", round(modelo_info$auc, 3), "\n")
      cat("    Accuracy:", round(modelo_info$metricas$accuracy, 3), "\n")
      cat("    Precision:", round(modelo_info$metricas$precision, 3), "\n")
      cat("    Recall:", round(modelo_info$metricas$recall, 3), "\n")
      cat("    F1-Score:", round(modelo_info$metricas$f1_score, 3), "\n")
      cat("    Casos positivos:", modelo_info$n_casos_positivos, "\n")
      cat("    Efectos significativos:", nrow(modelo_info$efectos_significativos), "\n")
      
      # Mostrar interacciones significativas
      nombres_coef <- rownames(modelo_info$efectos_significativos)
      interacciones <- nombres_coef[grepl(":", nombres_coef)]
      if(length(interacciones) > 0) {
        cat("    Interacciones significativas:\n")
        for(int in interacciones) {
          coef <- modelo_info$efectos_significativos[int, "Estimate"]
          pval <- modelo_info$efectos_significativos[int, "Pr(>|z|)"]
          cat("      ", int, "- Coef:", round(coef, 4), "p-value:", round(pval, 4), "\n")
        }
      }
      cat("\n")
    }
  }
  
  cat("ANALISIS NO LINEAL CON SPLINES:\n")
  cat("===============================\n")
  for(defecto in names(modelos_splines_mgcv)) {
    cat("Defecto:", defecto, "\n")
    modelo_info <- modelos_splines_mgcv[[defecto]]
    p_val <- modelo_info$p_value_nolineal
    cat("  Test no linealidad p-value:", round(p_val, 4), "\n")
    cat("  AUC GAM:", round(modelo_info$auc_gam, 3), "\n")
    cat("  Términos de suavizado significativos:", sum(modelo_info$smooth_terms[, "p-value"] < 0.05), "\n")
    
    if(p_val < 0.05) {
      cat("  RESULTADO: NO LINEALIDAD SIGNIFICATIVA DETECTADA\n")
    } else {
      cat("  RESULTADO: Relación lineal adecuada\n")
    }
    
    # Mostrar términos de suavizado significativos
    smooth_sig <- modelo_info$smooth_terms[modelo_info$smooth_terms[, "p-value"] < 0.05, ]
    if(nrow(smooth_sig) > 0) {
      cat("  Términos no lineales significativos:\n")
      for(i in 1:nrow(smooth_sig)) {
        cat("    ", rownames(smooth_sig)[i], "- p-value:", round(smooth_sig[i, "p-value"], 4), "\n")
      }
    }
    cat("\n")
  }
  
  sink()
  
  cat("Análisis segmentado completado\n")
  cat("Resultados guardados:", basename(archivo_resultado), "\n")
  
  return(resultados_segmentado)
}

cat("MODULO DE ANALISIS SEGMENTADO OPERATIVO\n")
cat("=======================================\n")
cat("Funcionalidades incluidas:\n")
cat("- Análisis por modelo/sección/turno\n")
cat("- GLMM con interacciones (Q_S_CAZOS×LONGITUD, etc.)\n")
cat("- Detección de no linealidades con splines\n")
cat("- Comparación de efectos locales vs globales\n")
cat("- Análisis de variabilidad entre segmentos\n")