# ============================================================================
# MODULO 02: ANALISIS ESTADISTICO AVANZADO COMPLETO
# Sistema de Analisis Estadistico - Control de Calidad IS
# ============================================================================

cat("MODULO DE ANALISIS ESTADISTICO AVANZADO\n")
cat("========================================\n")

# ============================================================================
# INSTALACION Y CARGA DE LIBRERIAS AVANZADAS
# ============================================================================

instalar_paquetes_avanzados <- function() {
  paquetes_necesarios <- c(
    "lme4",        # GLMM
    "glmmTMB",     # GLMM avanzados
    "car",         # VIF y diagnosticos
    "performance", # Diagnosticos de modelos
    "effects",     # Efectos marginales
    "emmeans",     # Medias marginales
    "pROC",        # Curvas ROC
    "caret",       # Confusion matrix
    "corrplot",    # Matrices correlacion
    "plotROC",     # Graficos ROC
    "ggeffects",   # Efectos para ggplot
    "DHARMa",      # Residuos de modelos mixtos
    "MuMIn",       # Seleccion de modelos
    "sjPlot",      # Tablas de modelos
    "broom.mixed", # Resultados de modelos mixtos
    "lattice"      # Graficos adicionales
  )
  
  cat("Instalando/verificando paquetes avanzados...\n")
  for(paquete in paquetes_necesarios) {
    if(!require(paquete, character.only = TRUE, quietly = TRUE)) {
      cat("Instalando", paquete, "...\n")
      tryCatch({
        install.packages(paquete, dependencies = TRUE, quiet = TRUE)
        library(paquete, character.only = TRUE)
        cat("Paquete", paquete, "instalado y cargado\n")
      }, error = function(e) {
        cat("Error instalando", paquete, ":", e$message, "\n")
      })
    }
  }
  cat("Verificacion de paquetes completada\n")
}

# ============================================================================
# FUNCION PRINCIPAL DE ANALISIS AVANZADO
# ============================================================================

ejecutar_analisis_completo_local <- function(nombre_archivo) {
  cat("\nEJECUTANDO ANALISIS AVANZADO PARA:", nombre_archivo, "\n")
  cat("", rep("=", nchar(nombre_archivo) + 32), "\n")
  
  # Instalar paquetes necesarios
  instalar_paquetes_avanzados()
  
  # ==========================================
  # CARGAR DATOS
  # ==========================================
  
  datos <- cargar_datos_local(nombre_archivo)
  
  if(is.null(datos)) {
    cat("ERROR: No se pudieron cargar los datos\n")
    return(NULL)
  }
  
  cat("Datos cargados:", nrow(datos), "observaciones\n")
  cat("Variables disponibles:", ncol(datos), "\n")
  
  # ==========================================
  # VALIDACION DE VARIABLES DEFECTO
  # ==========================================
  
  cat("\nVALIDACION DE VARIABLES DEFECTO\n")
  cat("===============================\n")
  
  # Verificar variables de defecto binarias
  variables_defecto <- c("CLARO", "PLIEGUEPEQ", "PLIEGUEGR", "DEFECTOS")
  variables_defecto_presentes <- variables_defecto[variables_defecto %in% names(datos)]
  
  if(length(variables_defecto_presentes) == 0) {
    cat("ERROR: No se encontraron variables de defecto\n")
    return(NULL)
  }
  
  cat("Variables de defecto encontradas:", paste(variables_defecto_presentes, collapse = ", "), "\n")
  
  # Validar que sean binarias (0/1)
  for(var in variables_defecto_presentes) {
    valores_unicos <- unique(datos[[var]][!is.na(datos[[var]])])
    if(!all(valores_unicos %in% c(0, 1))) {
      cat("ADVERTENCIA:", var, "no es binaria. Convirtiendo...\n")
      datos[[var]] <- ifelse(datos[[var]] > 0, 1, 0)
    }
  }
  
  # Verificar logica de DEFECTOS
  if(all(c("CLARO", "PLIEGUEPEQ", "PLIEGUEGR", "DEFECTOS") %in% names(datos))) {
    cat("Verificando logica DEFECTOS = 1 si cualquier defecto individual = 1...\n")
    
    defectos_calculados <- ifelse(
      datos$CLARO == 1 | datos$PLIEGUEPEQ == 1 | datos$PLIEGUEGR == 1, 1, 0
    )
    
    discrepancias <- sum(datos$DEFECTOS != defectos_calculados, na.rm = TRUE)
    if(discrepancias > 0) {
      cat("ADVERTENCIA:", discrepancias, "discrepancias en logica DEFECTOS. Corrigiendo...\n")
      datos$DEFECTOS <- defectos_calculados
    } else {
      cat("Logica DEFECTOS verificada correctamente\n")
    }
  }
  
  # ==========================================
  # ESTADISTICAS DESCRIPTIVAS AVANZADAS
  # ==========================================
  
  cat("\nESTADISTICAS DESCRIPTIVAS AVANZADAS\n")
  cat("===================================\n")
  
  resultados <- list()
  
  # Tasas de defecto por tipo
  tasas_defecto <- sapply(variables_defecto_presentes, function(var) {
    tasa <- mean(datos[[var]], na.rm = TRUE) * 100
    cat("Tasa", var, ":", round(tasa, 3), "%\n")
    return(tasa)
  })
  
  resultados$tasas_defecto <- tasas_defecto
  
  # Estadisticas por seccion si disponible
  if("SECCION" %in% names(datos)) {
    cat("\nAnalisis por SECCION:\n")
    tryCatch({
      for(var in variables_defecto_presentes) {
        tasas_seccion <- aggregate(datos[[var]], by = list(SECCION = datos$SECCION), 
                                  FUN = function(x) mean(x, na.rm = TRUE) * 100)
        names(tasas_seccion)[2] <- paste0("Tasa_", var)
        cat("Tasas", var, "por seccion:\n")
        print(tasas_seccion)
      }
    }, error = function(e) {
      cat("Error en analisis por seccion:", e$message, "\n")
    })
  }
  
  # ==========================================
  # ANALISIS DE COLINEALIDAD Y VIF
  # ==========================================
  
  cat("\nANALISIS DE COLINEALIDAD Y VIF\n")
  cat("==============================\n")
  
  # Preparar variables numericas para VIF
  variables_numericas <- sapply(datos, is.numeric)
  datos_numericos <- datos[, variables_numericas, drop = FALSE]
  
  # Excluir variables de defecto del analisis VIF (son variables respuesta)
  datos_predictores <- datos_numericos[, !names(datos_numericos) %in% variables_defecto, drop = FALSE]
  
  if(ncol(datos_predictores) >= 2) {
    tryCatch({
      # Crear modelo lineal temporal para VIF
      formula_temp <- as.formula(paste("DEFECTOS ~", paste(names(datos_predictores), collapse = " + ")))
      modelo_temp <- lm(formula_temp, data = datos)
      
      if(require(car, quietly = TRUE)) {
        vif_valores <- car::vif(modelo_temp)
        cat("Factores de Inflacion de Varianza (VIF):\n")
        
        if(is.matrix(vif_valores)) {
          vif_valores <- vif_valores[, 1]
        }
        
        for(i in 1:length(vif_valores)) {
          var_name <- names(vif_valores)[i]
          vif_val <- vif_valores[i]
          status <- ifelse(vif_val > 10, "ALTA COLINEALIDAD", 
                          ifelse(vif_val > 5, "MODERADA", "ACEPTABLE"))
          cat("  ", var_name, ":", round(vif_val, 3), "(", status, ")\n")
        }
        
        resultados$vif <- vif_valores
      }
    }, error = function(e) {
      cat("Error calculando VIF:", e$message, "\n")
    })
  }
  
  # ==========================================
  # MODELOS LINEALES GENERALIZADOS MIXTOS (GLMM)
  # ==========================================
  
  cat("\nMODELOS LINEALES GENERALIZADOS MIXTOS (GLMM)\n")
  cat("============================================\n")
  
  resultados_glmm <- list()
  
  if(require(lme4, quietly = TRUE) && "SECCION" %in% names(datos)) {
    
    # Preparar variables predictoras (excluir defectos y categoricas)
    vars_excluir <- c(variables_defecto, "FH", "MODELO", "SECCION", "CAVIDAD")
    variables_predictoras <- names(datos)[!names(datos) %in% vars_excluir & sapply(datos, is.numeric)]
    
    if(length(variables_predictoras) >= 1) {
      
      # Modelo para cada tipo de defecto
      for(var_defecto in variables_defecto_presentes[variables_defecto_presentes != "DEFECTOS"]) {
        cat("\nAjustando GLMM para", var_defecto, "...\n")
        
        tryCatch({
          # Formula del modelo con efectos aleatorios por SECCION
          formula_glmm <- as.formula(paste(
            var_defecto, "~", 
            paste(head(variables_predictoras, 5), collapse = " + "),
            "+ (1|SECCION)"
          ))
          
          cat("Formula:", deparse(formula_glmm), "\n")
          
          # Normalizar variables predictoras para mejorar convergencia
          datos_norm <- datos
          for(var in variables_predictoras[1:5]) {
            if(is.numeric(datos_norm[[var]])) {
              datos_norm[[var]] <- scale(datos_norm[[var]])[,1]
            }
          }
          
          # Ajustar modelo con datos normalizados
          modelo_glmm <- glmer(formula_glmm, data = datos_norm, family = binomial(),
                              control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6),
                                                   tolPwrss = 1e-3, check.conv.grad = .makeCC("warning", 0.05)))
          
          cat("Modelo", var_defecto, "ajustado exitosamente\n")
          
          # Extraer resultados
          summary_modelo <- summary(modelo_glmm)
          coeficientes <- fixef(modelo_glmm)
          efectos_aleatorios <- ranef(modelo_glmm)
          
          # Guardar resultados
          resultados_glmm[[var_defecto]] <- list(
            modelo = modelo_glmm,
            resumen = summary_modelo,
            coeficientes = coeficientes,
            efectos_aleatorios = efectos_aleatorios,
            formula = formula_glmm
          )
          
          # Mostrar coeficientes principales
          cat("Coeficientes significativos:\n")
          coef_table <- summary_modelo$coefficients
          hay_significativos <- FALSE
          for(i in 1:nrow(coef_table)) {
            if(coef_table[i, 4] < 0.05) {  # p-value < 0.05
              cat("  ", rownames(coef_table)[i], ": coef =", round(coef_table[i, 1], 4),
                  ", p =", round(coef_table[i, 4], 4), "\n")
              hay_significativos <- TRUE
            }
          }
          if(!hay_significativos) {
            cat("  Ningun coeficiente significativo (p < 0.05)\n")
          }
          
        }, error = function(e) {
          cat("Error ajustando GLMM para", var_defecto, ":", e$message, "\n")
        })
      }
    } else {
      cat("Variables predictoras insuficientes para GLMM\n")
    }
  } else {
    cat("lme4 no disponible o variable SECCION faltante\n")
  }
  
  resultados$glmm <- resultados_glmm
  
  # ==========================================
  # ANALISIS ROC Y MATRICES DE CONFUSION
  # ==========================================
  
  cat("\nANALISIS ROC Y MATRICES DE CONFUSION\n")
  cat("====================================\n")
  
  resultados_roc <- list()
  
  if(require(pROC, quietly = TRUE) && require(caret, quietly = TRUE)) {
    
    for(var_defecto in variables_defecto_presentes) {
      if(var_defecto %in% names(resultados_glmm)) {
        
        tryCatch({
          modelo <- resultados_glmm[[var_defecto]]$modelo
          
          # Predicciones con umbral ajustado para datos desbalanceados
          predicciones_prob <- predict(modelo, type = "response")
          
          # Calcular umbral optimo basado en prevalencia
          prevalencia <- mean(datos[[var_defecto]], na.rm = TRUE)
          umbral_optimo <- ifelse(prevalencia < 0.05, prevalencia * 2, 0.5)
          
          predicciones_bin <- ifelse(predicciones_prob > umbral_optimo, 1, 0)
          
          # Valores observados
          observados <- datos[[var_defecto]][!is.na(predicciones_prob)]
          pred_prob_clean <- predicciones_prob[!is.na(predicciones_prob)]
          pred_bin_clean <- predicciones_bin[!is.na(predicciones_prob)]
          
          if(length(observados) > 10 && length(unique(observados)) == 2) {
            
            # Curva ROC
            roc_obj <- roc(observados, pred_prob_clean, quiet = TRUE)
            auc_valor <- auc(roc_obj)
            
            cat("ROC para", var_defecto, "- AUC:", round(auc_valor, 3), "\n")
            
            # Matriz de confusion
            matriz_conf <- confusionMatrix(factor(pred_bin_clean), factor(observados))
            
            cat("Matriz de confusion para", var_defecto, ":\n")
            print(matriz_conf$table)
            cat("Accuracy:", round(matriz_conf$overall['Accuracy'], 3), "\n")
            cat("Sensitivity:", round(matriz_conf$byClass['Sensitivity'], 3), "\n")
            cat("Specificity:", round(matriz_conf$byClass['Specificity'], 3), "\n")
            
            # Guardar resultados
            resultados_roc[[var_defecto]] <- list(
              roc = roc_obj,
              auc = auc_valor,
              matriz_confusion = matriz_conf,
              predicciones_prob = pred_prob_clean,
              observados = observados
            )
          }
          
        }, error = function(e) {
          cat("Error en analisis ROC para", var_defecto, ":", e$message, "\n")
        })
      }
    }
  }
  
  resultados$roc <- resultados_roc
  
  # ==========================================
  # ANALISIS DE RESIDUOS DE PEARSON
  # ==========================================
  
  cat("\nANALISIS DE RESIDUOS DE PEARSON\n")
  cat("===============================\n")
  
  resultados_residuos <- list()
  
  if(require(DHARMa, quietly = TRUE)) {
    for(var_defecto in names(resultados_glmm)) {
      tryCatch({
        modelo <- resultados_glmm[[var_defecto]]$modelo
        
        # Residuos de Pearson
        residuos_pearson <- residuals(modelo, type = "pearson")
        
        # Residuos escalados DHARMa
        simulaciones <- simulateResiduals(modelo, plot = FALSE)
        
        cat("Residuos para", var_defecto, ":\n")
        cat("  Media residuos Pearson:", round(mean(residuos_pearson, na.rm = TRUE), 4), "\n")
        cat("  SD residuos Pearson:", round(sd(residuos_pearson, na.rm = TRUE), 4), "\n")
        
        # Test de uniformidad DHARMa
        test_uniformidad <- testUniformity(simulaciones, plot = FALSE)
        cat("  Test uniformidad p-value:", round(test_uniformidad$p.value, 4), "\n")
        
        resultados_residuos[[var_defecto]] <- list(
          residuos_pearson = residuos_pearson,
          simulaciones_dharma = simulaciones,
          test_uniformidad = test_uniformidad
        )
        
      }, error = function(e) {
        cat("Error en residuos para", var_defecto, ":", e$message, "\n")
      })
    }
  }
  
  resultados$residuos <- resultados_residuos
  
  # ==========================================
  # EFECTOS MARGINALES
  # ==========================================
  
  cat("\nEFECTOS MARGINALES\n")
  cat("==================\n")
  
  resultados_efectos <- list()
  
  if(require(ggeffects, quietly = TRUE)) {
    for(var_defecto in names(resultados_glmm)) {
      tryCatch({
        modelo <- resultados_glmm[[var_defecto]]$modelo
        
        # Obtener nombres de predictores
        predictores <- all.vars(resultados_glmm[[var_defecto]]$formula)[-1]
        predictores <- predictores[!grepl("\\|", predictores)]  # Remover efectos aleatorios
        
        if(length(predictores) > 0) {
          # Efectos marginales para primer predictor
          primer_predictor <- predictores[1]
          efectos_marg <- ggpredict(modelo, terms = primer_predictor)
          
          cat("Efectos marginales para", var_defecto, "~", primer_predictor, "calculados\n")
          
          resultados_efectos[[var_defecto]] <- list(
            efectos_principales = efectos_marg,
            predictor_principal = primer_predictor
          )
        }
        
      }, error = function(e) {
        cat("Error calculando efectos marginales para", var_defecto, ":", e$message, "\n")
      })
    }
  }
  
  resultados$efectos_marginales <- resultados_efectos
  
  # ==========================================
  # VALIDACION CRUZADA PARA ROBUSTEZ DEL MODELO
  # ==========================================
  
  cat("\nVALIDACION CRUZADA - ROBUSTEZ DEL MODELO\n")
  cat("========================================\n")
  
  resultados_cv <- list()
  
  if(length(resultados_glmm) > 0 && nrow(datos) > 100) {
    
    # Parametros de validacion cruzada
    k_folds <- 5  # 5-fold cross-validation
    cat("Configuracion: ", k_folds, "-fold cross-validation\n")
    
    # Crear indices de folds
    set.seed(42)  # Reproducibilidad
    n_obs <- nrow(datos)
    fold_indices <- sample(rep(1:k_folds, length.out = n_obs))
    
    for(var_defecto in names(resultados_glmm)) {
      if(var_defecto %in% variables_defecto_presentes[variables_defecto_presentes != "DEFECTOS"]) {
        
        cat("\nValidacion cruzada para", var_defecto, "...\n")
        
        # Vectores para almacenar resultados CV
        auc_folds <- numeric(k_folds)
        accuracy_folds <- numeric(k_folds)
        sensitivity_folds <- numeric(k_folds)
        specificity_folds <- numeric(k_folds)
        
        # Metricas adicionales para datos desbalanceados
        precision_folds <- numeric(k_folds)
        recall_folds <- numeric(k_folds)
        f1_folds <- numeric(k_folds)
        balanced_accuracy_folds <- numeric(k_folds)
        
        # Metricas con umbrales optimizados
        auc_optimized_folds <- numeric(k_folds)
        precision_optimized_folds <- numeric(k_folds)
        recall_optimized_folds <- numeric(k_folds)
        
        # Preparar datos normalizados
        datos_cv <- datos
        vars_predictoras <- c("Q_S_CAZOS", "Q_S_TIJERAS", "Q_CAZOS", "T_S_CAZOS", "T_S_TIJERAS")
        
        # Normalizar variables predictoras
        for(var in vars_predictoras) {
          if(var %in% names(datos_cv) && is.numeric(datos_cv[[var]])) {
            datos_cv[[var]] <- scale(datos_cv[[var]])[,1]
          }
        }
        
        valid_folds <- 0
        
        for(fold in 1:k_folds) {
          tryCatch({
            # Dividir en train/test
            test_indices <- which(fold_indices == fold)
            train_indices <- which(fold_indices != fold)
            
            datos_train <- datos_cv[train_indices, ]
            datos_test <- datos_cv[test_indices, ]
            
            # Verificar que hay suficientes datos en cada fold
            if(sum(datos_train[[var_defecto]], na.rm = TRUE) < 2 || 
               sum(datos_test[[var_defecto]], na.rm = TRUE) < 1) {
              next  # Skip este fold si no hay suficientes casos positivos
            }
            
            # Ajustar modelo en datos de entrenamiento
            formula_cv <- as.formula(paste(
              var_defecto, "~", 
              paste(head(vars_predictoras, 5), collapse = " + "),
              "+ (1|SECCION)"
            ))
            
            modelo_cv <- glmer(formula_cv, data = datos_train, family = binomial(),
                              control = glmerControl(optimizer = "bobyqa", 
                                                   optCtrl = list(maxfun = 1e5),
                                                   tolPwrss = 1e-3))
            
            # Predicciones en datos de test
            pred_prob_test <- predict(modelo_cv, newdata = datos_test, type = "response")
            
            # Calcular umbral optimo basado en prevalencia del training set
            prevalencia_train <- mean(datos_train[[var_defecto]], na.rm = TRUE)
            umbral_cv <- ifelse(prevalencia_train < 0.05, prevalencia_train * 2, 0.5)
            
            # Umbral adicional optimizado para deteccion (muy conservador)
            umbral_deteccion <- ifelse(prevalencia_train < 0.01, 0.1, 
                                     ifelse(prevalencia_train < 0.05, 0.2, 0.3))
            
            pred_bin_test <- ifelse(pred_prob_test > umbral_cv, 1, 0)
            pred_bin_optimized <- ifelse(pred_prob_test > umbral_deteccion, 1, 0)
            
            # Valores observados en test
            obs_test <- datos_test[[var_defecto]]
            
            # Limpiar NAs
            valid_indices <- !is.na(pred_prob_test) & !is.na(obs_test)
            pred_prob_clean <- pred_prob_test[valid_indices]
            pred_bin_clean <- pred_bin_test[valid_indices]
            obs_clean <- obs_test[valid_indices]
            
            if(length(obs_clean) > 5 && length(unique(obs_clean)) == 2) {
              # AUC
              if(require(pROC, quietly = TRUE)) {
                roc_cv <- roc(obs_clean, pred_prob_clean, quiet = TRUE)
                auc_folds[fold] <- auc(roc_cv)
                auc_optimized_folds[fold] <- auc(roc_cv)  # Mismo AUC, no depende del umbral
              }
              
              # Metricas de clasificacion con umbral estandar
              if(require(caret, quietly = TRUE)) {
                cm_cv <- confusionMatrix(factor(pred_bin_clean), factor(obs_clean))
                accuracy_folds[fold] <- cm_cv$overall['Accuracy']
                sensitivity_folds[fold] <- cm_cv$byClass['Sensitivity']
                specificity_folds[fold] <- cm_cv$byClass['Specificity']
                
                # Calcular Precision, Recall, F1 manualmente para mayor control
                tp <- sum(pred_bin_clean == 1 & obs_clean == 1, na.rm = TRUE)
                fp <- sum(pred_bin_clean == 1 & obs_clean == 0, na.rm = TRUE)
                fn <- sum(pred_bin_clean == 0 & obs_clean == 1, na.rm = TRUE)
                tn <- sum(pred_bin_clean == 0 & obs_clean == 0, na.rm = TRUE)
                
                # Precision = TP/(TP+FP)
                precision_val <- ifelse(tp + fp > 0, tp / (tp + fp), 0)
                precision_folds[fold] <- precision_val
                
                # Recall = TP/(TP+FN) = Sensitivity
                recall_val <- ifelse(tp + fn > 0, tp / (tp + fn), 0)
                recall_folds[fold] <- recall_val
                
                # F1-Score = 2 * (Precision * Recall) / (Precision + Recall)
                f1_val <- ifelse(precision_val + recall_val > 0, 
                               2 * (precision_val * recall_val) / (precision_val + recall_val), 0)
                f1_folds[fold] <- f1_val
                
                # Balanced Accuracy = (Sensitivity + Specificity) / 2
                sens_val <- ifelse(!is.na(cm_cv$byClass['Sensitivity']), cm_cv$byClass['Sensitivity'], 0)
                spec_val <- ifelse(!is.na(cm_cv$byClass['Specificity']), cm_cv$byClass['Specificity'], 0)
                balanced_accuracy_folds[fold] <- (sens_val + spec_val) / 2
              }
              
              # Metricas con umbral optimizado para deteccion
              pred_bin_optimized_clean <- pred_bin_optimized[valid_indices]
              
              if(length(unique(pred_bin_optimized_clean)) > 1) {
                tp_opt <- sum(pred_bin_optimized_clean == 1 & obs_clean == 1, na.rm = TRUE)
                fp_opt <- sum(pred_bin_optimized_clean == 1 & obs_clean == 0, na.rm = TRUE)
                fn_opt <- sum(pred_bin_optimized_clean == 0 & obs_clean == 1, na.rm = TRUE)
                
                # Precision optimizada para deteccion
                precision_opt <- ifelse(tp_opt + fp_opt > 0, tp_opt / (tp_opt + fp_opt), 0)
                precision_optimized_folds[fold] <- precision_opt
                
                # Recall optimizado para deteccion
                recall_opt <- ifelse(tp_opt + fn_opt > 0, tp_opt / (tp_opt + fn_opt), 0)
                recall_optimized_folds[fold] <- recall_opt
              }
              
              valid_folds <- valid_folds + 1
            }
            
          }, error = function(e) {
            # Silenciar errores de folds individuales
          })
        }
        
        # Calcular estadisticas de CV
        if(valid_folds >= 3) {
          # Remover NAs de los vectores
          auc_valid <- auc_folds[!is.na(auc_folds) & auc_folds > 0]
          acc_valid <- accuracy_folds[!is.na(accuracy_folds) & accuracy_folds > 0]
          sens_valid <- sensitivity_folds[!is.na(sensitivity_folds)]
          spec_valid <- specificity_folds[!is.na(specificity_folds)]
          
          # Nuevas metricas para datos desbalanceados
          precision_valid <- precision_folds[!is.na(precision_folds)]
          recall_valid <- recall_folds[!is.na(recall_folds)]
          f1_valid <- f1_folds[!is.na(f1_folds)]
          balanced_acc_valid <- balanced_accuracy_folds[!is.na(balanced_accuracy_folds)]
          
          # Metricas optimizadas
          precision_opt_valid <- precision_optimized_folds[!is.na(precision_optimized_folds)]
          recall_opt_valid <- recall_optimized_folds[!is.na(recall_optimized_folds)]
          
          cat("Folds validos:", valid_folds, "de", k_folds, "\n")
          
          # Metricas estandar
          if(length(auc_valid) >= 2) {
            cat("AUC CV - Media:", round(mean(auc_valid), 3), 
                "± SD:", round(sd(auc_valid), 3), "\n")
          }
          
          if(length(acc_valid) >= 2) {
            cat("Accuracy CV - Media:", round(mean(acc_valid), 3), 
                "± SD:", round(sd(acc_valid), 3), "\n")
          }
          
          # Metricas especializadas para datos desbalanceados
          if(length(precision_valid) >= 2) {
            cat("Precision CV - Media:", round(mean(precision_valid), 3), 
                "± SD:", round(sd(precision_valid), 3), "\n")
          }
          
          if(length(recall_valid) >= 2) {
            cat("Recall CV - Media:", round(mean(recall_valid), 3), 
                "± SD:", round(sd(recall_valid), 3), "\n")
          }
          
          if(length(f1_valid) >= 2) {
            cat("F1-Score CV - Media:", round(mean(f1_valid), 3), 
                "± SD:", round(sd(f1_valid), 3), "\n")
          }
          
          if(length(balanced_acc_valid) >= 2) {
            cat("Balanced Accuracy CV - Media:", round(mean(balanced_acc_valid), 3), 
                "± SD:", round(sd(balanced_acc_valid), 3), "\n")
          }
          
          # Metricas con umbral optimizado para deteccion
          if(length(precision_opt_valid) >= 2) {
            cat("Precision Optimizada CV - Media:", round(mean(precision_opt_valid), 3), 
                "± SD:", round(sd(precision_opt_valid), 3), "\n")
          }
          
          if(length(recall_opt_valid) >= 2) {
            cat("Recall Optimizado CV - Media:", round(mean(recall_opt_valid), 3), 
                "± SD:", round(sd(recall_opt_valid), 3), "\n")
          }
          
          # Advertencias para datos muy desbalanceados
          n_casos_positivos <- sum(datos[[var_defecto]], na.rm = TRUE)
          if(n_casos_positivos < 30) {
            cat("ADVERTENCIA: Solo", n_casos_positivos, "casos positivos - metricas poco confiables\n")
          }
          
          if(n_casos_positivos < 10) {
            cat("CRITICO: Menos de 10 casos positivos - considere Leave-One-Out CV\n")
          }
          
          # Guardar resultados completos
          resultados_cv[[var_defecto]] <- list(
            k_folds = k_folds,
            folds_validos = valid_folds,
            casos_positivos = n_casos_positivos,
            
            # Metricas estandar
            auc_folds = auc_valid,
            accuracy_folds = acc_valid,
            sensitivity_folds = sens_valid,
            specificity_folds = spec_valid,
            auc_mean = ifelse(length(auc_valid) >= 2, mean(auc_valid), NA),
            auc_sd = ifelse(length(auc_valid) >= 2, sd(auc_valid), NA),
            accuracy_mean = ifelse(length(acc_valid) >= 2, mean(acc_valid), NA),
            accuracy_sd = ifelse(length(acc_valid) >= 2, sd(acc_valid), NA),
            
            # Metricas para datos desbalanceados
            precision_folds = precision_valid,
            recall_folds = recall_valid,
            f1_folds = f1_valid,
            balanced_accuracy_folds = balanced_acc_valid,
            precision_mean = ifelse(length(precision_valid) >= 2, mean(precision_valid), NA),
            precision_sd = ifelse(length(precision_valid) >= 2, sd(precision_valid), NA),
            recall_mean = ifelse(length(recall_valid) >= 2, mean(recall_valid), NA),
            recall_sd = ifelse(length(recall_valid) >= 2, sd(recall_valid), NA),
            f1_mean = ifelse(length(f1_valid) >= 2, mean(f1_valid), NA),
            f1_sd = ifelse(length(f1_valid) >= 2, sd(f1_valid), NA),
            balanced_accuracy_mean = ifelse(length(balanced_acc_valid) >= 2, mean(balanced_acc_valid), NA),
            balanced_accuracy_sd = ifelse(length(balanced_acc_valid) >= 2, sd(balanced_acc_valid), NA),
            
            # Metricas optimizadas para deteccion
            precision_optimized_folds = precision_opt_valid,
            recall_optimized_folds = recall_opt_valid,
            precision_optimized_mean = ifelse(length(precision_opt_valid) >= 2, mean(precision_opt_valid), NA),
            precision_optimized_sd = ifelse(length(precision_opt_valid) >= 2, sd(precision_opt_valid), NA),
            recall_optimized_mean = ifelse(length(recall_opt_valid) >= 2, mean(recall_opt_valid), NA),
            recall_optimized_sd = ifelse(length(recall_opt_valid) >= 2, sd(recall_opt_valid), NA)
          )
          
        } else {
          cat("Validacion cruzada no viable para", var_defecto, 
              "- datos insuficientes (", valid_folds, "folds validos)\n")
        }
      }
    }
    
    if(length(resultados_cv) > 0) {
      cat("\nRESUMEN VALIDACION CRUZADA:\n")
      cat("===========================\n")
      for(var_def in names(resultados_cv)) {
        res_cv <- resultados_cv[[var_def]]
        cat(var_def, "(", res_cv$casos_positivos, "casos positivos):\n")
        
        # Metricas estandar
        cat("  AUC: ", 
            ifelse(is.na(res_cv$auc_mean), "N/A", 
                   paste0(round(res_cv$auc_mean, 3), " ± ", round(res_cv$auc_sd, 3))), "\n")
        
        # Metricas especializadas para datos desbalanceados
        cat("  Precision: ", 
            ifelse(is.na(res_cv$precision_mean), "N/A", 
                   paste0(round(res_cv$precision_mean, 3), " ± ", round(res_cv$precision_sd, 3))), "\n")
        cat("  Recall: ", 
            ifelse(is.na(res_cv$recall_mean), "N/A", 
                   paste0(round(res_cv$recall_mean, 3), " ± ", round(res_cv$recall_sd, 3))), "\n")
        cat("  F1-Score: ", 
            ifelse(is.na(res_cv$f1_mean), "N/A", 
                   paste0(round(res_cv$f1_mean, 3), " ± ", round(res_cv$f1_sd, 3))), "\n")
        cat("  Balanced Accuracy: ", 
            ifelse(is.na(res_cv$balanced_accuracy_mean), "N/A", 
                   paste0(round(res_cv$balanced_accuracy_mean, 3), " ± ", round(res_cv$balanced_accuracy_sd, 3))), "\n")
        
        # Metricas optimizadas para deteccion
        cat("  Recall Optimizado: ", 
            ifelse(is.na(res_cv$recall_optimized_mean), "N/A", 
                   paste0(round(res_cv$recall_optimized_mean, 3), " ± ", round(res_cv$recall_optimized_sd, 3))), "\n")
        
        # Evaluacion de confiabilidad
        if(res_cv$casos_positivos < 10) {
          cat("  CONFIABILIDAD: MUY BAJA (< 10 casos)\n")
        } else if(res_cv$casos_positivos < 30) {
          cat("  CONFIABILIDAD: BAJA (< 30 casos)\n")
        } else if(res_cv$casos_positivos < 100) {
          cat("  CONFIABILIDAD: MEDIA (< 100 casos)\n")
        } else {
          cat("  CONFIABILIDAD: ALTA (>= 100 casos)\n")
        }
        
        cat("\n")
      }
    }
    
  } else {
    cat("Validacion cruzada omitida - datos insuficientes o modelos no disponibles\n")
  }
  
  resultados$validacion_cruzada <- resultados_cv
  
  # ==========================================
  # VALIDACION ALTERNATIVA: LEAVE-ONE-OUT Y BOOTSTRAP
  # ==========================================
  
  cat("\nVALIDACION ALTERNATIVA PARA CASOS RAROS\n")
  cat("=======================================\n")
  
  resultados_validacion_alternativa <- list()
  
  if(length(resultados_glmm) > 0) {
    
    for(var_defecto in names(resultados_glmm)) {
      if(var_defecto %in% variables_defecto_presentes[variables_defecto_presentes != "DEFECTOS"]) {
        
        n_casos_positivos <- sum(datos[[var_defecto]], na.rm = TRUE)
        
        # Solo para casos muy raros (< 30 casos positivos)
        if(n_casos_positivos < 30) {
          
          cat("\nValidacion alternativa para", var_defecto, "(", n_casos_positivos, "casos)...\n")
          
          # Preparar datos
          datos_alt <- datos
          vars_predictoras <- c("Q_S_CAZOS", "Q_S_TIJERAS", "Q_CAZOS", "T_S_CAZOS", "T_S_TIJERAS")
          
          # Normalizar variables predictoras
          for(var in vars_predictoras) {
            if(var %in% names(datos_alt) && is.numeric(datos_alt[[var]])) {
              datos_alt[[var]] <- scale(datos_alt[[var]])[,1]
            }
          }
          
          # ==========================================
          # LEAVE-ONE-OUT CROSS-VALIDATION (LOOCV)
          # ==========================================
          
          if(n_casos_positivos >= 5 && n_casos_positivos <= 20) {
            
            cat("Ejecutando Leave-One-Out CV...\n")
            
            # Obtener indices de casos positivos
            indices_positivos <- which(datos_alt[[var_defecto]] == 1)
            
            # Vectores para LOOCV
            loocv_predictions <- numeric(length(indices_positivos))
            loocv_probabilities <- numeric(length(indices_positivos))
            
            n_loocv_exitoso <- 0
            
            for(i in 1:length(indices_positivos)) {
              tryCatch({
                # Remover un caso positivo
                test_idx <- indices_positivos[i]
                train_data <- datos_alt[-test_idx, ]
                test_data <- datos_alt[test_idx, , drop = FALSE]
                
                # Verificar que queden casos positivos en training
                if(sum(train_data[[var_defecto]], na.rm = TRUE) >= 2) {
                  
                  # Ajustar modelo
                  formula_loocv <- as.formula(paste(
                    var_defecto, "~", 
                    paste(head(vars_predictoras, 5), collapse = " + "),
                    "+ (1|SECCION)"
                  ))
                  
                  modelo_loocv <- glmer(formula_loocv, data = train_data, family = binomial(),
                                       control = glmerControl(optimizer = "bobyqa", 
                                                            optCtrl = list(maxfun = 1e5),
                                                            tolPwrss = 1e-3))
                  
                  # Prediccion en caso de test
                  pred_prob_loocv <- predict(modelo_loocv, newdata = test_data, type = "response")
                  
                  if(!is.na(pred_prob_loocv)) {
                    loocv_probabilities[i] <- pred_prob_loocv
                    
                    # Umbral conservador para deteccion
                    umbral_loocv <- 0.1
                    loocv_predictions[i] <- ifelse(pred_prob_loocv > umbral_loocv, 1, 0)
                    
                    n_loocv_exitoso <- n_loocv_exitoso + 1
                  }
                }
                
              }, error = function(e) {
                # Silenciar errores individuales
              })
            }
            
            # Calcular metricas LOOCV
            if(n_loocv_exitoso >= 3) {
              
              # Casos exitosamente predichos
              predicciones_validas <- loocv_predictions[loocv_probabilities > 0]
              probabilidades_validas <- loocv_probabilities[loocv_probabilities > 0]
              
              # Recall en LOOCV (todos los casos de test son positivos)
              recall_loocv <- mean(predicciones_validas, na.rm = TRUE)
              probabilidad_media_loocv <- mean(probabilidades_validas, na.rm = TRUE)
              
              cat("LOOCV - Casos exitosos:", n_loocv_exitoso, "de", length(indices_positivos), "\n")
              cat("LOOCV - Recall:", round(recall_loocv, 3), "\n")
              cat("LOOCV - Probabilidad media:", round(probabilidad_media_loocv, 3), "\n")
              
              resultados_validacion_alternativa[[var_defecto]]$loocv <- list(
                casos_exitosos = n_loocv_exitoso,
                casos_totales = length(indices_positivos),
                recall = recall_loocv,
                probabilidad_media = probabilidad_media_loocv,
                probabilidades = probabilidades_validas
              )
            }
          }
          
          # ==========================================
          # BOOTSTRAP SAMPLING VALIDATION
          # ==========================================
          
          cat("Ejecutando Bootstrap Validation...\n")
          
          # Parametros bootstrap
          n_bootstrap <- 100
          bootstrap_recalls <- numeric(n_bootstrap)
          bootstrap_precisions <- numeric(n_bootstrap)
          bootstrap_f1s <- numeric(n_bootstrap)
          
          n_bootstrap_exitoso <- 0
          set.seed(42)
          
          for(b in 1:n_bootstrap) {
            tryCatch({
              # Sample con reemplazo, asegurando casos positivos
              n_obs <- nrow(datos_alt)
              
              # Estratificar: asegurar al menos algunos casos positivos
              indices_pos <- which(datos_alt[[var_defecto]] == 1)
              indices_neg <- which(datos_alt[[var_defecto]] == 0)
              
              # Sample indices
              n_pos_sample <- max(2, round(length(indices_pos) * 0.8))
              n_neg_sample <- round(length(indices_neg) * 0.2)
              
              sample_pos <- sample(indices_pos, n_pos_sample, replace = TRUE)
              sample_neg <- sample(indices_neg, n_neg_sample, replace = FALSE)
              
              train_indices <- c(sample_pos, sample_neg)
              test_indices <- setdiff(1:n_obs, train_indices)
              
              # Verificar que hay casos positivos en test
              if(length(test_indices) > 0 && sum(datos_alt[test_indices, var_defecto], na.rm = TRUE) >= 1) {
                
                train_boot <- datos_alt[train_indices, ]
                test_boot <- datos_alt[test_indices, ]
                
                # Ajustar modelo bootstrap
                formula_boot <- as.formula(paste(
                  var_defecto, "~", 
                  paste(head(vars_predictoras, 3), collapse = " + ")  # Modelo mas simple
                ))
                
                modelo_boot <- glm(formula_boot, data = train_boot, family = binomial())
                
                # Predicciones bootstrap
                pred_prob_boot <- predict(modelo_boot, newdata = test_boot, type = "response")
                pred_bin_boot <- ifelse(pred_prob_boot > 0.1, 1, 0)
                
                obs_boot <- test_boot[[var_defecto]]
                
                # Limpiar NAs
                valid_boot <- !is.na(pred_bin_boot) & !is.na(obs_boot)
                if(sum(valid_boot) > 0) {
                  pred_boot_clean <- pred_bin_boot[valid_boot]
                  obs_boot_clean <- obs_boot[valid_boot]
                  
                  # Calcular metricas
                  tp_boot <- sum(pred_boot_clean == 1 & obs_boot_clean == 1)
                  fp_boot <- sum(pred_boot_clean == 1 & obs_boot_clean == 0)
                  fn_boot <- sum(pred_boot_clean == 0 & obs_boot_clean == 1)
                  
                  if(tp_boot + fn_boot > 0) {
                    recall_boot <- tp_boot / (tp_boot + fn_boot)
                    bootstrap_recalls[b] <- recall_boot
                  }
                  
                  if(tp_boot + fp_boot > 0) {
                    precision_boot <- tp_boot / (tp_boot + fp_boot)
                    bootstrap_precisions[b] <- precision_boot
                  }
                  
                  if(recall_boot > 0 && precision_boot > 0) {
                    f1_boot <- 2 * (precision_boot * recall_boot) / (precision_boot + recall_boot)
                    bootstrap_f1s[b] <- f1_boot
                  }
                  
                  n_bootstrap_exitoso <- n_bootstrap_exitoso + 1
                }
              }
              
            }, error = function(e) {
              # Silenciar errores bootstrap
            })
          }
          
          # Estadisticas bootstrap
          if(n_bootstrap_exitoso >= 20) {
            recalls_validos <- bootstrap_recalls[bootstrap_recalls > 0]
            precisions_validos <- bootstrap_precisions[bootstrap_precisions > 0]
            f1s_validos <- bootstrap_f1s[bootstrap_f1s > 0]
            
            cat("Bootstrap - Iteraciones exitosas:", n_bootstrap_exitoso, "de", n_bootstrap, "\n")
            
            if(length(recalls_validos) >= 10) {
              cat("Bootstrap - Recall:", round(mean(recalls_validos), 3), 
                  "± SD:", round(sd(recalls_validos), 3), "\n")
            }
            
            if(length(precisions_validos) >= 10) {
              cat("Bootstrap - Precision:", round(mean(precisions_validos), 3), 
                  "± SD:", round(sd(precisions_validos), 3), "\n")
            }
            
            if(length(f1s_validos) >= 10) {
              cat("Bootstrap - F1:", round(mean(f1s_validos), 3), 
                  "± SD:", round(sd(f1s_validos), 3), "\n")
            }
            
            resultados_validacion_alternativa[[var_defecto]]$bootstrap <- list(
              iteraciones_exitosas = n_bootstrap_exitoso,
              iteraciones_totales = n_bootstrap,
              recall_mean = ifelse(length(recalls_validos) >= 10, mean(recalls_validos), NA),
              recall_sd = ifelse(length(recalls_validos) >= 10, sd(recalls_validos), NA),
              precision_mean = ifelse(length(precisions_validos) >= 10, mean(precisions_validos), NA),
              precision_sd = ifelse(length(precisions_validos) >= 10, sd(precisions_validos), NA),
              f1_mean = ifelse(length(f1s_validos) >= 10, mean(f1s_validos), NA),
              f1_sd = ifelse(length(f1s_validos) >= 10, sd(f1s_validos), NA)
            )
          }
          
        } else {
          cat("Validacion alternativa omitida para", var_defecto, "- suficientes casos para 5-fold CV\n")
        }
      }
    }
    
    if(length(resultados_validacion_alternativa) > 0) {
      cat("\nRESUMEN VALIDACION ALTERNATIVA:\n")
      cat("===============================\n")
      for(var_def in names(resultados_validacion_alternativa)) {
        cat(var_def, ":\n")
        
        if(!is.null(resultados_validacion_alternativa[[var_def]]$loocv)) {
          loocv_res <- resultados_validacion_alternativa[[var_def]]$loocv
          cat("  LOOCV - Recall:", round(loocv_res$recall, 3), 
              "(", loocv_res$casos_exitosos, "/", loocv_res$casos_totales, "casos)\n")
        }
        
        if(!is.null(resultados_validacion_alternativa[[var_def]]$bootstrap)) {
          boot_res <- resultados_validacion_alternativa[[var_def]]$bootstrap
          if(!is.na(boot_res$recall_mean)) {
            cat("  Bootstrap - Recall:", round(boot_res$recall_mean, 3), 
                "± SD:", round(boot_res$recall_sd, 3), "\n")
          }
          if(!is.na(boot_res$precision_mean)) {
            cat("  Bootstrap - Precision:", round(boot_res$precision_mean, 3), 
                "± SD:", round(boot_res$precision_sd, 3), "\n")
          }
        }
        
        cat("\n")
      }
    }
    
  } else {
    cat("Validacion alternativa omitida - modelos GLMM no disponibles\n")
  }
  
  resultados$validacion_alternativa <- resultados_validacion_alternativa
  
  # ==========================================
  # ANALISIS PCA AVANZADO
  # ==========================================
  
  cat("\nANALISIS PCA AVANZADO\n")
  cat("=====================\n")
  
  # Variables numericas excluyendo defectos
  vars_pca <- names(datos_predictores)
  if(length(vars_pca) >= 2) {
    datos_pca <- datos[, vars_pca, drop = FALSE]
    datos_pca_completos <- datos_pca[complete.cases(datos_pca), ]
    
    if(nrow(datos_pca_completos) > 10) {
      tryCatch({
        # PCA
        pca_resultado <- prcomp(datos_pca_completos, scale. = TRUE, center = TRUE)
        
        # Varianza explicada
        varianza_explicada <- summary(pca_resultado)$importance[2, ]
        varianza_acumulada <- summary(pca_resultado)$importance[3, ]
        
        cat("Componentes principales:\n")
        for(i in 1:min(5, length(varianza_explicada))) {
          cat("  PC", i, ":", round(varianza_explicada[i] * 100, 1), "% individual,",
              round(varianza_acumulada[i] * 100, 1), "% acumulada\n")
        }
        
        # Variables mas importantes en primeros componentes
        loadings_pc1 <- abs(pca_resultado$rotation[, 1])
        loadings_pc2 <- abs(pca_resultado$rotation[, 2])
        
        vars_importantes_pc1 <- names(sort(loadings_pc1, decreasing = TRUE))[1:3]
        vars_importantes_pc2 <- names(sort(loadings_pc2, decreasing = TRUE))[1:3]
        
        cat("Variables importantes PC1:", paste(vars_importantes_pc1, collapse = ", "), "\n")
        cat("Variables importantes PC2:", paste(vars_importantes_pc2, collapse = ", "), "\n")
        
        resultados$pca <- list(
          resultado = pca_resultado,
          varianza_explicada = varianza_explicada,
          varianza_acumulada = varianza_acumulada,
          variables_importantes_pc1 = vars_importantes_pc1,
          variables_importantes_pc2 = vars_importantes_pc2
        )
        
      }, error = function(e) {
        cat("Error en PCA:", e$message, "\n")
      })
    }
  }
  
  # ==========================================
  # MATRICES DE CORRELACION AVANZADAS
  # ==========================================
  
  cat("\nMATRICES DE CORRELACION AVANZADAS\n")
  cat("=================================\n")
  
  if(ncol(datos_numericos) >= 2) {
    # Correlacion de Pearson
    cor_pearson <- cor(datos_numericos, use = "complete.obs", method = "pearson")
    
    # Correlacion de Spearman
    cor_spearman <- cor(datos_numericos, use = "complete.obs", method = "spearman")
    
    cat("Matrices de correlacion calculadas (Pearson y Spearman)\n")
    
    # Correlaciones altas con defectos
    if("DEFECTOS" %in% names(datos_numericos)) {
      cor_con_defectos <- cor_pearson[, "DEFECTOS"]
      cor_altas <- cor_con_defectos[abs(cor_con_defectos) > 0.3 & names(cor_con_defectos) != "DEFECTOS"]
      
      if(length(cor_altas) > 0) {
        cat("Correlaciones altas (>0.3) con DEFECTOS:\n")
        for(i in 1:length(cor_altas)) {
          cat("  ", names(cor_altas)[i], ":", round(cor_altas[i], 3), "\n")
        }
      }
    }
    
    resultados$correlaciones <- list(
      pearson = cor_pearson,
      spearman = cor_spearman
    )
  }
  
  # ==========================================
  # GUARDAR RESULTADOS
  # ==========================================
  
  cat("\nGUARDANDO RESULTADOS\n")
  cat("====================\n")
  
  timestamp <- crear_timestamp()
  nombre_base <- gsub("\\.csv$", "", nombre_archivo)
  
  # Guardar resumen detallado
  archivo_resumen <- file.path(DIRECTORIO_RESULTADOS, 
                              paste0("analisis_avanzado_", nombre_base, "_", timestamp, ".txt"))
  
  sink(archivo_resumen)
  cat("ANALISIS ESTADISTICO AVANZADO\n")
  cat("=============================\n")
  cat("Archivo:", nombre_archivo, "\n")
  cat("Fecha:", Sys.time(), "\n")
  cat("Observaciones:", nrow(datos), "\n\n")
  
  cat("TASAS DE DEFECTO:\n")
  for(var in names(tasas_defecto)) {
    cat(var, ":", round(tasas_defecto[var], 3), "%\n")
  }
  cat("\n")
  
  if(length(resultados_glmm) > 0) {
    cat("MODELOS GLMM AJUSTADOS:\n")
    for(var in names(resultados_glmm)) {
      cat("\nModelo para", var, ":\n")
      cat("Formula:", deparse(resultados_glmm[[var]]$formula), "\n")
      coef_table <- summary(resultados_glmm[[var]]$modelo)$coefficients
      cat("Coeficientes significativos (p < 0.05):\n")
      for(i in 1:nrow(coef_table)) {
        if(coef_table[i, 4] < 0.05) {
          cat("  ", rownames(coef_table)[i], ": coef =", round(coef_table[i, 1], 4),
              ", p =", round(coef_table[i, 4], 4), "\n")
        }
      }
    }
  }
  
  if(length(resultados_roc) > 0) {
    cat("\nANALISIS ROC:\n")
    for(var in names(resultados_roc)) {
      cat(var, "- AUC:", round(resultados_roc[[var]]$auc, 3), "\n")
    }
  }
  
  if(!is.null(resultados$vif)) {
    cat("\nFACTORES VIF:\n")
    for(i in 1:length(resultados$vif)) {
      cat(names(resultados$vif)[i], ":", round(resultados$vif[i], 3), "\n")
    }
  }
  
  if(length(resultados_cv) > 0) {
    cat("\nVALIDACION CRUZADA (ROBUSTEZ):\n")
    for(var in names(resultados_cv)) {
      cv_info <- resultados_cv[[var]]
      cat("Modelo para", var, ":\n")
      cat("K-folds:", cv_info$k_folds, "| Folds validos:", cv_info$folds_validos, "\n")
      cat("Casos positivos:", cv_info$casos_positivos, "\n")
      
      if(!is.na(cv_info$auc_mean)) {
        cat("AUC CV: ", round(cv_info$auc_mean, 3), " ± ", round(cv_info$auc_sd, 3), "\n")
      }
      
      if(!is.na(cv_info$precision_mean)) {
        cat("Precision CV: ", round(cv_info$precision_mean, 3), " ± ", round(cv_info$precision_sd, 3), "\n")
      }
      
      if(!is.na(cv_info$recall_mean)) {
        cat("Recall CV: ", round(cv_info$recall_mean, 3), " ± ", round(cv_info$recall_sd, 3), "\n")
      }
      
      if(!is.na(cv_info$f1_mean)) {
        cat("F1-Score CV: ", round(cv_info$f1_mean, 3), " ± ", round(cv_info$f1_sd, 3), "\n")
      }
      
      cat("\n")
    }
  }
  
  if(length(resultados_validacion_alternativa) > 0) {
    cat("\nVALIDACION ALTERNATIVA (CASOS RAROS):\n")
    for(var in names(resultados_validacion_alternativa)) {
      cat("Modelo para", var, ":\n")
      
      if(!is.null(resultados_validacion_alternativa[[var]]$loocv)) {
        loocv_info <- resultados_validacion_alternativa[[var]]$loocv
        cat("LOOCV - Recall: ", round(loocv_info$recall, 3), "\n")
      }
      
      if(!is.null(resultados_validacion_alternativa[[var]]$bootstrap)) {
        boot_info <- resultados_validacion_alternativa[[var]]$bootstrap
        if(!is.na(boot_info$recall_mean)) {
          cat("Bootstrap - Recall: ", round(boot_info$recall_mean, 3), " ± ", round(boot_info$recall_sd, 3), "\n")
        }
        if(!is.na(boot_info$precision_mean)) {
          cat("Bootstrap - Precision: ", round(boot_info$precision_mean, 3), " ± ", round(boot_info$precision_sd, 3), "\n")
        }
      }
      
      cat("\n")
    }
  }
  
  sink()
  
  cat("Resumen guardado:", basename(archivo_resumen), "\n")
  
  # Agregar datos originales a resultados
  resultados$datos <- datos
  resultados$archivo_original <- nombre_archivo
  
  cat("Analisis avanzado completado exitosamente\n")
  return(resultados)
}

# ============================================================================
# MENSAJE DE CARGA DEL MODULO
# ============================================================================

cat("MODULO DE ANALISIS ESTADISTICO AVANZADO OPERATIVO\n")
cat("==================================================\n")
cat("Funcionalidades incluidas:\n")
cat("- Modelos Lineales Generalizados Mixtos (GLMM)\n")
cat("- Analisis de colinealidad y VIF\n")
cat("- Curvas ROC y matrices de confusion\n")
cat("- Residuos de Pearson y diagnosticos DHARMa\n")
cat("- Efectos marginales\n")
cat("- PCA avanzado\n")
cat("- Matrices de correlacion Pearson/Spearman\n")
cat("- Validacion de variables defecto binarias\n")
cat("- Analisis por seccion con efectos aleatorios\n")
cat("- Validacion cruzada para robustez del modelo\n")