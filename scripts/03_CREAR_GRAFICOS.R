# ============================================================================
# MODULO 03: GENERACION DE GRAFICOS AVANZADOS
# Sistema de Analisis Estadistico - Control de Calidad IS
# ============================================================================

cat("MODULO DE GENERACION DE GRAFICOS AVANZADOS\n")
cat("==========================================\n")

# ============================================================================
# INSTALACION DE PAQUETES GRAFICOS
# ============================================================================

instalar_paquetes_graficos <- function() {
  paquetes_graficos <- c(
    "ggplot2", "corrplot", "plotROC", "ggeffects", 
    "DHARMa", "lattice", "gridExtra", "RColorBrewer",
    "pheatmap", "ggpubr", "plotly", "car"
  )
  
  for(paquete in paquetes_graficos) {
    if(!require(paquete, character.only = TRUE, quietly = TRUE)) {
      cat("Instalando", paquete, "para graficos...\n")
      tryCatch({
        install.packages(paquete, dependencies = TRUE, quiet = TRUE)
        library(paquete, character.only = TRUE)
      }, error = function(e) {
        cat("Error instalando", paquete, ":", e$message, "\n")
      })
    }
  }
}

# ============================================================================
# FUNCION PRINCIPAL DE GRAFICOS AVANZADOS
# ============================================================================

generar_graficos_completos_local <- function(datos, resultado_analisis, correlaciones) {
  cat("\nGENERANDO GRAFICOS AVANZADOS COMPLETOS\n")
  cat("======================================\n")
  
  if(is.null(datos)) {
    cat("ERROR: Datos no disponibles para graficos\n")
    return(NULL)
  }
  
  # Instalar paquetes graficos
  instalar_paquetes_graficos()
  
  # Crear timestamp y nombres de archivo
  timestamp <- crear_timestamp()
  archivo_original <- attr(datos, "archivo_original")
  if(is.null(archivo_original)) archivo_original <- "datos"
  nombre_base <- gsub("\\.csv$", "", archivo_original)
  
  archivo_pdf <- file.path(DIRECTORIO_GRAFICOS, 
                          paste0("graficos_avanzados_", nombre_base, "_", timestamp, ".pdf"))
  
  cat("Creando archivo PDF:", basename(archivo_pdf), "\n")
  
  # ==========================================
  # CONFIGURAR PDF MULTI-PAGINA
  # ==========================================
  
  pdf(archivo_pdf, width = 14, height = 10)
  
  tryCatch({
    
    # ==========================================
    # GRAFICO 1: DISTRIBUCION DE DEFECTOS CON DETALLES
    # ==========================================
    
    cat("Generando grafico 1: Distribucion detallada de defectos...\n")
    
    variables_defecto <- c("CLARO", "PLIEGUEPEQ", "PLIEGUEGR", "DEFECTOS")
    variables_defecto_presentes <- variables_defecto[variables_defecto %in% names(datos)]
    
    if(length(variables_defecto_presentes) > 0) {
      
      # Layout con multiples subgraficos
      par(mfrow = c(2, 2), mar = c(5, 4, 4, 2))
      
      # Barplot de tasas
      tasas_defecto <- sapply(variables_defecto_presentes, function(var) {
        mean(datos[[var]], na.rm = TRUE) * 100
      })
      
      barplot(tasas_defecto, 
              main = "Tasas de Defecto por Tipo",
              ylab = "Tasa (%)",
              col = RColorBrewer::brewer.pal(length(tasas_defecto), "Set3"),
              las = 2)
      
      # Agregar valores encima de barras
      text(seq_along(tasas_defecto), tasas_defecto + max(tasas_defecto)*0.05, 
           paste0(round(tasas_defecto, 2), "%"), cex = 0.8)
      
      # Histograma de defectos totales
      if("DEFECTOS" %in% names(datos)) {
        hist(datos$DEFECTOS, 
             main = "Distribucion de Defectos Binarios",
             xlab = "Defecto (0=No, 1=Si)",
             ylab = "Frecuencia",
             col = "lightblue",
             breaks = c(-0.5, 0.5, 1.5))
      }
      
      # Box plot por seccion si disponible
      if("SECCION" %in% names(datos) && "DEFECTOS" %in% names(datos)) {
        boxplot(DEFECTOS ~ SECCION, data = datos,
                main = "Defectos por Seccion",
                xlab = "Seccion",
                ylab = "Defectos",
                col = "lightgreen")
      }
      
      # Grafico de correlacion entre tipos de defecto
      if(length(variables_defecto_presentes) >= 2) {
        datos_defecto <- datos[, variables_defecto_presentes, drop = FALSE]
        cor_defectos <- cor(datos_defecto, use = "complete.obs")
        
        if(require(corrplot, quietly = TRUE)) {
          corrplot(cor_defectos, method = "color", type = "upper",
                  title = "Correlacion entre Tipos de Defecto",
                  mar = c(0,0,2,0))
        }
      }
      
      # Resetear layout
      par(mfrow = c(1, 1))
    }
    
    # ==========================================
    # GRAFICO 2: CURVAS ROC
    # ==========================================
    
    cat("Generando grafico 2: Curvas ROC...\n")
    
    if(!is.null(resultado_analisis$roc) && length(resultado_analisis$roc) > 0) {
      
      if(require(pROC, quietly = TRUE)) {
        # Layout para multiples ROC
        n_roc <- length(resultado_analisis$roc)
        if(n_roc <= 4) {
          par(mfrow = c(2, 2))
        } else {
          par(mfrow = c(3, 2))
        }
        
        roc_exitosos <- 0
        
        for(var_defecto in names(resultado_analisis$roc)) {
          tryCatch({
            roc_info <- resultado_analisis$roc[[var_defecto]]
            
            # Verificar que el objeto ROC existe y es valido
            if(!is.null(roc_info$roc) && !is.null(roc_info$auc) && 
               is.numeric(roc_info$auc) && !is.na(roc_info$auc)) {
              
              roc_obj <- roc_info$roc
              auc_val <- roc_info$auc
              
              plot(roc_obj, 
                   main = paste("Curva ROC -", var_defecto),
                   sub = paste("AUC =", round(auc_val, 3)),
                   col = "red", lwd = 2)
              
              # Linea diagonal de referencia
              abline(a = 0, b = 1, lty = 2, col = "gray")
              
              # Leyenda
              legend("bottomright", 
                     legend = c(paste("AUC =", round(auc_val, 3)), "Referencia"),
                     col = c("red", "gray"),
                     lty = c(1, 2),
                     cex = 0.8)
              
              roc_exitosos <- roc_exitosos + 1
              
            } else {
              # Crear grafico placeholder para este defecto
              plot.new()
              title(paste("ROC -", var_defecto, "(No disponible)"))
              text(0.5, 0.5, "Datos insuficientes\nModelo no convergio", 
                   cex = 1.2, adj = 0.5)
            }
            
          }, error = function(e) {
            # Crear grafico placeholder en caso de error
            plot.new()
            title(paste("ROC -", var_defecto, "(Error)"))
            text(0.5, 0.5, paste("Error generando ROC:\n", substr(e$message, 1, 50)), 
                 cex = 1.0, adj = 0.5)
          })
        }
        
        # Si no se genero ningun ROC, crear mensaje informativo
        if(roc_exitosos == 0) {
          plot.new()
          title("Curvas ROC - Informacion")
          text(0.5, 0.7, "No se pudieron generar curvas ROC", cex = 1.4, adj = 0.5)
          text(0.5, 0.5, "Posibles causas:", cex = 1.2, adj = 0.5, font = 2)
          text(0.5, 0.4, "• Muy pocos casos positivos", cex = 1.1, adj = 0.5)
          text(0.5, 0.35, "• Modelos GLMM no convergieron", cex = 1.1, adj = 0.5)
          text(0.5, 0.3, "• Datos insuficientes para prediccion", cex = 1.1, adj = 0.5)
        }
        
        par(mfrow = c(1, 1))
      } else {
        plot.new()
        title("Curvas ROC no disponibles")
        text(0.5, 0.5, "Libreria pROC requerida", cex = 1.5)
      }
    } else {
      plot.new()
      title("Curvas ROC no disponibles")
      text(0.5, 0.5, "Modelos GLMM requeridos para ROC", cex = 1.5)
    }
    
    # ==========================================
    # GRAFICO 3: MATRICES DE CONFUSION
    # ==========================================
    
    cat("Generando grafico 3: Matrices de confusion...\n")
    
    if(!is.null(resultado_analisis$roc) && length(resultado_analisis$roc) > 0) {
      
      n_matrices <- length(resultado_analisis$roc)
      par(mfrow = c(2, 2))
      
      matrices_exitosas <- 0
      
      for(var_defecto in names(resultado_analisis$roc)) {
        tryCatch({
          roc_info <- resultado_analisis$roc[[var_defecto]]
          
          if(!is.null(roc_info$matriz_confusion) && 
             !is.null(roc_info$matriz_confusion$table)) {
            
            matriz <- roc_info$matriz_confusion$table
            accuracy <- roc_info$matriz_confusion$overall['Accuracy']
            
            # Verificar que la matriz tiene dimensiones validas
            if(is.matrix(matriz) && nrow(matriz) >= 2 && ncol(matriz) >= 2) {
              
              # Crear heatmap de confusion matrix
              image(1:ncol(matriz), 1:nrow(matriz), as.matrix(matriz),
                    col = heat.colors(max(matriz, na.rm = TRUE)),
                    main = paste("Matriz Confusion -", var_defecto),
                    sub = paste("Accuracy =", round(accuracy, 3)),
                    xlab = "Predicho", ylab = "Observado",
                    axes = FALSE)
              
              # Agregar valores en celdas
              for(i in 1:nrow(matriz)) {
                for(j in 1:ncol(matriz)) {
                  text(j, i, matriz[i, j], cex = 1.2, font = 2)
                }
              }
              
              # Agregar ejes
              axis(1, at = 1:ncol(matriz), labels = colnames(matriz))
              axis(2, at = 1:nrow(matriz), labels = rownames(matriz))
              
              matrices_exitosas <- matrices_exitosas + 1
              
            } else {
              # Matriz invalida
              plot.new()
              title(paste("Matriz -", var_defecto, "(Datos insuficientes)"))
              text(0.5, 0.5, "Muy pocos casos\npara matriz valida", 
                   cex = 1.2, adj = 0.5)
            }
            
          } else {
            # Sin matriz de confusion
            plot.new()
            title(paste("Matriz -", var_defecto, "(No disponible)"))
            text(0.5, 0.5, "Modelo no convergio\no datos insuficientes", 
                 cex = 1.1, adj = 0.5)
          }
          
        }, error = function(e) {
          # Error procesando matriz
          plot.new()
          title(paste("Matriz -", var_defecto, "(Error)"))
          text(0.5, 0.5, paste("Error generando matriz:\n", substr(e$message, 1, 40)), 
               cex = 1.0, adj = 0.5)
        })
      }
      
      # Si no se genero ninguna matriz, crear mensaje informativo
      if(matrices_exitosas == 0) {
        plot.new()
        title("Matrices de Confusion - Informacion")
        text(0.5, 0.7, "No se pudieron generar matrices", cex = 1.4, adj = 0.5)
        text(0.5, 0.5, "Causas tipicas:", cex = 1.2, adj = 0.5, font = 2)
        text(0.5, 0.4, "• Dataset muy desbalanceado", cex = 1.1, adj = 0.5)
        text(0.5, 0.35, "• Menos de 10 casos positivos", cex = 1.1, adj = 0.5)
        text(0.5, 0.3, "• Modelos no convergieron", cex = 1.1, adj = 0.5)
      }
      
      par(mfrow = c(1, 1))
    } else {
      plot.new()
      title("Matrices de Confusion no disponibles")
      text(0.5, 0.5, "Modelos GLMM requeridos", cex = 1.5)
    }
    
    # ==========================================
    # GRAFICO 4: EFECTOS MARGINALES
    # ==========================================
    
    cat("Generando grafico 4: Efectos marginales...\n")
    
    if(!is.null(resultado_analisis$efectos_marginales) && length(resultado_analisis$efectos_marginales) > 0) {
      
      if(require(ggplot2, quietly = TRUE)) {
        n_efectos <- length(resultado_analisis$efectos_marginales)
        par(mfrow = c(2, 2))
        
        for(var_defecto in names(resultado_analisis$efectos_marginales)) {
          efectos <- resultado_analisis$efectos_marginales[[var_defecto]]$efectos_principales
          predictor <- resultado_analisis$efectos_marginales[[var_defecto]]$predictor_principal
          
          if(!is.null(efectos)) {
            tryCatch({
              # Convertir ggeffects a plot base
              plot(efectos$x, efectos$predicted,
                   type = "l", lwd = 2, col = "blue",
                   main = paste("Efectos Marginales -", var_defecto),
                   xlab = predictor,
                   ylab = "Probabilidad Predicha")
              
              # Agregar intervalos de confianza
              if(!is.null(efectos$conf.low) && !is.null(efectos$conf.high)) {
                polygon(c(efectos$x, rev(efectos$x)), 
                       c(efectos$conf.low, rev(efectos$conf.high)),
                       col = rgb(0, 0, 1, 0.2), border = NA)
              }
              
              grid()
              
            }, error = function(e) {
              plot.new()
              title(paste("Error en efectos -", var_defecto))
            })
          }
        }
        
        par(mfrow = c(1, 1))
      }
    } else {
      plot.new()
      title("Efectos marginales no disponibles")
    }
    
    # ==========================================
    # GRAFICO 5: ANALISIS DE RESIDUOS
    # ==========================================
    
    cat("Generando grafico 5: Analisis de residuos...\n")
    
    if(!is.null(resultado_analisis$residuos) && length(resultado_analisis$residuos) > 0) {
      
      par(mfrow = c(2, 2))
      
      for(var_defecto in names(resultado_analisis$residuos)) {
        residuos_pearson <- resultado_analisis$residuos[[var_defecto]]$residuos_pearson
        
        if(length(residuos_pearson) > 10) {
          
          # Q-Q plot de residuos
          qqnorm(residuos_pearson, main = paste("Q-Q Residuos -", var_defecto))
          qqline(residuos_pearson, col = "red")
          
          # Histograma de residuos
          hist(residuos_pearson, 
               main = paste("Distribucion Residuos -", var_defecto),
               xlab = "Residuos de Pearson",
               col = "lightblue",
               breaks = 20)
          
          # Residuos vs fitted
          if(!is.null(resultado_analisis$glmm[[var_defecto]])) {
            modelo <- resultado_analisis$glmm[[var_defecto]]$modelo
            fitted_vals <- fitted(modelo)
            
            plot(fitted_vals, residuos_pearson,
                 main = paste("Residuos vs Ajustados -", var_defecto),
                 xlab = "Valores Ajustados",
                 ylab = "Residuos de Pearson")
            abline(h = 0, col = "red", lty = 2)
            
            # Lowess smooth
            lines(lowess(fitted_vals, residuos_pearson), col = "blue", lwd = 2)
          }
        }
      }
      
      par(mfrow = c(1, 1))
    } else {
      plot.new()
      title("Analisis de residuos no disponible")
    }
    
    # ==========================================
    # GRAFICO 6: MATRIZ DE CORRELACION AVANZADA
    # ==========================================
    
    cat("Generando grafico 6: Matriz de correlacion avanzada...\n")
    
    if(!is.null(resultado_analisis$correlaciones)) {
      
      if(require(corrplot, quietly = TRUE)) {
        
        # Layout para dos matrices de correlacion
        par(mfrow = c(1, 2))
        
        # Pearson
        if(!is.null(resultado_analisis$correlaciones$pearson)) {
          corrplot(resultado_analisis$correlaciones$pearson, 
                  method = "color", type = "upper",
                  order = "hclust",
                  title = "Correlacion Pearson",
                  mar = c(0,0,2,0),
                  tl.cex = 0.8, tl.col = "black")
        }
        
        # Spearman
        if(!is.null(resultado_analisis$correlaciones$spearman)) {
          corrplot(resultado_analisis$correlaciones$spearman, 
                  method = "color", type = "upper",
                  order = "hclust",
                  title = "Correlacion Spearman",
                  mar = c(0,0,2,0),
                  tl.cex = 0.8, tl.col = "black")
        }
        
        par(mfrow = c(1, 1))
      } else {
        # Fallback con heatmap base
        if(!is.null(resultado_analisis$correlaciones$pearson)) {
          heatmap(resultado_analisis$correlaciones$pearson,
                 main = "Matriz de Correlacion",
                 col = heat.colors(50))
        }
      }
    }
    
    # ==========================================
    # GRAFICO 7: ANALISIS VIF Y COLINEALIDAD
    # ==========================================
    
    cat("Generando grafico 7: VIF y colinealidad...\n")
    
    if(!is.null(resultado_analisis$vif)) {
      
      vif_valores <- resultado_analisis$vif
      
      # Barplot de VIF
      par(mar = c(8, 4, 4, 2))
      
      barplot(vif_valores, 
              main = "Factores de Inflacion de Varianza (VIF)",
              ylab = "VIF",
              col = ifelse(vif_valores > 10, "red", 
                          ifelse(vif_valores > 5, "orange", "green")),
              las = 2)
      
      # Lineas de referencia
      abline(h = 5, col = "orange", lty = 2, lwd = 2)
      abline(h = 10, col = "red", lty = 2, lwd = 2)
      
      # Leyenda
      legend("topright", 
             legend = c("VIF < 5 (Aceptable)", "5 < VIF < 10 (Moderado)", "VIF > 10 (Alto)"),
             fill = c("green", "orange", "red"),
             cex = 0.8)
      
      # Agregar valores encima de barras
      text(seq_along(vif_valores), vif_valores + max(vif_valores)*0.05, 
           round(vif_valores, 2), cex = 0.7, srt = 45)
      
      par(mar = c(5, 4, 4, 2))
    } else {
      plot.new()
      title("Analisis VIF no disponible")
    }
    
    # ==========================================
    # GRAFICO 8: PCA AVANZADO
    # ==========================================
    
    cat("Generando grafico 8: PCA avanzado...\n")
    
    if(!is.null(resultado_analisis$pca)) {
      
      pca_resultado <- resultado_analisis$pca$resultado
      varianza_explicada <- resultado_analisis$pca$varianza_explicada
      
      par(mfrow = c(2, 2))
      
      # Scree plot
      barplot(varianza_explicada[1:min(10, length(varianza_explicada))], 
              main = "Scree Plot - Varianza Explicada",
              xlab = "Componente Principal",
              ylab = "Proporcion de Varianza",
              col = "steelblue")
      
      # Biplot
      biplot(pca_resultado, scale = 0,
             main = "Biplot PCA",
             cex = 0.8)
      
      # Loadings PC1
      loadings_pc1 <- pca_resultado$rotation[, 1]
      barplot(sort(abs(loadings_pc1), decreasing = TRUE)[1:10],
              main = "Loadings PC1 (Top 10)",
              horiz = TRUE,
              las = 1,
              col = "lightgreen")
      
      # Varianza acumulada
      varianza_acum <- resultado_analisis$pca$varianza_acumulada
      plot(1:length(varianza_acum), varianza_acum,
           type = "b", pch = 19,
           main = "Varianza Acumulada",
           xlab = "Componente Principal",
           ylab = "Varianza Acumulada",
           col = "blue")
      abline(h = 0.8, col = "red", lty = 2)
      text(length(varianza_acum)/2, 0.85, "80% Varianza", col = "red")
      
      par(mfrow = c(1, 1))
    } else {
      plot.new()
      title("PCA no disponible")
    }
    
    # ==========================================
    # GRAFICO 9: INTERACCIONES (SI HAY MULTIPLES PREDICTORES)
    # ==========================================
    
    cat("Generando grafico 9: Validacion cruzada...\n")
    
    # Grafico de validacion cruzada
    if(!is.null(resultado_analisis$validacion_cruzada) && length(resultado_analisis$validacion_cruzada) > 0) {
      
      # Preparar datos para visualizacion
      cv_data <- list()
      cv_names <- character()
      
      for(var_def in names(resultado_analisis$validacion_cruzada)) {
        cv_info <- resultado_analisis$validacion_cruzada[[var_def]]
        if(!is.na(cv_info$auc_mean) && length(cv_info$auc_folds) > 0) {
          cv_data[[length(cv_data) + 1]] <- cv_info$auc_folds
          cv_names <- c(cv_names, var_def)
        }
      }
      
      if(length(cv_data) > 0) {
        par(mfrow = c(2, 2), mar = c(5, 4, 4, 2))
        
        # Boxplot de AUC por defecto
        boxplot(cv_data,
                names = cv_names,
                main = "Robustez AUC - Validacion Cruzada",
                ylab = "AUC",
                col = c("lightblue", "lightgreen", "lightcoral")[1:length(cv_data)],
                las = 2)
        grid()
        
        # Barplot de medias con error bars
        auc_means <- sapply(cv_data, mean, na.rm = TRUE)
        auc_sds <- sapply(cv_data, sd, na.rm = TRUE)
        
        bp <- barplot(auc_means,
                      names.arg = cv_names,
                      main = "AUC Promedio ± SD",
                      ylab = "AUC",
                      col = c("lightblue", "lightgreen", "lightcoral")[1:length(cv_data)],
                      ylim = c(0, max(auc_means + auc_sds, na.rm = TRUE) * 1.1),
                      las = 2)
        
        # Error bars
        for(i in 1:length(auc_means)) {
          lines(c(bp[i], bp[i]), c(auc_means[i] - auc_sds[i], auc_means[i] + auc_sds[i]), lwd = 2)
          lines(c(bp[i] - 0.1, bp[i] + 0.1), c(auc_means[i] - auc_sds[i], auc_means[i] - auc_sds[i]), lwd = 2)
          lines(c(bp[i] - 0.1, bp[i] + 0.1), c(auc_means[i] + auc_sds[i], auc_means[i] + auc_sds[i]), lwd = 2)
        }
        
        # Accuracy si disponible
        acc_data <- list()
        acc_names <- character()
        
        for(var_def in names(resultado_analisis$validacion_cruzada)) {
          cv_info <- resultado_analisis$validacion_cruzada[[var_def]]
          if(!is.na(cv_info$accuracy_mean) && length(cv_info$accuracy_folds) > 0) {
            acc_data[[length(acc_data) + 1]] <- cv_info$accuracy_folds
            acc_names <- c(acc_names, var_def)
          }
        }
        
        if(length(acc_data) > 0) {
          boxplot(acc_data,
                  names = acc_names,
                  main = "Robustez Accuracy - Validacion Cruzada",
                  ylab = "Accuracy",
                  col = c("lightblue", "lightgreen", "lightcoral")[1:length(acc_data)],
                  las = 2)
          grid()
        }
        
        # Grafico de metricas para datos desbalanceados
        if(length(cv_data) > 0) {
          
          # Precision data
          precision_data <- list()
          precision_names <- character()
          
          for(var_def in names(resultado_analisis$validacion_cruzada)) {
            cv_info <- resultado_analisis$validacion_cruzada[[var_def]]
            if(!is.na(cv_info$precision_mean) && length(cv_info$precision_folds) > 0) {
              precision_data[[length(precision_data) + 1]] <- cv_info$precision_folds
              precision_names <- c(precision_names, paste(var_def, "\n(", cv_info$casos_positivos, "casos)", sep=""))
            }
          }
          
          if(length(precision_data) > 0) {
            boxplot(precision_data,
                    names = precision_names,
                    main = "Precision - Datos Desbalanceados",
                    ylab = "Precision",
                    col = c("lightyellow", "lightpink", "lightcyan")[1:length(precision_data)],
                    las = 2,
                    cex.names = 0.8)
            grid()
          }
          
        } else {
          # Texto informativo
          plot.new()
          title("Resumen Validacion Cruzada")
          
          texto_resumen <- c()
          for(var_def in names(resultado_analisis$validacion_cruzada)) {
            cv_info <- resultado_analisis$validacion_cruzada[[var_def]]
            if(!is.na(cv_info$auc_mean)) {
              texto_resumen <- c(texto_resumen,
                               paste(var_def, ": AUC =", round(cv_info$auc_mean, 3), 
                                     "±", round(cv_info$auc_sd, 3)))
            }
          }
          
          if(length(texto_resumen) > 0) {
            text(0.1, 0.9, paste(texto_resumen, collapse = "\n"), 
                 cex = 1.0, adj = 0, family = "mono")
            text(0.1, 0.5, "5-fold Cross-Validation", cex = 1.4, font = 2, adj = 0)
            text(0.1, 0.4, paste("Folds validos promedio:", 
                                 round(mean(sapply(resultado_analisis$validacion_cruzada, function(x) x$folds_validos)), 1)), 
                 cex = 1.1, adj = 0)
            
            # Agregar informacion sobre validacion alternativa si existe
            if(!is.null(resultado_analisis$validacion_alternativa) && length(resultado_analisis$validacion_alternativa) > 0) {
              text(0.1, 0.2, "Validacion alternativa disponible\npara casos raros", 
                   cex = 1.0, adj = 0, col = "red")
            }
          }
        }
        
        par(mfrow = c(1, 1))
        
      } else {
        plot.new()
        title("Validacion Cruzada: Datos insuficientes")
        text(0.5, 0.5, "No hay suficientes datos\npara validacion cruzada", 
             cex = 1.5, adj = 0.5)
      }
      
    } else {
      plot.new()
      title("Validacion Cruzada: No disponible")
      text(0.5, 0.5, "Validacion cruzada\nno implementada", 
           cex = 1.5, adj = 0.5)
    }
    
    # ==========================================
    # GRAFICO 10: ANALISIS TEMPORAL Y PATRONES  
    # ==========================================
    
    cat("Generando grafico 10: Analisis temporal y patrones...\n")
    
    # Analisis temporal si FH esta disponible
    if("FH" %in% names(datos) && "DEFECTOS" %in% names(datos)) {
      
      par(mfrow = c(2, 2))
      
      # Crear variable hora si no existe
      if(inherits(datos$FH, "POSIXct")) {
        datos$hora <- as.numeric(format(datos$FH, "%H"))
        datos$dia_semana <- weekdays(datos$FH)
        
        # Defectos por hora
        defectos_hora <- aggregate(DEFECTOS ~ hora, data = datos, 
                                  FUN = function(x) mean(x, na.rm = TRUE) * 100)
        
        plot(defectos_hora$hora, defectos_hora$DEFECTOS,
             type = "b", pch = 19,
             main = "Tasa de Defectos por Hora",
             xlab = "Hora del Dia",
             ylab = "Tasa de Defectos (%)",
             col = "blue")
        grid()
        
        # Defectos por dia de semana
        if(length(unique(datos$dia_semana)) > 1) {
          defectos_dia <- aggregate(DEFECTOS ~ dia_semana, data = datos,
                                   FUN = function(x) mean(x, na.rm = TRUE) * 100)
          
          barplot(defectos_dia$DEFECTOS,
                  names.arg = defectos_dia$dia_semana,
                  main = "Defectos por Dia de Semana",
                  ylab = "Tasa de Defectos (%)",
                  col = "orange",
                  las = 2)
        }
      }
      
      # Boxplot de variables principales vs defectos
      variables_numericas <- names(datos)[sapply(datos, is.numeric)]
      variables_predictoras <- variables_numericas[!variables_numericas %in% 
                                                  c("CLARO", "PLIEGUEPEQ", "PLIEGUEGR", "DEFECTOS")]
      
      if(length(variables_predictoras) >= 2) {
        # Tomar primeras dos variables predictoras
        var1 <- variables_predictoras[1]
        var2 <- variables_predictoras[2]
        
        # Categorizar variables continuas para boxplot
        datos[[paste0(var1, "_cat")]] <- cut(datos[[var1]], breaks = 3, labels = c("Bajo", "Medio", "Alto"))
        datos[[paste0(var2, "_cat")]] <- cut(datos[[var2]], breaks = 3, labels = c("Bajo", "Medio", "Alto"))
        
        boxplot(DEFECTOS ~ get(paste0(var1, "_cat")), data = datos,
                main = paste("Defectos vs", var1),
                xlab = var1,
                ylab = "Defectos",
                col = "lightblue")
        
        boxplot(DEFECTOS ~ get(paste0(var2, "_cat")), data = datos,
                main = paste("Defectos vs", var2),
                xlab = var2,
                ylab = "Defectos",
                col = "lightcoral")
      }
      
      par(mfrow = c(1, 1))
    } else {
      plot.new()
      title("Analisis temporal no disponible")
    }
    
  }, error = function(e) {
    cat("Error generando graficos:", e$message, "\n")
  })
  
  # Cerrar PDF
  dev.off()
  
  cat("PDF de graficos avanzados completado:", basename(archivo_pdf), "\n")
  
  return(list(
    archivo_pdf = archivo_pdf,
    graficos_generados = 10
  ))
}

# ============================================================================
# MENSAJE DE CARGA DEL MODULO
# ============================================================================

cat("MODULO DE GRAFICOS AVANZADOS OPERATIVO\n")
cat("======================================\n")
cat("Graficos incluidos:\n")
cat("1. Distribucion detallada de defectos\n")
cat("2. Curvas ROC con AUC\n")
cat("3. Matrices de confusion\n")
cat("4. Efectos marginales\n")
cat("5. Analisis de residuos de Pearson\n")
cat("6. Matrices de correlacion Pearson/Spearman\n")
cat("7. Analisis VIF y colinealidad\n")
cat("8. PCA avanzado con biplots\n")
cat("9. Validacion cruzada para robustez\n")
cat("10. Analisis temporal y patrones\n")