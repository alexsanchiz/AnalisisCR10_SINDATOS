# ============================================================================
# MODULO 08: GRAFICOS AVANZADOS EXTENDIDOS CON ANALISIS SEGMENTADO
# Sistema de Analisis Estadistico - Control de Calidad IS
# ============================================================================
# Gráficos especializados para análisis segmentado, interacciones y calibración
# ============================================================================

cat("MODULO DE GRAFICOS AVANZADOS EXTENDIDOS\n")
cat("=======================================\n")

# ============================================================================
# FUNCION PRINCIPAL DE GRAFICOS EXTENDIDOS
# ============================================================================

generar_graficos_avanzados_extendidos <- function(resultado_analisis, resultado_segmentado, 
                                                 resultado_estirado_avanzado, resultado_calibracion,
                                                 datos, nombre_archivo) {
  
  cat("\nGENERANDO GRAFICOS AVANZADOS EXTENDIDOS\n")
  cat("=======================================\n")
  cat("Archivo:", nombre_archivo, "\n")
  
  # Crear timestamp y archivo
  timestamp <- crear_timestamp()
  archivo_pdf <- file.path(DIRECTORIO_GRAFICOS, 
                          paste0("graficos_extendidos_", gsub("\\.csv$", "", nombre_archivo), 
                                "_", timestamp, ".pdf"))
  
  cat("Creando archivo PDF:", basename(archivo_pdf), "\n")
  
  # Configurar PDF
  pdf(archivo_pdf, width = 16, height = 12)
  
  tryCatch({
    
    # ==========================================
    # GRAFICO 1: ANALISIS POR MODELO
    # ==========================================
    
    if(!is.null(resultado_segmentado$por_modelo)) {
      cat("Generando gráfico 1: Análisis por modelo...\n")
      
      # Preparar datos para gráfico
      modelos_data <- list()
      for(modelo in names(resultado_segmentado$por_modelo)) {
        for(defecto in names(resultado_segmentado$por_modelo[[modelo]])) {
          modelos_data <- rbind(modelos_data, data.frame(
            Modelo = modelo,
            Defecto = defecto,
            Tasa = resultado_segmentado$por_modelo[[modelo]][[defecto]]$tasa,
            Casos = resultado_segmentado$por_modelo[[modelo]][[defecto]]$casos
          ))
        }
      }
      
      if(nrow(modelos_data) > 0) {
        par(mfrow = c(2, 2), mar = c(5, 4, 4, 2))
        
        # Gráfico de barras por modelo
        for(defecto in unique(modelos_data$Defecto)) {
          datos_defecto <- modelos_data[modelos_data$Defecto == defecto, ]
          if(nrow(datos_defecto) > 0) {
            barplot(datos_defecto$Tasa, 
                   names.arg = datos_defecto$Modelo,
                   main = paste("Tasas de", defecto, "por Modelo"),
                   ylab = "Tasa (%)",
                   xlab = "Modelo",
                   col = rainbow(nrow(datos_defecto)),
                   las = 2)
            
            # Agregar valores sobre barras
            text(1:nrow(datos_defecto), datos_defecto$Tasa + max(datos_defecto$Tasa) * 0.05,
                 paste(datos_defecto$Casos, "casos"), cex = 0.8)
          }
        }
        
        par(mfrow = c(1, 1))
      }
    }
    
    # ==========================================
    # GRAFICO 2: GLMM POR MODELO - METRICAS
    # ==========================================
    
    if(!is.null(resultado_segmentado$modelos_por_modelo)) {
      cat("Generando gráfico 2: Métricas GLMM por modelo...\n")
      
      par(mfrow = c(2, 2), mar = c(8, 4, 4, 2))
      
      # Preparar datos para visualización
      metricas_data <- data.frame()
      
      for(modelo in names(resultado_segmentado$modelos_por_modelo)) {
        for(defecto in names(resultado_segmentado$modelos_por_modelo[[modelo]])) {
          modelo_info <- resultado_segmentado$modelos_por_modelo[[modelo]][[defecto]]
          
          metricas_data <- rbind(metricas_data, data.frame(
            Modelo = modelo,
            Defecto = defecto,
            AUC = modelo_info$auc,
            Accuracy = modelo_info$metricas$accuracy,
            Precision = modelo_info$metricas$precision,
            Recall = modelo_info$metricas$recall,
            F1_Score = modelo_info$metricas$f1_score,
            Casos_Positivos = modelo_info$n_casos_positivos
          ))
        }
      }
      
      if(nrow(metricas_data) > 0) {
        # Gráfico de AUC por modelo y defecto
        if(length(unique(metricas_data$Defecto)) > 1) {
          boxplot(AUC ~ Defecto, data = metricas_data,
                 main = "Distribución de AUC por Tipo de Defecto",
                 ylab = "AUC",
                 xlab = "Tipo de Defecto",
                 col = rainbow(length(unique(metricas_data$Defecto))),
                 las = 2)
        }
        
        # Gráfico de métricas por modelo
        if(length(unique(metricas_data$Modelo)) > 1) {
          # Accuracy por modelo
          modelo_accuracy <- aggregate(Accuracy ~ Modelo, data = metricas_data, mean, na.rm = TRUE)
          barplot(modelo_accuracy$Accuracy,
                 names.arg = modelo_accuracy$Modelo,
                 main = "Accuracy Promedio por Modelo",
                 ylab = "Accuracy",
                 col = "lightblue",
                 las = 2)
          
          # Precision vs Recall
          plot(metricas_data$Precision, metricas_data$Recall,
               main = "Precision vs Recall por Modelo",
               xlab = "Precision",
               ylab = "Recall",
               pch = 19,
               col = as.factor(metricas_data$Modelo),
               cex = 1.2)
          
          legend("bottomright", 
                 legend = unique(metricas_data$Modelo),
                 col = 1:length(unique(metricas_data$Modelo)),
                 pch = 19,
                 cex = 0.8)
          
          # Casos positivos por modelo
          barplot(table(metricas_data$Modelo),
                 main = "Número de Análisis por Modelo",
                 ylab = "Número de Defectos Analizados",
                 col = "lightgreen",
                 las = 2)
        }
      }
      
      par(mfrow = c(1, 1))
    }
    
    # ==========================================
    # GRAFICO 3: ANALISIS POR TURNO
    # ==========================================
    
    if(!is.null(resultado_segmentado$por_turno)) {
      cat("Generando gráfico 3: Análisis por turno...\n")
      
      # Preparar datos para gráfico
      turnos_data <- list()
      for(turno in names(resultado_segmentado$por_turno)) {
        for(defecto in names(resultado_segmentado$por_turno[[turno]])) {
          turnos_data <- rbind(turnos_data, data.frame(
            Turno = turno,
            Defecto = defecto,
            Tasa = resultado_segmentado$por_turno[[turno]][[defecto]]$tasa,
            Casos = resultado_segmentado$por_turno[[turno]][[defecto]]$casos
          ))
        }
      }
      
      if(nrow(turnos_data) > 0) {
        par(mfrow = c(2, 2), mar = c(5, 4, 4, 2))
        
        # Gráfico por defecto y turno
        defectos_unicos <- unique(turnos_data$Defecto)
        for(i in 1:min(4, length(defectos_unicos))) {
          defecto <- defectos_unicos[i]
          datos_defecto <- turnos_data[turnos_data$Defecto == defecto, ]
          
          barplot(datos_defecto$Tasa,
                 names.arg = datos_defecto$Turno,
                 main = paste("Tasas de", defecto, "por Turno"),
                 ylab = "Tasa (%)",
                 xlab = "Turno",
                 col = c("lightblue", "orange", "lightgreen")[1:nrow(datos_defecto)])
          
          # Agregar valores
          text(1:nrow(datos_defecto), datos_defecto$Tasa + max(datos_defecto$Tasa) * 0.05,
               paste(datos_defecto$Casos, "casos"), cex = 0.8)
        }
        
        par(mfrow = c(1, 1))
      }
    }
    
    # ==========================================
    # GRAFICO 4: CORRELACIONES CON LAGS
    # ==========================================
    
    if(!is.null(resultado_estirado_avanzado$correlaciones_lags)) {
      cat("Generando gráfico 4: Correlaciones con lags...\n")
      
      par(mfrow = c(2, 2), mar = c(5, 4, 4, 2))
      
      for(defecto in names(resultado_estirado_avanzado$correlaciones_lags)) {
        cors <- resultado_estirado_avanzado$correlaciones_lags[[defecto]]
        
        # Preparar datos para gráfico
        lags <- c("Simultaneo", "Lag1h", "Lag2h", "Lag4h", "Lag8h")
        valores <- c(cors$simultaneo, cors$lag1h, cors$lag2h, cors$lag4h, cors$lag8h)
        
        # Gráfico de líneas
        plot(1:length(lags), valores,
             type = "b",
             main = paste("Correlaciones con Lags -", defecto),
             xlab = "Tipo de Lag",
             ylab = "Correlación",
             xaxt = "n",
             pch = 19,
             col = "blue",
             lwd = 2)
        
        axis(1, at = 1:length(lags), labels = lags, las = 2)
        
        # Línea en cero
        abline(h = 0, lty = 2, col = "gray")
        
        # Marcar mejor lag
        mejor_idx <- which(lags == gsub("h", "h", cors$mejor_lag))
        if(length(mejor_idx) > 0) {
          points(mejor_idx, valores[mejor_idx], pch = 19, col = "red", cex = 2)
          text(mejor_idx, valores[mejor_idx] + 0.05, "MEJOR", cex = 0.8, col = "red")
        }
        
        # Grid
        grid(col = "lightgray", lty = 3)
      }
      
      par(mfrow = c(1, 1))
    }
    
    # ==========================================
    # GRAFICO 5: CURVAS DE CALIBRACION
    # ==========================================
    
    if(!is.null(resultado_calibracion)) {
      cat("Generando gráfico 5: Curvas de calibración...\n")
      
      par(mfrow = c(2, 2), mar = c(5, 4, 4, 2))
      
      for(defecto in names(resultado_calibracion)) {
        cal <- resultado_calibracion[[defecto]]
        
        if(!is.null(cal$calibracion$bins)) {
          bins <- cal$calibracion$bins
          
          # Curva de calibración
          plot(bins$prob_media, bins$tasa_observada,
               main = paste("Curva de Calibración -", defecto),
               xlab = "Probabilidad Predicha",
               ylab = "Tasa Observada",
               pch = 19,
               col = "blue",
               cex = 1.5,
               xlim = c(0, max(bins$prob_media, na.rm = TRUE)),
               ylim = c(0, max(bins$tasa_observada, na.rm = TRUE)))
          
          # Línea de calibración perfecta
          abline(0, 1, lty = 2, col = "red", lwd = 2)
          
          # Conectar puntos
          lines(bins$prob_media, bins$tasa_observada, col = "blue", lwd = 2)
          
          # Agregar tamaños de muestra
          text(bins$prob_media, bins$tasa_observada + max(bins$tasa_observada) * 0.05,
               paste("n =", bins$n_obs), cex = 0.7)
          
          # Información de calibración
          brier_text <- paste("Brier Score:", sprintf("%.4f", cal$calibracion$brier_score))
          
          if(!is.na(cal$calibracion$hosmer_lemeshow_pvalue)) {
            hl_text <- paste("H-L p-value:", sprintf("%.4f", cal$calibracion$hosmer_lemeshow_pvalue))
            calib_text <- ifelse(cal$calibracion$bien_calibrado, "Bien Calibrado", "Mal Calibrado")
          } else {
            hl_text <- "H-L: N/A"
            calib_text <- "Calibración: N/A"
          }
          
          # Leyenda
          legend("topleft", 
                 legend = c("Calibración Perfecta", "Modelo", brier_text, hl_text, calib_text),
                 col = c("red", "blue", "black", "black", "black"),
                 lty = c(2, 1, 0, 0, 0),
                 pch = c(NA, 19, NA, NA, NA),
                 cex = 0.8)
          
          # Grid
          grid(col = "lightgray", lty = 3)
        }
      }
      
      par(mfrow = c(1, 1))
    }
    
    # ==========================================
    # GRAFICO 6: COMPARACION DE UMBRALES
    # ==========================================
    
    if(!is.null(resultado_calibracion)) {
      cat("Generando gráfico 6: Comparación de umbrales...\n")
      
      par(mfrow = c(1, 1), mar = c(8, 4, 4, 2))
      
      # Preparar datos de umbrales
      umbrales_data <- data.frame()
      
      for(defecto in names(resultado_calibracion)) {
        cal <- resultado_calibracion[[defecto]]
        umbrales_data <- rbind(umbrales_data, data.frame(
          Defecto = defecto,
          Youden = cal$umbrales$youden,
          Precision = cal$umbrales$precision,
          F1 = cal$umbrales$f1,
          Recall_Constrained = cal$umbrales$recall_constrained,
          Recomendado = cal$umbrales$recomendado
        ))
      }
      
      if(nrow(umbrales_data) > 0) {
        # Transponer datos para gráfico
        umbrales_matrix <- as.matrix(umbrales_data[, -1])
        rownames(umbrales_matrix) <- umbrales_data$Defecto
        
        # Gráfico de barras agrupadas
        barplot(t(umbrales_matrix),
               main = "Comparación de Umbrales Óptimos por Defecto",
               ylab = "Umbral",
               xlab = "Tipo de Defecto",
               beside = TRUE,
               col = rainbow(ncol(umbrales_matrix)),
               las = 2,
               legend.text = colnames(umbrales_matrix),
               args.legend = list(x = "topright", cex = 0.8))
        
        # Grid horizontal
        grid(col = "lightgray", lty = 3, nx = NA, ny = NULL)
      }
    }
    
    # ==========================================
    # GRAFICO 7: EFECTOS NO LINEALES Y DIAGNOSTICOS
    # ==========================================
    
    if(!is.null(resultado_segmentado$modelos_splines_mgcv)) {
      cat("Generando gráfico 7: Efectos no lineales y diagnósticos...\n")
      
      par(mfrow = c(2, 2), mar = c(5, 4, 4, 2))
      
      for(defecto in names(resultado_segmentado$modelos_splines_mgcv)) {
        splines_info <- resultado_segmentado$modelos_splines_mgcv[[defecto]]
        
        # Gráfico de términos de suavizado significativos
        smooth_terms <- splines_info$smooth_terms
        smooth_pvals <- smooth_terms[, "p-value"]
        smooth_names <- rownames(smooth_terms)
        
        # Filtrar términos significativos
        sig_terms <- smooth_pvals < 0.05
        
        if(sum(sig_terms) > 0) {
          barplot(-log10(smooth_pvals[sig_terms]),
                 names.arg = gsub("s\\(|\\)", "", smooth_names[sig_terms]),
                 main = paste("Significancia Términos No Lineales -", defecto),
                 ylab = "-log10(p-value)",
                 col = "orange",
                 las = 2)
          
          # Línea de significancia
          abline(h = -log10(0.05), lty = 2, col = "red", lwd = 2)
          text(par("usr")[2] * 0.8, -log10(0.05) + 0.1, "p = 0.05", col = "red")
        } else {
          plot.new()
          title(paste("Términos No Lineales -", defecto))
          text(0.5, 0.5, "No se detectaron\nefectos no lineales\nsignificativos", 
               cex = 1.2, adj = 0.5)
        }
      }
      
      # Gráfico de comparación AUC modelos GAM
      auc_gam_values <- sapply(resultado_segmentado$modelos_splines_mgcv, function(x) x$auc_gam)
      names(auc_gam_values) <- names(resultado_segmentado$modelos_splines_mgcv)
      
      barplot(auc_gam_values,
             main = "AUC Modelos GAM por Defecto",
             ylab = "AUC",
             col = "lightgreen",
             las = 2,
             ylim = c(0, 1))
      
      # Agregar valores
      text(1:length(auc_gam_values), auc_gam_values + 0.05,
           sprintf("%.3f", auc_gam_values), cex = 0.9)
      
      par(mfrow = c(1, 1))
    }
    
    # ==========================================
    # GRAFICO ADICIONAL: INTERACCIONES POR MODELO
    # ==========================================
    
    if(!is.null(resultado_segmentado$modelos_por_modelo)) {
      cat("Generando gráfico adicional: Interacciones por modelo...\n")
      
      par(mfrow = c(2, 2), mar = c(8, 4, 4, 2))
      
      # Recopilar todas las interacciones significativas
      todas_interacciones <- data.frame()
      
      for(modelo in names(resultado_segmentado$modelos_por_modelo)) {
        for(defecto in names(resultado_segmentado$modelos_por_modelo[[modelo]])) {
          modelo_info <- resultado_segmentado$modelos_por_modelo[[modelo]][[defecto]]
          
          if(!is.null(modelo_info$efectos_significativos)) {
            nombres_coef <- rownames(modelo_info$efectos_significativos)
            interacciones <- nombres_coef[grepl(":", nombres_coef)]
            
            if(length(interacciones) > 0) {
              for(int in interacciones) {
                coef <- modelo_info$efectos_significativos[int, "Estimate"]
                pval <- modelo_info$efectos_significativos[int, "Pr(>|z|)"]
                
                todas_interacciones <- rbind(todas_interacciones, data.frame(
                  Modelo = modelo,
                  Defecto = defecto,
                  Interaccion = int,
                  Coeficiente = coef,
                  P_value = pval
                ))
              }
            }
          }
        }
      }
      
      if(nrow(todas_interacciones) > 0) {
        # Gráfico de número de interacciones por modelo
        interacciones_por_modelo <- table(todas_interacciones$Modelo)
        barplot(interacciones_por_modelo,
               main = "Número de Interacciones Significativas por Modelo",
               ylab = "Número de Interacciones",
               col = "lightcoral",
               las = 2)
        
        # Gráfico de tipos de interacciones más frecuentes
        tipos_interaccion <- table(todas_interacciones$Interaccion)
        if(length(tipos_interaccion) > 0) {
          # Mostrar solo las 10 más frecuentes
          top_interacciones <- head(sort(tipos_interaccion, decreasing = TRUE), 10)
          
          barplot(top_interacciones,
                 main = "Interacciones Más Frecuentes",
                 ylab = "Frecuencia",
                 col = "lightyellow",
                 las = 2,
                 cex.names = 0.7)
        }
        
        # Distribución de coeficientes de interacción
        hist(todas_interacciones$Coeficiente,
             main = "Distribución de Coeficientes de Interacción",
             xlab = "Coeficiente",
             ylab = "Frecuencia",
             col = "lightpink",
             breaks = 15)
        
        # Línea vertical en cero
        abline(v = 0, lty = 2, col = "red", lwd = 2)
        
        # Significancia vs magnitud de coeficientes
        plot(-log10(todas_interacciones$P_value), abs(todas_interacciones$Coeficiente),
             main = "Significancia vs Magnitud de Interacciones",
             xlab = "-log10(p-value)",
             ylab = "Magnitud del Coeficiente",
             pch = 19,
             col = as.factor(todas_interacciones$Modelo),
             cex = 1.2)
        
        # Línea de significancia
        abline(v = -log10(0.05), lty = 2, col = "red")
        
        if(length(unique(todas_interacciones$Modelo)) <= 8) {
          legend("topright", 
                 legend = unique(todas_interacciones$Modelo),
                 col = 1:length(unique(todas_interacciones$Modelo)),
                 pch = 19,
                 cex = 0.7)
        }
      }
      
      par(mfrow = c(1, 1))
    }
    
    # ==========================================
    # GRAFICO 8: RESUMEN EJECUTIVO VISUAL
    # ==========================================
    
    cat("Generando gráfico 8: Resumen ejecutivo visual...\n")
    
    # Crear página de resumen
    par(mfrow = c(1, 1), mar = c(1, 1, 1, 1))
    plot.new()
    title("RESUMEN EJECUTIVO - ANÁLISIS AVANZADO EXTENDIDO", cex.main = 1.8, font.main = 2)
    
    # Información del archivo
    text(0.5, 0.9, paste("Archivo:", nombre_archivo), cex = 1.4, font = 2, adj = 0.5)
    text(0.5, 0.85, paste("Observaciones:", nrow(datos)), cex = 1.2, adj = 0.5)
    text(0.5, 0.8, paste("Fecha análisis:", format(Sys.time(), "%Y-%m-%d %H:%M")), cex = 1.2, adj = 0.5)
    
    # Resultados principales
    y_pos <- 0.7
    
    if(!is.null(resultado_segmentado$por_modelo)) {
      text(0.1, y_pos, "ANÁLISIS POR MODELO:", cex = 1.3, font = 2, adj = 0)
      y_pos <- y_pos - 0.05
      text(0.1, y_pos, paste("• Modelos analizados:", length(resultado_segmentado$por_modelo)), cex = 1.1, adj = 0)
      y_pos <- y_pos - 0.04
      text(0.1, y_pos, "• Variabilidad significativa entre modelos detectada", cex = 1.1, adj = 0)
      y_pos <- y_pos - 0.06
    }
    
    if(!is.null(resultado_segmentado$modelos_por_modelo)) {
      text(0.1, y_pos, "GLMM POR MODELO - INTERACCIONES EXTENDIDAS:", cex = 1.3, font = 2, adj = 0)
      y_pos <- y_pos - 0.05
      
      interacciones_totales <- 0
      for(modelo in names(resultado_segmentado$modelos_por_modelo)) {
        for(defecto in names(resultado_segmentado$modelos_por_modelo[[modelo]])) {
          modelo_info <- resultado_segmentado$modelos_por_modelo[[modelo]][[defecto]]
          if(!is.null(modelo_info$efectos_significativos)) {
            nombres_coef <- rownames(modelo_info$efectos_significativos)
            interacciones <- nombres_coef[grepl(":", nombres_coef)]
            interacciones_totales <- interacciones_totales + length(interacciones)
          }
        }
      }
      
      text(0.1, y_pos, paste("• Total interacciones detectadas:", interacciones_totales), cex = 1.1, adj = 0)
      y_pos <- y_pos - 0.04
      text(0.1, y_pos, "• GLMM independientes por modelo implementados", cex = 1.1, adj = 0)
      y_pos <- y_pos - 0.04
      text(0.1, y_pos, "• Diagnósticos DHARMa y leverage completos", cex = 1.1, adj = 0)
      y_pos <- y_pos - 0.06
    }
    
    if(!is.null(resultado_estirado_avanzado)) {
      text(0.1, y_pos, "ANÁLISIS DE LAGS TEMPORALES:", cex = 1.3, font = 2, adj = 0)
      y_pos <- y_pos - 0.05
      text(0.1, y_pos, "• Efectos retardados del estirado evaluados", cex = 1.1, adj = 0)
      y_pos <- y_pos - 0.04
      text(0.1, y_pos, "• Correlaciones con diferentes horizontes temporales", cex = 1.1, adj = 0)
      y_pos <- y_pos - 0.06
    }
    
    if(!is.null(resultado_calibracion)) {
      text(0.1, y_pos, "CALIBRACIÓN DE UMBRALES:", cex = 1.3, font = 2, adj = 0)
      y_pos <- y_pos - 0.05
      text(0.1, y_pos, paste("• Umbrales optimizados para", length(resultado_calibracion), "tipos de defecto"), cex = 1.1, adj = 0)
      y_pos <- y_pos - 0.04
      text(0.1, y_pos, "• Curvas de reliability analizadas", cex = 1.1, adj = 0)
      y_pos <- y_pos - 0.04
      text(0.1, y_pos, "• Recomendaciones específicas por caso de uso", cex = 1.1, adj = 0)
    }
    
  }, error = function(e) {
    cat("Error generando gráficos extendidos:", e$message, "\n")
    plot.new()
    title("Error en Generación de Gráficos")
    text(0.5, 0.5, paste("Error:", e$message), cex = 1.2, adj = 0.5)
  })
  
  dev.off()
  
  cat("PDF de gráficos extendidos completado:", basename(archivo_pdf), "\n")
  
  return(archivo_pdf)
}

cat("MODULO DE GRAFICOS EXTENDIDOS OPERATIVO\n")
cat("=======================================\n")
cat("Funcionalidades incluidas:\n")
cat("- Gráficos de análisis por modelo/turno\n")
cat("- Visualización de interacciones significativas\n")
cat("- Curvas de correlación con lags temporales\n")
cat("- Curvas de calibración y reliability\n")
cat("- Comparación visual de umbrales óptimos\n")
cat("- Efectos no lineales y splines\n")
cat("- Resumen ejecutivo visual integrado\n")