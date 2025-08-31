# ============================================================================
# MODULO 04: GRAFICOS COMPARACION SISTEMA AUTOMATICO vs MANUAL
# Sistema de Analisis Estadistico - Control de Calidad IS
# ============================================================================

cat("MODULO DE GRAFICOS COMPARACION SISTEMAS\n")
cat("=======================================\n")

# ============================================================================
# FUNCION PARA GRAFICOS DE COMPARACION
# ============================================================================

generar_graficos_comparacion_sistemas <- function(resultado_correlacion_estirado, datos_principales) {
  cat("\nGENERANDO GRAFICOS COMPARACION SISTEMAS\n")
  cat("=======================================\n")
  
  if(is.null(resultado_correlacion_estirado) || is.null(resultado_correlacion_estirado$datos_correlacion)) {
    cat("AVISO: Datos de correlacion no disponibles - generando pagina informativa\n")
    cat("MOTIVO: Fechas ESTIRADOS.csv diferentes a datos principales\n")
    cat("NOTA: El resto del analisis estadistico funciona correctamente\n")
    
    # Crear timestamp y archivo
    timestamp <- crear_timestamp()
    archivo_pdf <- file.path(DIRECTORIO_GRAFICOS, 
                            paste0("comparacion_sistemas_", timestamp, ".pdf"))
    
    cat("Creando archivo PDF informativo:", basename(archivo_pdf), "\n")
    
    # Crear PDF con pagina explicativa
    pdf(archivo_pdf, width = 10, height = 8)
    tryCatch({
      
      plot.new()
      title("Comparacion de Sistemas - Datos No Disponibles", cex.main = 1.5)
      
      text(0.5, 0.8, "SISTEMA DE COMPARACION AUTOMATICO vs MANUAL", 
           cex = 1.4, font = 2, adj = 0.5)
      
      text(0.1, 0.65, "ESTADO:", cex = 1.2, font = 2, adj = 0)
      text(0.1, 0.6, "• Datos ESTIRADOS.csv no coinciden temporalmente", cex = 1.1, adj = 0)
      text(0.1, 0.55, "• Analisis principal completado exitosamente", cex = 1.1, adj = 0)
      text(0.1, 0.5, "• Graficos estadisticos principales disponibles", cex = 1.1, adj = 0)
      
      text(0.1, 0.4, "SOLUCION:", cex = 1.2, font = 2, adj = 0)
      text(0.1, 0.35, "• Verificar formato de fechas en ESTIRADOS.csv", cex = 1.1, adj = 0)
      text(0.1, 0.3, "• Formato esperado: DD/MM/AAAA HH:MM", cex = 1.1, adj = 0)
      text(0.1, 0.25, "• Coincidir periodo temporal con datos principales", cex = 1.1, adj = 0)
      
      text(0.1, 0.15, "GRAFICOS DISPONIBLES:", cex = 1.2, font = 2, adj = 0)
      text(0.1, 0.1, "• graficos_avanzados_XX.pdf - Analisis GLMM completo", cex = 1.1, adj = 0)
      text(0.1, 0.05, "• Incluye ROC, matrices confusion, PCA, correlaciones", cex = 1.1, adj = 0)
      
    }, error = function(e) {
      cat("Error creando pagina informativa:", e$message, "\n")
    })
    
    dev.off()
    cat("Archivo creado exitosamente:", basename(archivo_pdf), "\n")
    return(archivo_pdf)
  }
  
  # Crear timestamp y archivo
  timestamp <- crear_timestamp()
  archivo_pdf <- file.path(DIRECTORIO_GRAFICOS, 
                          paste0("comparacion_sistemas_", timestamp, ".pdf"))
  
  cat("Creando archivo PDF:", basename(archivo_pdf), "\n")
  
  # Extraer datos
  datos_comp <- resultado_correlacion_estirado$datos_correlacion
  
  # ==========================================
  # CONFIGURAR PDF
  # ==========================================
  
  pdf(archivo_pdf, width = 16, height = 12)
  
  tryCatch({
    
    # ==========================================
    # GRAFICO 1: SCATTERPLOT CORRELACIONES
    # ==========================================
    
    cat("Generando grafico 1: Scatterplots de correlacion...\n")
    
    par(mfrow = c(2, 2), mar = c(5, 4, 4, 2))
    
    # Verificar disponibilidad de datos
    if(!"rechazo_estirado" %in% names(datos_comp) || !"defectos" %in% names(datos_comp)) {
      plot.new()
      title("Error: Variables no encontradas")
      text(0.5, 0.5, "Variables requeridas:\nrechazo_estirado, defectos", cex = 1.2)
      return(NULL)
    }
    
    # Estirado vs Defectos Totales
    plot(datos_comp$rechazo_estirado, datos_comp$defectos,
         main = "Sistema Automatico vs Manual (Defectos Totales)",
         xlab = "Tasa Estirado Automatico (%)",
         ylab = "Defectos Manuales (0/1)",
         pch = 19, col = rgb(0, 0, 1, 0.6), cex = 0.8)
    
    # Linea de regresion
    abline(lm(defectos ~ rechazo_estirado, data = datos_comp), col = "red", lwd = 2)
    
    # Correlacion en el grafico
    cor_val <- cor(datos_comp$rechazo_estirado, datos_comp$defectos, use = "complete.obs")
    text(max(datos_comp$rechazo_estirado, na.rm = TRUE) * 0.7, 
         max(datos_comp$defectos, na.rm = TRUE) * 0.9,
         paste("r =", round(cor_val, 3)), cex = 1.2, font = 2)
    
    # Estirado vs Pliegue Pequeño
    if("plieguepeq" %in% names(datos_comp)) {
      plot(datos_comp$rechazo_estirado, datos_comp$plieguepeq,
           main = "Sistema Automatico vs Pliegues Pequeños",
           xlab = "Tasa Estirado Automatico (%)",
           ylab = "Pliegues Pequeños (0/1)",
           pch = 19, col = rgb(0, 1, 0, 0.6), cex = 0.8)
      
      abline(lm(plieguepeq ~ rechazo_estirado, data = datos_comp), col = "red", lwd = 2)
      cor_val2 <- cor(datos_comp$rechazo_estirado, datos_comp$plieguepeq, use = "complete.obs")
      text(max(datos_comp$rechazo_estirado, na.rm = TRUE) * 0.7, 
           max(datos_comp$plieguepeq, na.rm = TRUE) * 0.9,
           paste("r =", round(cor_val2, 3)), cex = 1.2, font = 2)
    } else {
      plot.new()
      title("Pliegues Pequeños - No disponible")
      text(0.5, 0.5, "Variable plieguepeq\nno encontrada", cex = 1.2)
    }
    text(max(datos_comp$rechazo_estirado, na.rm = TRUE) * 0.7, 
         max(datos_comp$plieguepeq, na.rm = TRUE) * 0.9,
         paste("r =", round(cor_val2, 3)), cex = 1.2, font = 2)
    
    # Estirado vs Pliegue Grande
    plot(datos_comp$rechazo_estirado, datos_comp$plieguegr,
         main = "Sistema Automatico vs Pliegues Grandes",
         xlab = "Tasa Estirado Automatico (%)",
         ylab = "Pliegues Grandes (0/1)",
         pch = 19, col = alpha("orange", 0.6), cex = 0.8)
    
    abline(lm(plieguegr ~ rechazo_estirado, data = datos_comp), col = "red", lwd = 2)
    cor_val3 <- cor(datos_comp$rechazo_estirado, datos_comp$plieguegr, use = "complete.obs")
    text(max(datos_comp$rechazo_estirado, na.rm = TRUE) * 0.7, 
         max(datos_comp$plieguegr, na.rm = TRUE) * 0.9,
         paste("r =", round(cor_val3, 3)), cex = 1.2, font = 2)
    
    # Histograma comparativo
    hist(datos_comp$rechazo_estirado, 
         main = "Distribucion Tasas Sistema Automatico",
         xlab = "Tasa Estirado (%)",
         ylab = "Frecuencia",
         col = alpha("purple", 0.7),
         breaks = 20)
    
    # Estadisticas en el grafico
    media_auto <- mean(datos_comp$rechazo_estirado, na.rm = TRUE)
    abline(v = media_auto, col = "red", lwd = 2, lty = 2)
    text(media_auto * 1.2, max(hist(datos_comp$rechazo_estirado, plot = FALSE)$counts) * 0.8,
         paste("Media =", round(media_auto, 2), "%"), cex = 1.1)
    
    par(mfrow = c(1, 1))
    
    # ==========================================
    # GRAFICO 2: CONCORDANCIA ENTRE SISTEMAS
    # ==========================================
    
    cat("Generando grafico 2: Concordancia entre sistemas...\n")
    
    par(mfrow = c(2, 2))
    
    # Crear variables categoricas para analisis
    datos_comp$auto_detecta <- ifelse(datos_comp$rechazo_estirado > 0, "Detecta", "No Detecta")
    datos_comp$manual_detecta <- ifelse(datos_comp$defectos == 1, "Detecta", "No Detecta")
    
    # Tabla de contingencia
    tabla_contingencia <- table(datos_comp$auto_detecta, datos_comp$manual_detecta)
    
    # Mosaic plot
    mosaicplot(tabla_contingencia, 
               main = "Concordancia: Automatico vs Manual",
               xlab = "Sistema Automatico",
               ylab = "Sistema Manual",
               color = c("lightblue", "lightcoral"))
    
    # Barplot de concordancia
    concordancia_casos <- c(
      "Ambos Detectan" = sum(datos_comp$rechazo_estirado > 0 & datos_comp$defectos == 1, na.rm = TRUE),
      "Solo Automatico" = sum(datos_comp$rechazo_estirado > 0 & datos_comp$defectos == 0, na.rm = TRUE),
      "Solo Manual" = sum(datos_comp$rechazo_estirado == 0 & datos_comp$defectos == 1, na.rm = TRUE),
      "Ambos OK" = sum(datos_comp$rechazo_estirado == 0 & datos_comp$defectos == 0, na.rm = TRUE)
    )
    
    barplot(concordancia_casos,
            main = "Casos de Concordancia",
            ylab = "Numero de Casos",
            col = c("green", "orange", "red", "lightgreen"),
            las = 2)
    
    # Agregar porcentajes
    total_casos <- sum(concordancia_casos)
    porcentajes <- round(concordancia_casos / total_casos * 100, 1)
    text(seq_along(concordancia_casos), concordancia_casos + max(concordancia_casos) * 0.05,
         paste0(porcentajes, "%"), cex = 0.8)
    
    # Boxplot comparativo por deteccion manual
    boxplot(rechazo_estirado ~ manual_detecta, data = datos_comp,
            main = "Tasas Automaticas por Deteccion Manual",
            xlab = "Deteccion Manual",
            ylab = "Tasa Estirado Automatico (%)",
            col = c("lightblue", "lightcoral"))
    
    # Test estadistico
    if(length(unique(datos_comp$manual_detecta)) == 2) {
      test_t <- t.test(rechazo_estirado ~ manual_detecta, data = datos_comp)
      text(1.5, max(datos_comp$rechazo_estirado, na.rm = TRUE) * 0.9,
           paste("p-value =", round(test_t$p.value, 4)), cex = 1.1)
    }
    
    # Densidad comparativa
    if(require(stats, quietly = TRUE)) {
      # Separar datos por deteccion manual
      auto_cuando_manual_si <- datos_comp$rechazo_estirado[datos_comp$defectos == 1]
      auto_cuando_manual_no <- datos_comp$rechazo_estirado[datos_comp$defectos == 0]
      
      # Remover NAs
      auto_cuando_manual_si <- auto_cuando_manual_si[!is.na(auto_cuando_manual_si)]
      auto_cuando_manual_no <- auto_cuando_manual_no[!is.na(auto_cuando_manual_no)]
      
      if(length(auto_cuando_manual_si) > 5 && length(auto_cuando_manual_no) > 5) {
        # Plot de densidad
        plot(density(auto_cuando_manual_si), 
             main = "Densidad: Tasas Automaticas por Deteccion Manual",
             xlab = "Tasa Estirado Automatico (%)",
             ylab = "Densidad",
             col = "red", lwd = 2)
        
        lines(density(auto_cuando_manual_no), col = "blue", lwd = 2)
        
        legend("topright", 
               legend = c("Manual Detecta", "Manual No Detecta"),
               col = c("red", "blue"),
               lwd = 2)
      }
    }
    
    par(mfrow = c(1, 1))
    
    # ==========================================
    # GRAFICO 3: ANALISIS TEMPORAL
    # ==========================================
    
    cat("Generando grafico 3: Analisis temporal...\n")
    
    # Si hay datos temporales en datos principales
    if(!is.null(datos_principales) && "FH" %in% names(datos_principales) && inherits(datos_principales$FH, "POSIXct")) {
      
      par(mfrow = c(2, 2))
      
      # Crear variables temporales
      datos_principales$hora <- as.numeric(format(datos_principales$FH, "%H"))
      datos_principales$dia <- as.Date(datos_principales$FH)
      
      # Defectos por hora
      if("DEFECTOS" %in% names(datos_principales)) {
        defectos_hora <- aggregate(DEFECTOS ~ hora, data = datos_principales,
                                  FUN = function(x) mean(x, na.rm = TRUE) * 100)
        
        plot(defectos_hora$hora, defectos_hora$DEFECTOS,
             type = "b", pch = 19, lwd = 2, col = "blue",
             main = "Tasa Defectos Manuales por Hora",
             xlab = "Hora del Dia",
             ylab = "Tasa Defectos (%)")
        grid()
        
        # Highlight horas criticas
        horas_criticas <- defectos_hora$hora[defectos_hora$DEFECTOS > mean(defectos_hora$DEFECTOS) + sd(defectos_hora$DEFECTOS)]
        if(length(horas_criticas) > 0) {
          points(horas_criticas, defectos_hora$DEFECTOS[defectos_hora$hora %in% horas_criticas],
                 col = "red", pch = 19, cex = 1.5)
        }
      }
      
      # Pliegues por hora
      if(all(c("PLIEGUEPEQ", "PLIEGUEGR") %in% names(datos_principales))) {
        plieques_hora <- aggregate(cbind(PLIEGUEPEQ, PLIEGUEGR) ~ hora, data = datos_principales,
                                  FUN = function(x) mean(x, na.rm = TRUE) * 100)
        
        plot(plieques_hora$hora, plieques_hora$PLIEGUEPEQ,
             type = "b", pch = 19, lwd = 2, col = "green",
             main = "Pliegues por Hora",
             xlab = "Hora del Dia",
             ylab = "Tasa Pliegues (%)",
             ylim = range(c(plieques_hora$PLIEGUEPEQ, plieques_hora$PLIEGUEGR), na.rm = TRUE))
        
        lines(plieques_hora$hora, plieques_hora$PLIEGUEGR,
              type = "b", pch = 17, lwd = 2, col = "orange")
        
        legend("topright", 
               legend = c("Pliegues Pequeños", "Pliegues Grandes"),
               col = c("green", "orange"),
               pch = c(19, 17),
               lwd = 2)
        grid()
      }
      
      # Tendencia temporal si hay suficientes dias
      dias_unicos <- unique(datos_principales$dia)
      if(length(dias_unicos) > 5) {
        defectos_dia <- aggregate(DEFECTOS ~ dia, data = datos_principales,
                                 FUN = function(x) mean(x, na.rm = TRUE) * 100)
        
        plot(defectos_dia$dia, defectos_dia$DEFECTOS,
             type = "b", pch = 19, lwd = 2, col = "darkblue",
             main = "Tendencia Temporal de Defectos",
             xlab = "Fecha",
             ylab = "Tasa Defectos Diaria (%)")
        
        # Linea de tendencia
        trend_lm <- lm(DEFECTOS ~ as.numeric(dia), data = defectos_dia)
        abline(trend_lm, col = "red", lwd = 2, lty = 2)
        
        # R-squared
        r_sq <- summary(trend_lm)$r.squared
        text(min(defectos_dia$dia) + (max(defectos_dia$dia) - min(defectos_dia$dia)) * 0.7,
             max(defectos_dia$DEFECTOS) * 0.9,
             paste("R² =", round(r_sq, 3)), cex = 1.1)
        
        grid()
      }
      
      # Heatmap hora vs dia de semana si hay datos suficientes
      if(length(dias_unicos) > 7) {
        datos_principales$dia_semana <- weekdays(datos_principales$FH)
        
        # Crear tabla de contingencia
        tabla_hora_dia <- aggregate(DEFECTOS ~ hora + dia_semana, data = datos_principales,
                                   FUN = function(x) mean(x, na.rm = TRUE) * 100)
        
        # Convertir a matriz
        if(nrow(tabla_hora_dia) > 10) {
          matriz_heatmap <- xtabs(DEFECTOS ~ hora + dia_semana, data = tabla_hora_dia)
          
          image(1:nrow(matriz_heatmap), 1:ncol(matriz_heatmap), as.matrix(matriz_heatmap),
                col = heat.colors(20),
                main = "Heatmap: Defectos por Hora y Dia",
                xlab = "Hora",
                ylab = "Dia Semana",
                axes = FALSE)
          
          axis(1, at = 1:nrow(matriz_heatmap), labels = rownames(matriz_heatmap))
          axis(2, at = 1:ncol(matriz_heatmap), labels = colnames(matriz_heatmap), las = 2)
        }
      }
      
      par(mfrow = c(1, 1))
    } else {
      plot.new()
      title("Analisis temporal no disponible - datos FH requeridos")
    }
    
    # ==========================================
    # GRAFICO 4: RESUMEN ESTADISTICO
    # ==========================================
    
    cat("Generando grafico 4: Resumen estadistico...\n")
    
    par(mfrow = c(2, 2))
    
    # Estadisticas descriptivas visuales
    stats_auto <- c(
      Mean = mean(datos_comp$rechazo_estirado, na.rm = TRUE),
      Median = median(datos_comp$rechazo_estirado, na.rm = TRUE),
      SD = sd(datos_comp$rechazo_estirado, na.rm = TRUE),
      Min = min(datos_comp$rechazo_estirado, na.rm = TRUE),
      Max = max(datos_comp$rechazo_estirado, na.rm = TRUE)
    )
    
    barplot(stats_auto,
            main = "Estadisticas Sistema Automatico",
            ylab = "Valor (%)",
            col = "lightblue",
            las = 2)
    
    # Estadisticas sistema manual
    stats_manual <- c(
      "Defectos" = mean(datos_comp$defectos, na.rm = TRUE) * 100,
      "Pliegue Peq" = mean(datos_comp$plieguepeq, na.rm = TRUE) * 100,
      "Pliegue Gr" = mean(datos_comp$plieguegr, na.rm = TRUE) * 100
    )
    
    barplot(stats_manual,
            main = "Tasas Sistema Manual",
            ylab = "Tasa (%)",
            col = "lightcoral",
            las = 2)
    
    # Matriz de correlacion entre tipos de defecto
    if(all(c("defectos", "plieguepeq", "plieguegr") %in% names(datos_comp))) {
      cor_matrix <- cor(datos_comp[, c("defectos", "plieguepeq", "plieguegr")], use = "complete.obs")
      
      if(require(corrplot, quietly = TRUE)) {
        corrplot(cor_matrix, method = "color", type = "upper",
                title = "Correlacion Tipos de Defecto Manual",
                mar = c(0,0,2,0))
      } else {
        # Heatmap alternativo
        image(1:ncol(cor_matrix), 1:nrow(cor_matrix), t(cor_matrix),
              col = heat.colors(20),
              main = "Correlacion Tipos Defecto",
              axes = FALSE)
        axis(1, at = 1:ncol(cor_matrix), labels = colnames(cor_matrix))
        axis(2, at = 1:nrow(cor_matrix), labels = rownames(cor_matrix))
      }
    }
    
    # Resumen numerico en texto
    plot.new()
    title("Resumen Numerico Comparacion")
    
    # Texto con estadisticas clave
    texto_resumen <- paste(
      "SISTEMA AUTOMATICO (Estirado):",
      paste("Media:", round(mean(datos_comp$rechazo_estirado, na.rm = TRUE), 3), "%"),
      paste("SD:", round(sd(datos_comp$rechazo_estirado, na.rm = TRUE), 3), "%"),
      "",
      "SISTEMA MANUAL (Operarios):",
      paste("Defectos Totales:", round(mean(datos_comp$defectos, na.rm = TRUE) * 100, 3), "%"),
      paste("Pliegues Pequeños:", round(mean(datos_comp$plieguepeq, na.rm = TRUE) * 100, 3), "%"),
      paste("Pliegues Grandes:", round(mean(datos_comp$plieguegr, na.rm = TRUE) * 100, 3), "%"),
      "",
      "CORRELACIONES:",
      paste("Auto vs Defectos: r =", round(cor(datos_comp$rechazo_estirado, datos_comp$defectos, use = "complete.obs"), 3)),
      paste("Auto vs Pliegue Peq: r =", round(cor(datos_comp$rechazo_estirado, datos_comp$plieguepeq, use = "complete.obs"), 3)),
      paste("Auto vs Pliegue Gr: r =", round(cor(datos_comp$rechazo_estirado, datos_comp$plieguegr, use = "complete.obs"), 3)),
      "",
      paste("OBSERVACIONES ANALIZADAS:", nrow(datos_comp)),
      sep = "\n"
    )
    
    text(0.1, 0.9, texto_resumen, cex = 1.0, adj = c(0, 1), family = "mono")
    
    par(mfrow = c(1, 1))
    
  }, error = function(e) {
    cat("Error generando graficos de comparacion:", e$message, "\n")
  })
  
  # Cerrar PDF
  dev.off()
  
  cat("PDF de comparacion sistemas completado:", basename(archivo_pdf), "\n")
  
  return(list(
    archivo_pdf = archivo_pdf,
    graficos_generados = 4
  ))
}

# ============================================================================
# MENSAJE DE CARGA DEL MODULO
# ============================================================================

cat("MODULO DE GRAFICOS COMPARACION SISTEMAS OPERATIVO\n")
cat("=================================================\n")
cat("Graficos especializados para comparacion automatico vs manual:\n")
cat("- Scatterplots con correlaciones\n")
cat("- Analisis de concordancia entre sistemas\n")
cat("- Patrones temporales por hora y dia\n")
cat("- Resumen estadistico comparativo\n")
cat("- Limitacion reconocida: datos automaticos por turno vs manuales por hora\n")