# ============================================================================
# MODULO 00: INICIALIZACION DEL SISTEMA
# Sistema de Analisis Estadistico - Control de Calidad IS
# ============================================================================

cat("INICIALIZANDO SISTEMA DE ANALISIS ESTADISTICO\n")
cat("==============================================\n")

# ============================================================================
# CONFIGURACION DE DIRECTORIOS
# ============================================================================

# Detectar directorio base automaticamente
DIRECTORIO_BASE <- getwd()

# Si estamos en un subdirectorio de scripts, subir un nivel
if(basename(DIRECTORIO_BASE) == "scripts") {
  DIRECTORIO_BASE <- dirname(DIRECTORIO_BASE)
}

# Definir estructura de directorios
DIRECTORIO_DATOS <- file.path(DIRECTORIO_BASE, "datos")
DIRECTORIO_SCRIPTS <- file.path(DIRECTORIO_BASE, "scripts")  
DIRECTORIO_RESULTADOS <- file.path(DIRECTORIO_BASE, "resultados")
DIRECTORIO_GRAFICOS <- file.path(DIRECTORIO_BASE, "graficos")

cat("Directorio base:", DIRECTORIO_BASE, "\n")
cat("Directorio datos:", DIRECTORIO_DATOS, "\n")
cat("Directorio resultados:", DIRECTORIO_RESULTADOS, "\n")
cat("Directorio graficos:", DIRECTORIO_GRAFICOS, "\n")

# ============================================================================
# CREACION DE DIRECTORIOS
# ============================================================================

directorios_crear <- c(DIRECTORIO_RESULTADOS, DIRECTORIO_GRAFICOS)

for(directorio in directorios_crear) {
  if(!dir.exists(directorio)) {
    dir.create(directorio, recursive = TRUE)
    cat("Directorio creado:", directorio, "\n")
  } else {
    cat("Directorio existe:", directorio, "\n")
  }
}

# ============================================================================
# INSTALACION Y CARGA DE LIBRERIAS
# ============================================================================

cat("\nVERIFICANDO E INSTALANDO LIBRERIAS\n")
cat("===================================\n")

# Lista de librerias necesarias
librerias_necesarias <- c(
  "readr",      # Para lectura de CSV con mejor encoding
  "dplyr",      # Para manipulacion de datos
  "ggplot2",    # Para graficos avanzados
  "corrplot",   # Para matrices de correlacion
  "VIM",        # Para analisis de datos faltantes
  "psych"       # Para analisis estadistico descriptivo
)

# Funcion para cargar librerias (sin instalacion automatica)
cargar_libreria_disponible <- function(libreria) {
  if(require(libreria, character.only = TRUE, quietly = TRUE)) {
    cat("✓ Libreria disponible:", libreria, "\n")
    return(TRUE)
  } else {
    cat("⚠ Libreria no disponible:", libreria, "- usando funciones base\n")
    return(FALSE)
  }
}

# Verificar disponibilidad de librerias
cat("Verificando librerias disponibles...\n")
librerias_disponibles <- list()
for(libreria in librerias_necesarias) {
  librerias_disponibles[[libreria]] <- cargar_libreria_disponible(libreria)
}

# Reporte de librerias
cat("\nREPORTE DE LIBRERIAS:\n")
disponibles <- sum(unlist(librerias_disponibles))
cat("Disponibles:", disponibles, "de", length(librerias_necesarias), "\n")
if(disponibles < length(librerias_necesarias)) {
  cat("El sistema funcionará con funciones base de R\n")
}

# ============================================================================
# FUNCIONES AUXILIARES GLOBALES
# ============================================================================

# Crear timestamp para archivos
crear_timestamp <- function() {
  format(Sys.time(), "%Y%m%d_%H%M%S")
}

# Detectar encoding de archivo CSV
detectar_encoding <- function(archivo) {
  muestra <- readLines(archivo, n = 10, warn = FALSE)
  encoding <- "UTF-8"
  
  # Probar diferentes encodings
  encodings_probar <- c("UTF-8", "latin1", "CP1252")
  
  for(enc in encodings_probar) {
    tryCatch({
      test <- iconv(muestra, from = enc, to = "UTF-8")
      if(!any(is.na(test))) {
        encoding <- enc
        break
      }
    }, error = function(e) {
      # Continuar con siguiente encoding
    })
  }
  
  return(encoding)
}

# ============================================================================
# VARIABLES GLOBALES
# ============================================================================

# Estructura de 24 variables esperada
VARIABLES_ESPERADAS <- c(
  # Variables estructurales (4)
  "FH", "MODELO", "SECCION", "CAVIDAD",
  
  # Variables de defecto (4) - Binarias detectadas visualmente
  "CLARO", "PLIEGUEPEQ", "PLIEGUEGR", "DEFECTOS",
  
  # Variables de proceso (6)
  "FLUJO_SPRAY_1", "FLUJO_SPRAY_2", "TEMP_MOLDE_1", 
  "TEMP_MOLDE_2", "PRESION_1", "PRESION_2",
  
  # Variables geometricas (6)
  "XPOS", "YPOS", "XY", "LONGITUD", "DIAMETRO", "VELOCIDAD",
  
  # Variables de calidad (4)
  "OVALIDAD", "ASIMETRIA", "VERTICALIDAD", "GROSOR"
)

cat("\nVARIABLES ESPERADAS DEFINIDAS:", length(VARIABLES_ESPERADAS), "\n")

# ============================================================================
# CONFIGURACION DE OPCIONES R
# ============================================================================

# Configurar opciones para mejor rendimiento
options(stringsAsFactors = FALSE)
options(scipen = 999)  # Evitar notacion cientifica
options(digits = 6)    # Precision numerica

# Configurar locale para fechas en español si es necesario
# Sys.setlocale("LC_TIME", "Spanish")

cat("\nSISTEMA INICIALIZADO CORRECTAMENTE\n")
cat("===================================\n")
cat("Directorio base configurado en:", DIRECTORIO_BASE, "\n")
cat("Librerias cargadas:", length(librerias_necesarias), "\n")
cat("Variables esperadas:", length(VARIABLES_ESPERADAS), "\n")
cat("Sistema listo para procesamiento de datos\n")