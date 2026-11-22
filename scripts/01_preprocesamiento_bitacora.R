# ==============================================================================
# SCRIPT 01: INGESTA, PREPROCESAMIENTO Y BITÁCORA DE MINERÍA DE DATOS
# ==============================================================================
setwd("C:/Users/imamo/Desktop/Informatica/DAT-253/GMM")

paquetes <- c("readxl", "dplyr", "tidyr", "jsonlite")
instalar <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
if (length(instalar) > 0) install.packages(instalar)

library(readxl)
library(dplyr)
library(tidyr)
library(jsonlite)

# 1. Función de limpieza para cuadros jerárquicos Año/Mes del INE Bolivia
meses_es <- c("Enero" = 1, "Febrero" = 2, "Marzo" = 3, "Abril" = 4,
              "Mayo" = 5, "Junio" = 6, "Julio" = 7, "Agosto" = 8,
              "Septiembre" = 9, "Octubre" = 10, "Noviembre" = 11, "Diciembre" = 12)

limpiar_excel_ine <- function(ruta_archivo, hoja, saltar_filas) {
  raw <- read_excel(ruta_archivo, sheet = hoja, skip = saltar_filas)
  raw <- raw[, colSums(!is.na(raw)) > 0]
  names(raw) <- trimws(names(raw))
  
  anio_actual <- NA
  lista_filas <- list()
  idx <- 1
  
  for (i in seq_len(nrow(raw))) {
    periodo <- trimws(as.character(raw$PERIODO[i]))
    if (is.na(periodo) || periodo == "") next
    periodo_limpio <- trimws(gsub("\\(p\\)", "", periodo))
    
    if (grepl("^[0-9]{4}$", periodo_limpio)) {
      anio_actual <- as.integer(periodo_limpio)
    } else if (periodo_limpio %in% names(meses_es) && !is.na(anio_actual)) {
      fila_df <- data.frame(
        Anio = anio_actual,
        Mes = unname(meses_es[periodo_limpio]),
        Mes_Nombre = periodo_limpio,
        stringsAsFactors = FALSE
      )
      for (col in names(raw)[-1]) {
        fila_df[[col]] <- suppressWarnings(as.numeric(raw[[col]][i]))
      }
      lista_filas[[idx]] <- fila_df
      idx <- idx + 1
    }
  }
  res <- bind_rows(lista_filas)
  res$Fecha <- as.Date(sprintf("%04d-%02d-01", res$Anio, res$Mes))
  return(res)
}

# 2. Lectura desde data/raw/
f_nac  <- "data/raw/1. Bolivia - Produccion Nacional de Minerales por Tipo de Mineral segun Año y Mes 1990 - 2025.xlsx"
f_coop <- "data/raw/4. Bolivia - Produccion de Minerales del Sector de la Mineria Chica y Cooperativas por Tipo de Mineral segun Año y Mes 1990 - 2025.xlsx"
f_oro  <- "data/raw/5. Bolivia - Exportaciones  de Oro  segun Mes 1992 - 2025.xlsx"

df_nac  <- limpiar_excel_ine(f_nac, "M01", 3)
df_coop <- limpiar_excel_ine(f_coop, "M04", 7)
df_oro  <- limpiar_excel_ine(f_oro, "M01", 3) %>% rename(ORO_EXP = VOLUMEN)

df_integrado <- df_nac %>%
  inner_join(df_coop, by = c("Anio", "Mes", "Mes_Nombre", "Fecha"), suffix = c("_NAC", "_COOP")) %>%
  left_join(df_oro %>% select(Anio, Mes, ORO_EXP), by = c("Anio", "Mes"))

# 3. Construcción de Bitácora de Preprocesamiento
bitacora <- data.frame(
  Paso = character(), Accion_Tecnica = character(),
  Filas_Antes = integer(), Filas_Despues = integer(),
  Columnas = integer(), Justificacion = character(),
  stringsAsFactors = FALSE
)

registrar <- function(log_df, paso, accion, f_antes, f_desp, cols, justif) {
  rbind(log_df, data.frame(
    Paso = paso, Accion_Tecnica = accion,
    Filas_Antes = f_antes, Filas_Despues = f_desp,
    Columnas = cols, Justificacion = justif,
    stringsAsFactors = FALSE
  ))
}

bitacora <- registrar(bitacora, "01. Ingesta e Integración",
                      "Parseo Año/Mes y unión relacional de 3 cuadros INE",
                      510, nrow(df_integrado), ncol(df_integrado),
                      "Se extraen solo observaciones mensuales (1990-2025) descartando subtotales anuales.")

na_oro <- sum(is.na(df_integrado$ORO_EXP))
df_sin_na <- df_integrado %>% filter(!is.na(ORO_EXP))
bitacora <- registrar(bitacora, "02. Limpieza de Nulos (NA)",
                      "Filtrado de periodo común completo (1992-2025)",
                      nrow(df_integrado), nrow(df_sin_na), ncol(df_sin_na),
                      paste0("Se excluyen ", na_oro, " meses sin reporte en serie de oro (1990-1991 y 1 NA)."))

ceros_cadmio <- sum(df_integrado$CADMIO == 0, na.rm = TRUE)
df_sin_ruido <- df_sin_na %>% select(-CADMIO)
bitacora <- registrar(bitacora, "03. Selección de Variables",
                      "Eliminación de columna CADMIO",
                      nrow(df_sin_na), nrow(df_sin_ruido), ncol(df_sin_ruido),
                      paste0("91% de ceros (", ceros_cadmio, "/432); evita matrices singulares en GMM."))

df_prep <- df_sin_ruido %>%
  mutate(
    PART_COOP_ZINC   = round(ZINC_COOP / ZINC_NAC, 4),
    PART_COOP_ESTANO = round(ESTAÑO_COOP / ESTAÑO_NAC, 4),
    LOG_ORO_EXP      = log1p(ORO_EXP)
  )
bitacora <- registrar(bitacora, "04. Feature Engineering y Log1p",
                      "Ratios Cooperativas/Nacional y log1p(ORO_EXP)",
                      nrow(df_sin_ruido), nrow(df_prep), ncol(df_prep),
                      "Controla multicolinealidad (r > 0.90) y reduce asimetría positiva del oro (Skewness = 1.54).")

# 4. Estandarización Z-Score y guardado de matrices base
vars_modelo <- c("ESTAÑO_NAC", "ZINC_NAC", "PLATA_NAC", "LOG_ORO_EXP", "PART_COOP_ZINC")
X_scaled <- scale(df_prep[, vars_modelo])

bitacora <- registrar(bitacora, "05. Normalización Z-Score",
                      "Centrado (media=0) y escalado (sd=1) en 5 variables",
                      nrow(df_prep), nrow(X_scaled), ncol(X_scaled),
                      "Iguala magnitudes entre toneladas de Zinc y Oro para distancias en LOF.")

saveRDS(list(df_prep = df_prep, X_scaled = X_scaled, vars_modelo = vars_modelo),
        "data/processed/datos_modelado.rds")
write.csv(bitacora, "data/processed/bitacora_preprocesamiento.csv", row.names = FALSE)
write_json(bitacora, "data/processed/bitacora_preprocesamiento.json", pretty = TRUE)

print(bitacora)
cat("\n[OK] Script 01 finalizado: Bitácora y matriz estandarizada guardadas en data/processed/\n")