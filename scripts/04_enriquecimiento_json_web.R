# ==============================================================================
# SCRIPT 04: ENRIQUECIMIENTO DE JSON CON CONTEXTO HISTÓRICO Y KPIs PARA WEB
# ==============================================================================
setwd("C:/Users/imamo/Desktop/Informatica/DAT-253/GMM")

library(dplyr)
library(jsonlite)

# 1. Cargar el dataset completo generado por el Script 03
df_prep <- read.csv("data/processed/mineria_bolivia_procesado.csv", stringsAsFactors = FALSE)
df_prep$Fecha <- as.Date(df_prep$Fecha)
df_prep$Cluster_GMM <- as.character(df_prep$Cluster_GMM)
df_prep$Anomalia_GMM <- as.logical(df_prep$Anomalia_GMM)
df_prep$Anomalia_LOF <- as.logical(df_prep$Anomalia_LOF)

# 2. Asignar etiquetas de contexto histórico y explicación comercial por evento
df_web <- df_prep %>%
  mutate(
    Regimen_Nombre = case_when(
      Cluster_GMM == "1" ~ "Régimen I: Minería Tradicional (1992-2007)",
      Cluster_GMM == "2" ~ "Régimen II: Expansión Megaproyectos (2008-2018)",
      Cluster_GMM == "3" ~ "Régimen III: Auge Aurífero y Volatilidad (2019-2025)",
      TRUE               ~ paste("Régimen", Cluster_GMM)
    ),
    Evento_Historico = case_when(
      Anio == 2020 & Mes %in% c(4, 5) ~
        "Cuarentena Rígida COVID-19: Paralización operativa simultánea en minería tradicional y exportaciones.",
      Anio == 2020 & Mes == 3 ~
        "Inicio de Emergencia Sanitaria: Primer quiebre abrupto en volúmenes de despacho y transporte.",
      (Anio == 2021 & Mes %in% c(9, 11, 12)) | (Anio == 2022 & Mes == 1) ~
        "Boom Exportador Aurífero: Récord histórico de exportación de oro (> 4.9 TM/mes) desacoplado del zinc/plata.",
      Anio == 2000 & Mes == 3 ~
        "Anomalía Operativa Oculta: Caída abrupta en producción de estaño (471 TM) dentro de un ciclo estable.",
      Anio == 2001 & Mes == 11 ~
        "Choque de Precios Internacionales: Contracción simultánea de estaño y aumento atípico relativo de oro.",
      Anio == 2009 & Mes == 2 ~
        "Crisis Financiera Global y Transición: Desajuste en volúmenes de plata y exportaciones tras caída de commodities.",
      Anio == 2011 & Mes %in% c(6, 8) ~
        "Superciclo de Materias Primas: Picos de cotización y volatilidad en despachos de plata y estaño.",
      Anio == 2019 & Mes %in% c(2, 3, 8, 11) ~
        "Inestabilidad Social y Cambios de Oferta: Variaciones atípicas en producción de zinc y despachos de oro.",
      Anio == 2024 & Mes %in% c(10, 12) ~
        "Máximos Recientes de Minería Tradicional: Récord histórico de estaño (> 2,000 TM) con baja exportación formal de oro.",
      Anomalia_GMM & Anomalia_LOF ~
        "Alerta Crítica de Consenso: Desviación extrema confirmada tanto en probabilidad global como en densidad local.",
      Anomalia_GMM & !Anomalia_LOF ~
        "Desviación de Régimen (GMM): Combinación mineralógica con baja probabilidad frente a su ciclo histórico.",
      !Anomalia_GMM & Anomalia_LOF ~
        "Aislamiento Local (LOF): Registro atípico respecto a sus 15 vecinos más cercanos dentro de su misma época.",
      TRUE ~ "Operación dentro de parámetros históricos normales."
    ),
    Nivel_Severidad = case_when(
      Anomalia_GMM & Anomalia_LOF  ~ "Crítica",
      Anomalia_LOF & LOF_Score > 2 ~ "Alta",
      Anomalia_GMM | Anomalia_LOF  ~ "Moderada",
      TRUE                         ~ "Normal"
    )
  )

# 3. Exportar serie completa enriquecida (407 meses) para el Simulador Interactivo
write.csv(df_web, "data/processed/mineria_bolivia_procesado.csv", row.names = FALSE)
write_json(df_web, "data/processed/mineria_bolivia_procesado.json", pretty = TRUE)

# 4. Exportar tabla filtrada de anomalías enriquecida
tabla_anomalias_web <- df_web %>%
  filter(Diagnostico_Final != "Normal") %>%
  select(
    Fecha, Anio, Mes, Mes_Nombre,
    ESTAÑO_NAC, ZINC_NAC, PLATA_NAC, PLOMO_NAC, ORO_EXP, PART_COOP_ZINC,
    Cluster_GMM, Regimen_Nombre, LogDens_GMM, LOF_Score,
    Diagnostico_Final, Nivel_Severidad, Evento_Historico
  ) %>%
  arrange(desc(LOF_Score))

write.csv(tabla_anomalias_web, "data/processed/anomalias_detectadas.csv", row.names = FALSE)
write_json(tabla_anomalias_web, "data/processed/anomalias_detectadas.json", pretty = TRUE)

# 5. Generar archivo de KPIs y Casos Guiados (resumen_kpi.json)
evento_max <- df_web %>% arrange(desc(LOF_Score)) %>% slice(1)

resumen_kpi <- list(
  meta = list(
    titulo = "Auditoría Inteligente del Sector Minero de Bolivia",
    periodo = "1992 - 2025",
    total_meses = nrow(df_web),
    variables_analizadas = c("ESTAÑO_NAC", "ZINC_NAC", "PLATA_NAC", "ORO_EXP", "PART_COOP_ZINC")
  ),
  metricas = list(
    total_anomalias_unicas = nrow(tabla_anomalias_web),
    alertas_gmm = sum(df_web$Anomalia_GMM),
    alertas_lof = sum(df_web$Anomalia_LOF),
    alertas_consenso = sum(df_web$Anomalia_GMM & df_web$Anomalia_LOF),
    umbral_tukey_oro = round(quantile(df_web$ORO_EXP, 0.75) + 1.5 * IQR(df_web$ORO_EXP), 2),
    umbral_gmm_logdens = round(quantile(df_web$LogDens_GMM, 0.05), 4),
    umbral_lof_default = 1.5,
    k_vecinos_lof = 15,
    componentes_gmm = 3
  ),
  evento_mas_critico = list(
    fecha = as.character(evento_max$Fecha),
    lof_score = evento_max$LOF_Score,
    log_dens_gmm = evento_max$LogDens_GMM,
    descripcion = evento_max$Evento_Historico
  ),
  casos_guiados = list(
    list(
      id = "covid_2020",
      boton = "Caso 1: Parálisis Total (Abr 2020)",
      fecha = "2020-04-01",
      lof_score = df_web$LOF_Score[df_web$Fecha == as.Date("2020-04-01")],
      resumen = "Colapso simultáneo de producción y exportación durante la cuarentena rígida. Detectado por consenso crítico (GMM + LOF)."
    ),
    list(
      id = "estano_2000",
      boton = "Caso 2: Anomalía Oculta (Mar 2000)",
      fecha = "2000-03-01",
      lof_score = df_web$LOF_Score[df_web$Fecha == as.Date("2000-03-01")],
      resumen = "Caída puntual de estaño dentro de una década estable. GMM lo considera normal en su régimen, pero LOF lo aísla por densidad local."
    ),
    list(
      id = "oro_2022",
      boton = "Caso 3: Boom Aurífero (Ene 2022)",
      fecha = "2022-01-01",
      lof_score = df_web$LOF_Score[df_web$Fecha == as.Date("2022-01-01")],
      resumen = "Pico máximo histórico de exportación de oro (6.37 TM finas) frente a volúmenes estables de minería tradicional."
    )
  )
)

write_json(resumen_kpi, "data/processed/resumen_kpi.json", pretty = TRUE, auto_unbox = TRUE)

cat("\n[OK] Script 04 finalizado: JSONs enriquecidos y resumen_kpi.json listos para web.\n")