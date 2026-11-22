# ==============================================================================
# SCRIPT 03: LOCAL OUTLIER FACTOR (LOF), COMPARATIVA PCA Y EXPORTACIÓN FINAL
# ==============================================================================
setwd("C:/Users/imamo/Desktop/Informatica/DAT-253/GMM")

paquetes <- c("dplyr", "ggplot2", "dbscan", "jsonlite")
instalar <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
if (length(instalar) > 0) install.packages(instalar)

library(dplyr)
library(ggplot2)
library(dbscan)
library(jsonlite)

# Cargar datos con resultados GMM del Script 02
estado <- readRDS("data/processed/datos_modelado.rds")
df_prep <- estado$df_prep
X_scaled <- estado$X_scaled

tema_web <- theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.background = element_rect(fill = "#FFFFFF", color = NA),
    plot.title = element_text(face = "bold", color = "#0f172a"),
    plot.subtitle = element_text(color = "#475569")
  )

# 1. Cálculo de Local Outlier Factor (k = 15 vecinos)
df_prep$LOF_Score <- round(lof(X_scaled, minPts = 15), 4)
df_prep$Anomalia_LOF <- df_prep$LOF_Score > 1.5

p3 <- ggplot(df_prep, aes(x = ZINC_NAC, y = ORO_EXP)) +
  geom_point(aes(color = Anomalia_LOF, size = LOF_Score), alpha = 0.85) +
  geom_text(data = subset(df_prep, LOF_Score > 1.65),
            aes(label = format(Fecha, "%Y-%m")), vjust = -0.9, size = 3, fontface = "bold") +
  scale_color_manual(values = c("FALSE" = "#2563eb", "TRUE" = "#ea580c"),
                     labels = c("Normal", "Anomalía LOF (> 1.5)")) +
  scale_size_continuous(range = c(1.8, 5.5)) +
  labs(title = "Local Outlier Factor (LOF): Zinc vs. Exportaciones de Oro",
       subtitle = "Detección de aislamiento por densidad local (dbscan, k = 15 vecinos)",
       x = "Producción Nacional de Zinc (TM)", y = "Exportación de Oro (TM Finas)",
       color = "Estado LOF", size = "Puntaje LOF") +
  tema_web

ggsave("outputs/figures/03_lof_zinc_oro.png", p3, width = 9, height = 5.5, dpi = 300)

# 2. Proyección PCA 2D y Diagnóstico Consolidado (GMM vs LOF)
pca_res <- prcomp(X_scaled, center = FALSE, scale. = FALSE)
df_prep$PC1 <- round(pca_res$x[, 1], 4)
df_prep$PC2 <- round(pca_res$x[, 2], 4)
var_exp <- round(summary(pca_res)$importance[2, 1:2] * 100, 1)

df_prep$Diagnostico_Final <- case_when(
  df_prep$Anomalia_GMM & df_prep$Anomalia_LOF  ~ "Ambos (GMM + LOF)",
  df_prep$Anomalia_GMM & !df_prep$Anomalia_LOF ~ "Solo GMM (Probabilístico)",
  !df_prep$Anomalia_GMM & df_prep$Anomalia_LOF ~ "Solo LOF (Densidad Local)",
  TRUE                                         ~ "Normal"
)

p4 <- ggplot(df_prep, aes(x = PC1, y = PC2, color = Diagnostico_Final)) +
  geom_point(size = 2.8, alpha = 0.85) +
  geom_text(data = subset(df_prep, Diagnostico_Final == "Ambos (GMM + LOF)" | LOF_Score > 2.0),
            aes(label = format(Fecha, "%Y-%m")), vjust = -0.8, size = 3, fontface = "bold") +
  scale_color_manual(values = c(
    "Normal"                    = "#cbd5e1",
    "Solo GMM (Probabilístico)" = "#dc2626",
    "Solo LOF (Densidad Local)" = "#ea580c",
    "Ambos (GMM + LOF)"         = "#7c3aed"
  )) +
  labs(title = "Mapa Espacial PCA: Consenso entre GMM y LOF (1992 - 2025)",
       subtitle = paste0("Varianza explicada: PC1 (", var_exp[1], "%) + PC2 (", var_exp[2], "%) = ", sum(var_exp), "%"),
       x = paste0("Componente Principal 1 (", var_exp[1], "%)"),
       y = paste0("Componente Principal 2 (", var_exp[2], "%)"),
       color = "Diagnóstico") +
  tema_web

ggsave("outputs/figures/04_comparativa_pca_gmm_lof.png", p4, width = 9.5, height = 5.5, dpi = 300)

# 3. Exportación de Datasets Finales (CSV y JSON para tu página web)
tabla_anomalias <- df_prep %>%
  filter(Diagnostico_Final != "Normal") %>%
  select(Fecha, Anio, Mes_Nombre, ESTAÑO_NAC, ZINC_NAC, PLATA_NAC, ORO_EXP,
         PART_COOP_ZINC, Cluster_GMM, LogDens_GMM, LOF_Score, Diagnostico_Final) %>%
  arrange(desc(LOF_Score))

write.csv(df_prep, "data/processed/mineria_bolivia_procesado.csv", row.names = FALSE)
write.csv(tabla_anomalias, "data/processed/anomalias_detectadas.csv", row.names = FALSE)

write_json(df_prep, "data/processed/mineria_bolivia_procesado.json", pretty = TRUE)
write_json(tabla_anomalias, "data/processed/anomalias_detectadas.json", pretty = TRUE)

print(head(tabla_anomalias, 12))
cat("\n[OK] Script 03 finalizado: Gráficos 03-04 y archivos CSV/JSON exportados correctamente.\n")