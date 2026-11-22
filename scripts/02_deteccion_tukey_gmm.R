# ==============================================================================
# SCRIPT 02: DETECCIÓN UNIVARIADA (TUKEY IQR) Y MEZCLAS GAUSSIANAS (GMM)
# ==============================================================================
setwd("C:/Users/imamo/Desktop/Informatica/DAT-253/GMM")

paquetes <- c("dplyr", "ggplot2", "mclust")
instalar <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
if (length(instalar) > 0) install.packages(instalar)

library(dplyr)
library(ggplot2)
library(mclust)

# Cargar datos preprocesados desde Script 01
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

# 1. Criterio de Tukey (IQR) en Exportaciones de Oro
Q1 <- quantile(df_prep$ORO_EXP, 0.25)
Q3 <- quantile(df_prep$ORO_EXP, 0.75)
lim_sup_oro <- Q3 + 1.5 * (Q3 - Q1)
df_prep$Outlier_Tukey_Oro <- df_prep$ORO_EXP > lim_sup_oro

p1 <- ggplot(df_prep, aes(x = "Exportaciones de Oro (1992-2025)", y = ORO_EXP)) +
  geom_boxplot(outlier.shape = NA, fill = "#e2e8f0", color = "#1e3a8a", width = 0.35) +
  geom_jitter(aes(color = Outlier_Tukey_Oro), width = 0.12, size = 2.2, alpha = 0.85) +
  scale_color_manual(values = c("FALSE" = "#2563eb", "TRUE" = "#dc2626"),
                     labels = c("Normal", "Atípico (> Q3 + 1.5*IQR)")) +
  labs(title = "Valores Atípicos Univariados: Exportación de Oro en Bolivia",
       subtitle = paste0("Criterio de Tukey | Umbral superior = ", round(lim_sup_oro, 2), " TM Finas"),
       x = "", y = "Toneladas Finas", color = "Estado") +
  tema_web

ggsave("outputs/figures/01_tukey_boxplot_oro.png", p1, width = 8, height = 5, dpi = 300)

# 2. Mezclas Gaussianas (GMM) con mclust (G = 3 regímenes)
set.seed(123)
gmm_fit <- Mclust(X_scaled, G = 3)
print(summary(gmm_fit))

df_prep$Cluster_GMM <- as.factor(predict(gmm_fit)$classification)
df_prep$LogDens_GMM <- round(dens(modelName = gmm_fit$modelName,
                                  data = X_scaled,
                                  parameters = gmm_fit$parameters,
                                  logarithm = TRUE), 4)

umbral_gmm <- quantile(df_prep$LogDens_GMM, 0.05)
df_prep$Anomalia_GMM <- df_prep$LogDens_GMM < umbral_gmm

p2 <- ggplot(df_prep, aes(x = ZINC_NAC, y = PLATA_NAC)) +
  geom_point(aes(color = Anomalia_GMM, shape = Cluster_GMM), size = 2.8, alpha = 0.85) +
  geom_text(data = subset(df_prep, Anomalia_GMM & (ZINC_NAC < 15000 | PLATA_NAC > 135)),
            aes(label = format(Fecha, "%Y-%m")), vjust = -0.8, size = 3, fontface = "bold") +
  scale_color_manual(values = c("FALSE" = "#2563eb", "TRUE" = "#dc2626"),
                     labels = c("Normal", "Anomalía GMM (< P5)")) +
  labs(title = "Mezclas Gaussianas (GMM): Producción de Zinc vs. Plata",
       subtitle = "Detección basada en baja densidad de probabilidad conjunta (mclust, G=3)",
       x = "Producción Nacional de Zinc (TM)", y = "Producción Nacional de Plata (TM)",
       color = "Estado GMM", shape = "Clúster GMM") +
  tema_web

ggsave("outputs/figures/02_gmm_zinc_plata.png", p2, width = 9, height = 5.5, dpi = 300)

# Actualizar estado para el Script 03
estado$df_prep <- df_prep
saveRDS(estado, "data/processed/datos_modelado.rds")

cat("\n[OK] Script 02 finalizado: Gráficos 01 y 02 guardados en outputs/figures/\n")