# Detección de Anomalías en el Sector Minero de Bolivia (1992 – 2025)

Proyecto de **Minería de Datos (DAT-253)** — Carrera de Informática, Universidad Mayor de San Andrés (UMSA).  
Pipeline reproducible en **R** para la auditoría, limpieza y detección de valores atípicos multivariantes sobre 33 años de series oficiales de producción y exportación minera de Bolivia.

---

## Objetivo y Metodología

El proyecto combina técnicas estadísticas univariadas, probabilísticas y basadas en densidad local para identificar choques operativos, cambios de régimen productivo y desviaciones atípicas en los principales minerales estratégicos de Bolivia (**Estaño, Zinc, Plata, Plomo y Oro**):

1. **Criterio de Tukey (IQR):** Detección univariada de picos extremos en las exportaciones auríferas.
2. **Mezclas Gaussianas (`GMM` con `mclust`):** Modelado probabilístico no supervisado mediante Expectation-Maximization (EM) para segmentar los regímenes estructurales de la minería boliviana e identificar observaciones de baja densidad conjunta.
3. **Local Outlier Factor (`LOF` con `dbscan`):** Detección de anomalías locales basada en la densidad de los $k = 15$ vecinos más cercanos, capaz de aislar caídas o picos abruptos dentro de un mismo ciclo histórico.

---

## Arquitectura del Repositorio

```text
GMM_LOF/
├── data/
│   ├── raw/                             # Cuadros estadísticos originales del INE Bolivia (.xlsx)
│   └── processed/                       # Datasets auditados, bitácora y anomalías (.csv, .json, .rds)
│       ├── bitacora_preprocesamiento.csv
│       ├── bitacora_preprocesamiento.json
│       ├── mineria_bolivia_procesado.csv
│       ├── mineria_bolivia_procesado.json
│       ├── anomalias_detectadas.csv
│       └── anomalias_detectadas.json
├── scripts/
│   ├── 01_preprocesamiento_bitacora.R   # Ingesta, parseo Año/Mes, bitácora, feature engineering y Z-score
│   ├── 02_deteccion_tukey_gmm.R         # Análisis univariado Tukey (IQR) y Mezclas Gaussianas (GMM)
│   └── 03_deteccion_lof_comparativa.R   # Cálculo LOF (k=15), proyección PCA 2D y exportación JSON/CSV
├── outputs/
│   └── figures/                         # Visualizaciones generadas en alta resolución (300 DPI)
│       ├── 01_tukey_boxplot_oro.png
│       ├── 02_gmm_zinc_plata.png
│       ├── 03_lof_zinc_oro.png
│       └── 04_comparativa_pca_gmm_lof.png
└── README.md