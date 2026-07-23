## ================================================================
## ANÁLISIS DE DATOS DE VEGETACIÓN — DATOS INDIVIDUALES POR ÁRBOL
## ----------------------------------------------------------------
## Formato de entrada: una fila por individuo
## Columnas: parcela, especie, individuo, DAP_cm, altura_m
## ----------------------------------------------------------------
## Módulos:
##   1. Carga y exploración
##   2. Agregación: abundancia y área basal reales por especie/parcela
##   3. Valor de Importancia (IVI) — área basal sumada árbol por árbol
##   4. Estructura forestal: distribución de DAP y altura
##   5. Índices de diversidad (Shannon, Simpson, Riqueza, Equidad)
##   6. Índices de similitud (Bray-Curtis, Jaccard)
##   7. Ordenación: NMDS, PCA, CA
##   8. Clasificación jerárquica (clustering Ward)
##   9. Exportación a Excel
## ================================================================

# ---------------------------------------------------------------
# 0. PAQUETES
# ---------------------------------------------------------------
paquetes <- c("vegan", "ggplot2", "dplyr", "tidyr",

              "ggrepel", "RColorBrewer", "writexl", "gridExtra")

instalar <- paquetes[!paquetes %in% installed.packages()[, "Package"]]
if (length(instalar) > 0)
  install.packages(instalar, repos = "https://cloud.r-project.org")

suppressPackageStartupMessages(lapply(paquetes, library, character.only = TRUE))
cat("✔ Paquetes cargados\n\n")

# ---------------------------------------------------------------
# 1. CARGA DE DATOS
# ---------------------------------------------------------------
# Una fila por individuo. Columnas requeridas:
#   parcela    → identificador de parcela o transecto
#   especie    → nombre de la especie
#   individuo  → número o código del individuo (opcional, para trazabilidad)
#   DAP_cm     → diámetro a la altura del pecho en cm
#   altura_m   → altura total en metros

datos <- read.csv("/Users/orlandoadolfojara/Library/Mobile Documents/com~apple~CloudDocs/Bosque-Unipaz/vegetacion_individual_prop.csv",


                  sep = ",", encoding = "UTF-8")

# Si tu CSV usa punto y coma (Excel en español):
# datos <- read.csv("data/vegetacion_individual.csv", sep = ";")

cat("=== Vista previa ===\n")
print(head(datos, 8))
cat("\nParcelas  :", length(unique(datos$parcela)), "\n")
cat("Especies  :", length(unique(datos$especie)),  "\n")
cat("Individuos:", nrow(datos), "\n\n")

# Metadata: una fila por parcela
meta <- datos %>%
  distinct(parcela) %>%
  tibble::column_to_rownames("parcela")

n_parc <- nrow(meta)

# ---------------------------------------------------------------
# 2. AGREGACIÓN POR ESPECIE × PARCELA
# ---------------------------------------------------------------
# Área basal calculada árbol por árbol (correcto ecológicamente)
datos <- datos %>%
  mutate(area_basal_m2 = (pi / 4) * (DAP_cm / 100)^2)

# Tabla resumen: abundancia real + área basal sumada
resumen_sp <- datos %>%
  group_by(parcela, especie) %>%
  summarise(
    abundancia   = n(),                      # conteo real de individuos
    AB_parcela   = sum(area_basal_m2),       # área basal total (m²)
    DAP_medio    = round(mean(DAP_cm), 2),   # DAP promedio (solo referencia)
    altura_media = round(mean(altura_m), 2), # altura promedio (referencia)
    .groups = "drop"
  )

cat("=== Tabla agregada (primeras filas) ===\n")
print(head(resumen_sp, 10))
cat("\n")

# ---------------------------------------------------------------
# 3. VALOR DE IMPORTANCIA (IVI)
# ---------------------------------------------------------------
cat("=== 3. Valor de Importancia (IVI) ===\n")

ivi_df <- resumen_sp %>%
  group_by(especie) %>%
  summarise(
    N_total  = sum(abundancia),
    Fr_abs   = n_distinct(parcela),
    AB_total = sum(AB_parcela),
    .groups = "drop"
  ) %>%
  mutate(
    Dens_rel = N_total  / sum(N_total)  * 100,
    Frec_rel = Fr_abs   / n_parc        * 100,
    Dom_rel  = AB_total / sum(AB_total) * 100,
    IVI      = Dens_rel + Frec_rel + Dom_rel
  ) %>%
  arrange(desc(IVI))

print(ivi_df)

p_ivi <- ggplot(slice_max(ivi_df, IVI, n = 12),
                aes(x = reorder(especie, IVI), y = IVI, fill = IVI)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(IVI, 1)), hjust = -0.1, size = 3.2) +
  scale_fill_gradient(low = "#a8d5a2", high = "#1a6b3c") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title    = "Valor de Importancia (IVI)",
       subtitle = "Área basal calculada individuo por individuo",
       x = NULL, y = "IVI") +
  theme_minimal(base_size = 12) +
  theme(plot.title  = element_text(face = "bold"),
        axis.text.y = element_text(face = "italic"))

ggsave("IVI_especies.png", p_ivi, width = 8, height = 5, dpi = 150)
print(p_ivi)
cat("✔ IVI_especies.png\n\n")

# ---------------------------------------------------------------
# 4. ESTRUCTURA FORESTAL
# ---------------------------------------------------------------
cat("=== 4. Estructura forestal ===\n")

# Clases diamétricas (cada 10 cm)
datos <- datos %>%
  mutate(clase_DAP = cut(DAP_cm,
                         breaks = seq(0, ceiling(max(DAP_cm)/10)*10, by = 10),
                         right  = FALSE,
                         labels = paste0(seq(0, ceiling(max(DAP_cm)/10)*10 - 10, by = 10),
                                         "-",
                                         seq(10, ceiling(max(DAP_cm)/10)*10, by = 10))))

# Distribución diamétrica (curva de J invertida esperada en bosques)
p_dap <- ggplot(datos, aes(x = clase_DAP)) +
  geom_bar(fill = "#2b8a3e", color = "white", alpha = 0.85) +
  labs(title    = "Distribución de clases diamétricas",
       subtitle = "Estructura de la comunidad arbórea",
       x = "Clase DAP (cm)", y = "Número de individuos") +
  theme_minimal(base_size = 12) +
  theme(plot.title  = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("estructura_DAP.png", p_dap, width = 8, height = 5, dpi = 150)
print(p_dap)
cat("✔ estructura_DAP.png\n\n")

# DAP vs Altura por hábitat
p_dap_alt <- ggplot(datos, aes(x = DAP_cm, y = altura_m)) +
  geom_point(color = "#1a6b3c", alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, color = "#2b8a3e",
              linewidth = 0.8, alpha = 0.15) +
  labs(title  = "Relación DAP — Altura",
       x = "DAP (cm)", y = "Altura (m)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave("DAP_vs_altura.png", p_dap_alt, width = 7, height = 5, dpi = 150)
print(p_dap_alt)
cat("✔ DAP_vs_altura.png\n\n")

# ---------------------------------------------------------------
# 5. MATRICES DE COMUNIDAD
# ---------------------------------------------------------------
# Abundancia
mat_abund <- resumen_sp %>%
  select(parcela, especie, abundancia) %>%
  pivot_wider(names_from = especie, values_from = abundancia,
              values_fill = 0) %>%
  tibble::column_to_rownames("parcela")


# Área basal
mat_ab <- resumen_sp %>%
  select(parcela, especie, AB_parcela) %>%
  pivot_wider(names_from = especie, values_from = AB_parcela,
              values_fill = 0) %>%
  tibble::column_to_rownames("parcela")

# Presencia-Ausencia
mat_pa <- (mat_abund > 0) * 1L

# Alinear metadata
meta <- meta[rownames(mat_abund), , drop = FALSE]

cat("Matriz comunidad:", nrow(mat_abund), "parcelas ×",
    ncol(mat_abund), "especies\n\n")

# ---------------------------------------------------------------
# 6. ÍNDICES DE DIVERSIDAD
# ---------------------------------------------------------------
cat("=== 5. Diversidad α ===\n")

div_df <- data.frame(
  parcela   = rownames(mat_abund),
  N_total   = rowSums(mat_abund),
  riqueza   = specnumber(mat_abund),
  shannon   = round(diversity(mat_abund, index = "shannon"),  3),
  simpson   = round(diversity(mat_abund, index = "simpson"),  3),
  equidad_J = round(diversity(mat_abund) / log(specnumber(mat_abund)), 3),
  AB_total  = round(rowSums(mat_ab), 4)
)
print(div_df)

resumen_div <- div_df %>%
  summarise(
    n        = n(),
    N_media  = round(mean(N_total),  1),
    S_media  = round(mean(riqueza),  2),
    H_media  = round(mean(shannon),  3),
    D_media  = round(mean(simpson),  3),
    J_media  = round(mean(equidad_J),3),
    AB_media = round(mean(AB_total), 4)
  )
cat("\nResumen general:\n")
print(resumen_div)

p_div <- div_df %>%
  pivot_longer(c(riqueza, shannon, simpson, equidad_J),
               names_to = "indice", values_to = "valor") %>%
  mutate(indice = factor(indice,
    levels = c("riqueza","shannon","simpson","equidad_J"),
    labels = c("Riqueza (S)","Shannon (H')","Simpson (1-D)","Equidad (J)"))) %>%
  ggplot(aes(x = parcela, y = valor)) +
  geom_col(fill = "#2b8a3e", alpha = 0.8) +
  facet_wrap(~indice, scales = "free_y", ncol = 2) +
  labs(title = "Diversidad α por parcela",
       x = "Parcela", y = "Valor") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("diversidad_alpha.png", p_div, width = 9, height = 6, dpi = 150)
print(p_div)
cat("✔ diversidad_alpha.png\n\n")

# ---------------------------------------------------------------
# 7. SIMILITUD
# ---------------------------------------------------------------
cat("=== 6. Similitud entre parcelas ===\n")

dist_bc  <- vegdist(mat_abund, method = "bray")
dist_jac <- vegdist(mat_pa,    method = "jaccard")

cat("Bray-Curtis:\n"); print(round(as.matrix(dist_bc), 3))
cat("\nJaccard:\n");    print(round(as.matrix(dist_jac), 3))

make_heatmap <- function(dist_mat, titulo) {
  as.matrix(dist_mat) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("P1") %>%
    pivot_longer(-P1, names_to = "P2", values_to = "valor") %>%
    ggplot(aes(x = P1, y = P2, fill = valor)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(valor, 2)), size = 3) +
    scale_fill_gradient2(low = "#2b8a3e", mid = "#fff9c4",
                         high = "#c0392b", midpoint = 0.5, limits = c(0,1)) +
    labs(title = titulo, x = NULL, y = NULL, fill = "Disimilitud") +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title  = element_text(face = "bold"))
}

p_sim <- gridExtra::grid.arrange(
  make_heatmap(dist_bc,  "Bray-Curtis (abundancia)"),
  make_heatmap(dist_jac, "Jaccard (presencia-ausencia)"),
  ncol = 2)
ggsave("similitud_matrices.png", p_sim, width = 12, height = 5, dpi = 150)
cat("✔ similitud_matrices.png\n\n")

# ---------------------------------------------------------------
# 8. ORDENACIÓN
# ---------------------------------------------------------------
plot_ord <- function(df_sit, df_spp, ex, ey, lx, ly, titulo, stress = NULL) {
  p <- ggplot(df_sit, aes_string(x = ex, y = ey)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray80") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray80") +
    geom_point(size = 4, alpha = 0.85, color = "#1a6b3c") +
    geom_text_repel(aes(label = parcela), size = 3.2,
                    color = "#1a6b3c", max.overlaps = 20) +
    geom_text_repel(data = df_spp,
                    aes_string(x = ex, y = ey, label = "especie"),
                    inherit.aes = FALSE, size = 2.6,
                    color = "gray35", fontface = "italic", max.overlaps = 20) +
    labs(title = titulo, x = lx, y = ly) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))
  if (!is.null(stress))
    p <- p + annotate("text", x = Inf, y = -Inf,
                      label = paste("Stress =", round(stress, 3)),
                      hjust = 1.1, vjust = -0.5, size = 3.5, color = "gray40")
  p
}

## NMDS
cat("=== 7a. NMDS ===\n")
set.seed(123)
nmds <- metaMDS(mat_abund, distance = "bray", k = 2,
                trymax = 200, trace = FALSE)
cat("Stress:", round(nmds$stress, 4),
    ifelse(nmds$stress < 0.10, "— excelente",
    ifelse(nmds$stress < 0.20, "— aceptable", "— revisar k")), "\n")

df_ns <- scores(nmds, "sites") %>% as.data.frame() %>%
  tibble::rownames_to_column("parcela")
df_nsp <- scores(nmds, "species") %>% as.data.frame() %>%
  tibble::rownames_to_column("especie")

p_nmds <- plot_ord(df_ns, df_nsp, "NMDS1","NMDS2","NMDS1","NMDS2",
                   "NMDS — Composición florística (Bray-Curtis)", nmds$stress)
ggsave("NMDS.png", p_nmds, width = 8, height = 7, dpi = 150)
print(p_nmds); cat("✔ NMDS.png\n\n")

## PCA (sobre área basal)
# Se usa prcomp() porque cuando n_parcelas < n_especies, rda() no puede
# extraer scores de especies correctamente
cat("=== 7b. PCA (área basal) ===\n")
pca     <- prcomp(mat_ab, scale. = TRUE)
eig_pca <- summary(pca)$importance[2, 1:2] * 100

df_ps <- as.data.frame(pca$x[, 1:2]) %>%
  tibble::rownames_to_column("parcela")

df_psp <- as.data.frame(pca$rotation[, 1:2]) %>%
  tibble::rownames_to_column("especie")

p_pca <- plot_ord(df_ps, df_psp, "PC1","PC2",
                  paste0("PC1 (", round(eig_pca[1],1), "%)"),
                  paste0("PC2 (", round(eig_pca[2],1), "%)"),
                  "PCA — Área basal por especie") +
  geom_segment(data = df_psp,
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               inherit.aes = FALSE,
               arrow = arrow(length = unit(0.2,"cm")),
               color = "gray55", alpha = 0.6)
ggsave("PCA.png", p_pca, width = 8, height = 7, dpi = 150)
print(p_pca); cat("✔ PCA.png\n\n")

## CA
cat("=== 7c. CA ===\n")
ca     <- cca(mat_abund)
eig_ca <- summary(eigenvals(ca))[2, 1:2] * 100

df_cs <- scores(ca, display = "sites", choices = 1:2) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("parcela")

df_csp <- scores(ca, display = "species", choices = 1:2) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("especie")

p_ca <- plot_ord(df_cs, df_csp, "CA1","CA2",
                 paste0("CA1 (", round(eig_ca[1],1), "%)"),
                 paste0("CA2 (", round(eig_ca[2],1), "%)"),
                 "CA — Análisis de Correspondencia")
ggsave("CA.png", p_ca, width = 8, height = 7, dpi = 150)
print(p_ca); cat("✔ CA.png\n\n")

# ---------------------------------------------------------------
# 9. CLUSTERING
# ---------------------------------------------------------------
cat("=== 8. Clustering jerárquico ===\n")
clust <- hclust(dist_bc, method = "ward.D2")
k     <- 3
grupos <- cutree(clust, k = k)
cat("Grupos (k =", k, "):\n"); print(grupos)

png("clustering_dendrograma.png", width = 700, height = 500, res = 100)
par(mar = c(2,1,3,8))
plot(as.dendrogram(clust), horiz = TRUE,
     main = paste0("Clustering Ward (Bray-Curtis) — k = ", k))
rect.hclust(clust, k = k,
            border = RColorBrewer::brewer.pal(max(k, 3), "Set1"))
dev.off()
plot(as.dendrogram(clust), horiz = TRUE,
     main = paste0("Clustering Ward (Bray-Curtis) — k = ", k))
rect.hclust(clust, k = k,
            border = RColorBrewer::brewer.pal(max(k, 3), "Set1"))
cat("✔ clustering_dendrograma.png\n\n")

# NMDS con grupos
df_ns$grupo <- factor(grupos[df_ns$parcela])

p_nmds_gr <- ggplot(df_ns,
                    aes(x = NMDS1, y = NMDS2, color = grupo)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray80") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray80") +
  stat_ellipse(aes(group = grupo), level = 0.8,
               linetype = "dashed", linewidth = 0.8) +
  geom_point(size = 4, alpha = 0.85) +
  geom_text_repel(aes(label = parcela), size = 3.5, show.legend = FALSE) +
  scale_color_brewer(palette = "Set1") +
  labs(title    = "NMDS con grupos del clustering",
       subtitle = paste0("Stress = ", round(nmds$stress, 3),
                         "  |  k = ", k, " grupos (Ward, Bray-Curtis)"),
       color = "Grupo") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
ggsave("NMDS_grupos.png", p_nmds_gr, width = 8, height = 7, dpi = 150)
print(p_nmds_gr); cat("✔ NMDS_grupos.png\n\n")

# ---------------------------------------------------------------
# 10. EXPORTAR A EXCEL
# ---------------------------------------------------------------
cat("=== 9. Exportando resultados ===\n")

write_xlsx(list(
  "Datos_individuales" = datos,
  "Resumen_sp_parcela" = resumen_sp,
  "IVI"                = ivi_df,
  "Diversidad"         = div_df,
  "Resumen_general"    = resumen_div,
  "BrayCurtis"         = as.data.frame(as.matrix(dist_bc)) %>%
                           tibble::rownames_to_column("parcela"),
  "Jaccard"            = as.data.frame(as.matrix(dist_jac)) %>%
                           tibble::rownames_to_column("parcela"),
  "Clustering"         = data.frame(
                           parcela = names(grupos),
                           grupo   = grupos)
), "resultados_vegetacion.xlsx")

cat("✔ resultados_vegetacion.xlsx\n")
cat("\n", strrep("=", 50), "\n", sep="")
cat("¡ANÁLISIS COMPLETO!\n")
cat(strrep("=", 50), "\n")
cat("Archivos generados:\n")
cat("  • IVI_especies.png\n")
cat("  • estructura_DAP.png\n")
cat("  • DAP_vs_altura.png\n")
cat("  • diversidad_alpha.png\n")
cat("  • similitud_matrices.png\n")
cat("  • NMDS.png  |  PCA.png  |  CA.png\n")
cat("  • clustering_dendrograma.png\n")
cat("  • NMDS_grupos.png\n")
cat("  • resultados_vegetacion.xlsx\n")
