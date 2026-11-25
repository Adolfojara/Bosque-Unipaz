## ================================================================
## 02. ANÁLISIS PARA EL ARTÍCULO — Bosque UNIPAZ (Barrancabermeja)
## ----------------------------------------------------------------
## Entrada : analisis_articulo/datos/datos_limpios.csv (de 01_depuracion.R)
## Salidas : analisis_articulo/tablas/*.csv, figuras/*.png,
##           resultados_articulo.xlsx y resultados_clave.txt
##
## Bloques:
##   A. Composición florística, IVI (especies) e IVF (familias)
##   B. Diversidad alfa: números de Hill (rarefacción/extrapolación),
##      cobertura de muestreo, estimadores no paramétricos, índices por
##      transecto y comparación entre fragmentos
##   C. Diversidad beta: partición de Baselga (recambio/anidamiento),
##      PERMANOVA, betadisper, NMDS y decaimiento con la distancia
##   D. Estructura horizontal (clases diamétricas) y vertical (estratos)
##   E. Afinidades biogeográficas
## ================================================================

invisible(Sys.setlocale("LC_ALL", "C.UTF-8"))
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(vegan)
  library(readxl); library(writexl); library(geosphere); library(stringr)
})

args <- commandArgs(trailingOnly = FALSE)
f    <- sub("--file=", "", args[grep("--file=", args)])
dir_a <- if (length(f)) normalizePath(dirname(f)) else getwd()
base  <- normalizePath(file.path(dir_a, ".."))
source(file.path(dir_a, "funciones_hill.R"))
dir.create(file.path(dir_a, "tablas"),  showWarnings = FALSE)
dir.create(file.path(dir_a, "figuras"), showWarnings = FALSE)
tab <- function(x, n) write.csv(x, file.path(dir_a, "tablas", paste0(n, ".csv")), row.names = FALSE)
fig <- function(p, n, w = 16, h = 10) ggsave(file.path(dir_a, "figuras", paste0(n, ".png")),
                                             p, width = w, height = h, units = "cm", dpi = 300)
log_txt <- file.path(dir_a, "resultados_clave.txt")
cat("RESULTADOS CLAVE — generado por 02_analisis.R\n\n", file = log_txt)
say <- function(...) { cat(..., "\n", sep = ""); cat(..., "\n", sep = "", file = log_txt, append = TRUE) }

tema <- theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank(), strip.background = element_rect(fill = "grey92"))
col_frag <- c(B1 = "#1b9e77", B2 = "#d95f02", Total = "grey25")

d <- read.csv(file.path(dir_a, "datos", "datos_limpios.csv"), stringsAsFactors = FALSE)
AREA_T  <- 0.01                                       # ha por transecto (100 m2)
n_tr    <- n_distinct(d$transecto)
area_ha <- n_tr * AREA_T
frag_tr <- d %>% distinct(transecto, fragmento) %>% arrange(transecto)
d$genero <- word(d$especie, 1)
es_gen <- !str_detect(d$genero, "aceae$|^Indet|^Moraceae")

## ---------------------------------------------------------------
## A. COMPOSICIÓN
## ---------------------------------------------------------------
say("== A. COMPOSICIÓN ==")
resumen_comp <- function(x, nom) {
  tr <- n_distinct(x$transecto)
  data.frame(Unidad = nom, Transectos = tr, Area_ha = tr * AREA_T,
             Individuos = nrow(x),
             Familias = n_distinct(x$familia[x$familia != "Indeterminada"]),
             Generos = n_distinct(x$genero[!str_detect(x$genero, "aceae$|^Indet")]),
             Especies = n_distinct(x$especie),
             Densidad_ind_ha = round(nrow(x) / (tr * AREA_T)),
             AB_m2_ha = round(sum(x$AB_m2) / (tr * AREA_T), 2))
}
t_comp <- bind_rows(resumen_comp(d, "Total"),
                    resumen_comp(filter(d, fragmento == "B1"), "B1"),
                    resumen_comp(filter(d, fragmento == "B2"), "B2"))
abund <- table(d$especie)
t_comp$Singletons <- c(sum(abund == 1),
                       sum(table(d$especie[d$fragmento == "B1"]) == 1),
                       sum(table(d$especie[d$fragmento == "B2"]) == 1))
t_comp$Doubletons <- c(sum(abund == 2),
                       sum(table(d$especie[d$fragmento == "B1"]) == 2),
                       sum(table(d$especie[d$fragmento == "B2"]) == 2))
tab(t_comp, "T1_resumen_composicion"); print(t_comp)
nivel <- d %>% distinct(especie, familia) %>%
  mutate(nivel = case_when(str_detect(especie, "^Indet") ~ "Indeterminada",
                           str_detect(especie, "aceae") ~ "Familia",
                           str_detect(especie, "sp\\.") ~ "Género",
                           TRUE ~ "Especie")) %>% count(nivel)
say("Nivel de determinación de las morfoespecies: ",
    paste(nivel$nivel, nivel$n, sep = " = ", collapse = "; "))

## IVI de especies (Curtis & McIntosh 1951)
ivi <- d %>% group_by(especie, familia) %>%
  summarise(N = n(), Frec = n_distinct(transecto), AB = sum(AB_m2), .groups = "drop") %>%
  mutate(DeR = 100 * N / sum(N), FrR = 100 * Frec / sum(Frec), DoR = 100 * AB / sum(AB),
         IVI = DeR + FrR + DoR) %>% arrange(desc(IVI))
tab(ivi, "T2_IVI_especies")

## IVF de familias (Mori et al. 1983): densidad + dominancia + diversidad relativas
ivf <- d %>% filter(familia != "Indeterminada") %>% group_by(familia) %>%
  summarise(N = n(), S = n_distinct(especie), AB = sum(AB_m2), .groups = "drop") %>%
  mutate(DeR = 100 * N / sum(N), DivR = 100 * S / sum(S), DoR = 100 * AB / sum(AB),
         IVF = DeR + DivR + DoR) %>% arrange(desc(IVF))
tab(ivf, "T3_IVF_familias")
say("Top 5 IVI: ", paste(sprintf("%s (%.1f)", ivi$especie[1:5], ivi$IVI[1:5]), collapse = "; "))
say("Top 5 IVF: ", paste(sprintf("%s (%.1f)", ivf$familia[1:5], ivf$IVF[1:5]), collapse = "; "))
say("Familias más ricas: ", paste(sprintf("%s (%d)", arrange(ivf, desc(S))$familia[1:6],
                                         arrange(ivf, desc(S))$S[1:6]), collapse = "; "))
say("Especies con 1 transecto: ", sum(ivi$Frec == 1), " de ", nrow(ivi))

p_ivi <- ivi %>% slice(1:15) %>%
  pivot_longer(c(DeR, FrR, DoR), names_to = "comp", values_to = "v") %>%
  mutate(comp = factor(comp, c("DoR", "FrR", "DeR"),
                       c("Dominancia relativa", "Frecuencia relativa", "Densidad relativa"))) %>%
  ggplot(aes(v, reorder(especie, IVI), fill = comp)) + geom_col(width = .75) +
  scale_fill_grey(start = .2, end = .8, name = NULL) +
  labs(x = "Índice de valor de importancia (IVI, máx. 300)", y = NULL) + tema +
  theme(axis.text.y = element_text(face = "italic"), legend.position = "bottom")
fig(p_ivi, "Fig3_IVI", 16, 12)

## ---------------------------------------------------------------
## B. DIVERSIDAD ALFA
## ---------------------------------------------------------------
say("\n== B. DIVERSIDAD ALFA ==")
vec <- function(x) as.vector(table(x$especie))
X <- list(Total = vec(d), B1 = vec(filter(d, fragmento == "B1")),
          B2 = vec(filter(d, fragmento == "B2")))

asin <- bind_rows(lapply(names(X), function(k) cbind(Unidad = k, asintoticos(X[[k]], B = 300))))
asin$Cobertura <- rep(sapply(X, cobertura_obs), each = 3)
asin$Completitud <- asin$Observado / asin$Estimado
tab(asin, "T4_hill_asintoticos"); print(asin)
for (k in names(X)) say(sprintf("%s: n=%d, S=%d, cobertura=%.3f, Chao1=%.1f (IC %.1f-%.1f), completitud q0=%.0f%%",
                               k, sum(X[[k]]), length(X[[k]]), cobertura_obs(X[[k]]),
                               asin$Estimado[asin$Unidad == k & asin$q == 0],
                               asin$LCL[asin$Unidad == k & asin$q == 0],
                               asin$UCL[asin$Unidad == k & asin$q == 0],
                               100 * asin$Completitud[asin$Unidad == k & asin$q == 0]))

## Curvas de rarefacción/extrapolación (hasta 2n del total)
m_max <- 2 * sum(X$Total)
curvas <- bind_rows(lapply(names(X), function(k)
  cbind(Unidad = k, curva_hill(X[[k]], m_max = m_max, n_pts = 50, B = 200))))
tab(curvas, "curvas_hill")

## Comparación estandarizada por tamaño (n del fragmento menor) y por cobertura
n_ref <- min(sum(X$B1), sum(X$B2))
comp_n <- bind_rows(lapply(c("B1", "B2"), function(k) {
  x <- X[[k]]; pb <- comunidad_boot(x); set.seed(2)
  est <- sapply(0:2, function(q) hill_m(x, q, n_ref))
  bt  <- replicate(300, { y <- as.vector(rmultinom(1, sum(x), pb)); y <- y[y > 0]
                          sapply(0:2, function(q) hill_m(y, q, n_ref)) })
  data.frame(Unidad = k, q = 0:2, m = n_ref, qD = est,
             LCL = est - 1.96 * apply(bt, 1, sd), UCL = est + 1.96 * apply(bt, 1, sd))
}))
## cobertura de referencia: mínimo entre las coberturas en 2n (Chao et al. 2014)
C_ref <- min(cobertura_m(X$B1, 2 * sum(X$B1)), cobertura_m(X$B2, 2 * sum(X$B2)))
m_para_C <- function(x, C) uniroot(function(m) cobertura_m(x, round(m)) - C,
                                   c(1, 2 * sum(x)))$root
comp_C <- bind_rows(lapply(c("B1", "B2"), function(k) {
  x <- X[[k]]; m <- round(m_para_C(x, C_ref)); pb <- comunidad_boot(x); set.seed(3)
  est <- sapply(0:2, function(q) hill_m(x, q, m))
  bt  <- replicate(300, { y <- as.vector(rmultinom(1, sum(x), pb)); y <- y[y > 0]
                          sapply(0:2, function(q) hill_m(y, q, m)) })
  data.frame(Unidad = k, q = 0:2, m = m, SC = C_ref, qD = est,
             LCL = est - 1.96 * apply(bt, 1, sd), UCL = est + 1.96 * apply(bt, 1, sd))
}))
tab(comp_n, "T5a_hill_estandarizado_n"); tab(comp_C, "T5b_hill_estandarizado_cobertura")
say("Comparación a n = ", n_ref, " individuos:")
for (i in seq_len(nrow(comp_n))) say(sprintf("  %s q=%d: %.1f (%.1f-%.1f)", comp_n$Unidad[i],
                                           comp_n$q[i], comp_n$qD[i], comp_n$LCL[i], comp_n$UCL[i]))
say("Comparación a cobertura = ", round(C_ref, 3), ":")
for (i in seq_len(nrow(comp_C))) say(sprintf("  %s q=%d (m=%d): %.1f (%.1f-%.1f)", comp_C$Unidad[i],
                                           comp_C$q[i], comp_C$m[i], comp_C$qD[i], comp_C$LCL[i], comp_C$UCL[i]))

curvas$qlab <- factor(paste0("q = ", curvas$q))
obs <- filter(curvas, metodo == "Observado")
p_hill <- ggplot(curvas, aes(m, qD, colour = Unidad, fill = Unidad)) +
  geom_ribbon(aes(ymin = LCL, ymax = UCL), alpha = .15, colour = NA) +
  geom_line(data = filter(curvas, m <= sapply(Unidad, function(u) sum(X[[u]])))) +
  geom_line(data = filter(curvas, m >= sapply(Unidad, function(u) sum(X[[u]]))), linetype = 2) +
  geom_point(data = obs, size = 2, shape = 21, fill = "white") +
  facet_wrap(~qlab, scales = "free_y") +
  scale_colour_manual(values = col_frag, name = NULL) + scale_fill_manual(values = col_frag, name = NULL) +
  labs(x = "Número de individuos", y = "Diversidad (número efectivo de especies)") +
  tema + theme(legend.position = "bottom")
fig(p_hill, "Fig2_curvas_Hill", 17, 8)
p_cov <- ggplot(filter(curvas, q == 0), aes(m, SC, colour = Unidad)) +
  geom_line(data = filter(curvas, q == 0, m <= sapply(Unidad, function(u) sum(X[[u]])))) +
  geom_line(data = filter(curvas, q == 0, m >= sapply(Unidad, function(u) sum(X[[u]]))), linetype = 2) +
  geom_point(data = filter(obs, q == 0), size = 2, shape = 21, fill = "white") +
  scale_colour_manual(values = col_frag, name = NULL) +
  labs(x = "Número de individuos", y = "Cobertura de muestreo") + tema
fig(p_cov, "FigS1_cobertura", 10, 8)

## Basado en muestras (transectos como unidades de muestreo)
mat <- d %>% count(transecto, especie) %>%
  pivot_wider(names_from = especie, values_from = n, values_fill = 0) %>%
  arrange(transecto) %>% as.data.frame()
rownames(mat) <- mat$transecto; mat$transecto <- NULL; mat <- as.matrix(mat)
grp <- frag_tr$fragmento[match(rownames(mat), frag_tr$transecto)]
pool <- rbind(Total = specpool(mat), specpool(mat, grp))
pool$Unidad <- rownames(pool)
tab(pool, "T6_estimadores_incidencia")
say("Estimadores por incidencia (Chao2 / Jack1 / Jack2):")
for (i in seq_len(nrow(pool))) say(sprintf("  %s: Sobs=%d, Chao2=%.1f±%.1f, Jack1=%.1f, Jack2=%.1f",
                                         pool$Unidad[i], pool$Species[i], pool$chao[i], pool$chao.se[i],
                                         pool$jack1[i], pool$jack2[i]))

## Índices por transecto
div_tr <- data.frame(transecto = as.integer(rownames(mat)), fragmento = grp,
                     N = rowSums(mat), S = specnumber(mat),
                     H = diversity(mat, "shannon"), invD = diversity(mat, "invsimpson"),
                     J = diversity(mat, "shannon") / log(specnumber(mat)),
                     alpha_Fisher = suppressWarnings(fisher.alpha(mat)),
                     AB_m2 = tapply(d$AB_m2, d$transecto, sum)[rownames(mat)])
div_tr$D1 <- exp(div_tr$H)
tab(div_tr, "T7_diversidad_por_transecto")
wt <- sapply(c("N", "S", "D1", "invD", "J", "AB_m2"), function(v) {
  w <- wilcox.test(div_tr[[v]] ~ div_tr$fragmento, exact = FALSE)
  c(B1 = median(div_tr[[v]][grp == "B1"]), B2 = median(div_tr[[v]][grp == "B2"]),
    W = unname(w$statistic), p = w$p.value)
})
wt <- data.frame(variable = colnames(wt), t(wt)); tab(wt, "T8_wilcoxon_transectos")
say("Por transecto (medias ± DE): S = ", round(mean(div_tr$S), 1), " ± ", round(sd(div_tr$S), 1),
    "; N = ", round(mean(div_tr$N), 1), " ± ", round(sd(div_tr$N), 1),
    "; H' = ", round(mean(div_tr$H), 2), " ± ", round(sd(div_tr$H), 2),
    "; J = ", round(mean(div_tr$J, na.rm = TRUE), 2), " ± ", round(sd(div_tr$J, na.rm = TRUE), 2))
say("Wilcoxon B1 vs B2 por transecto: ",
    paste(sprintf("%s p=%.3f", wt$variable, wt$p), collapse = "; "))

## ---------------------------------------------------------------
## C. DIVERSIDAD BETA
## ---------------------------------------------------------------
say("\n== C. DIVERSIDAD BETA ==")
pa <- (mat > 0) * 1
beta_multi <- function(P) {           # Baselga (2010), Sørensen multisitio
  P <- P[, colSums(P) > 0, drop = FALSE]
  Si <- sum(rowSums(P)); St <- ncol(P); a <- Si - St
  bij <- outer(1:nrow(P), 1:nrow(P), Vectorize(function(i, j) sum(P[i, ] & !P[j, ])))
  ut <- upper.tri(bij)
  mn <- sum(pmin(bij[ut], t(bij)[ut])); mx <- sum(pmax(bij[ut], t(bij)[ut]))
  c(SOR = (mn + mx) / (2 * a + mn + mx), SIM = mn / (a + mn),
    SNE = (mn + mx) / (2 * a + mn + mx) - mn / (a + mn))
}
## Remuestreo de 10 transectos (tamaño de B2) para comparar fragmentos
set.seed(4)
k_s <- min(table(grp))
rs <- function(P) t(replicate(200, beta_multi(P[sample(nrow(P), k_s), ])))
bm <- list(Total = rs(pa), B1 = rs(pa[grp == "B1", ]), B2 = beta_multi(pa[grp == "B2", ]))
t_beta <- bind_rows(
  data.frame(Unidad = "Total", t(colMeans(bm$Total)), SOR_sd = sd(bm$Total[, 1])),
  data.frame(Unidad = "B1",    t(colMeans(bm$B1)),    SOR_sd = sd(bm$B1[, 1])),
  data.frame(Unidad = "B2",    t(bm$B2),              SOR_sd = NA))
t_beta$Whittaker <- c(ncol(pa) / mean(rowSums(pa)),
                      sum(colSums(pa[grp == "B1", ]) > 0) / mean(rowSums(pa[grp == "B1", ])),
                      sum(colSums(pa[grp == "B2", ]) > 0) / mean(rowSums(pa[grp == "B2", ])))
t_beta$prop_recambio <- t_beta$SIM / t_beta$SOR
tab(t_beta, "T9_beta_Baselga"); print(t_beta)
for (i in 1:3) say(sprintf("%s (10 transectos): βSOR=%.3f, βSIM=%.3f, βSNE=%.3f (recambio = %.0f%%); βWhittaker=%.2f",
                          t_beta$Unidad[i], t_beta$SOR[i], t_beta$SIM[i], t_beta$SNE[i],
                          100 * t_beta$prop_recambio[i], t_beta$Whittaker[i]))
## Entre fragmentos (dos conjuntos)
sp1 <- unique(d$especie[d$fragmento == "B1"]); sp2 <- unique(d$especie[d$fragmento == "B2"])
a <- length(intersect(sp1, sp2)); b <- length(setdiff(sp1, sp2)); cc <- length(setdiff(sp2, sp1))
say(sprintf("Entre fragmentos: compartidas=%d, exclusivas B1=%d, exclusivas B2=%d; Sørensen sim=%.3f; βSIM=%.3f",
            a, b, cc, 2 * a / (2 * a + b + cc), min(b, cc) / (a + min(b, cc))))

## Pares de transectos
pares <- function(P) {
  n <- nrow(P); res <- NULL
  for (i in 1:(n - 1)) for (j in (i + 1):n) {
    A <- sum(P[i, ] & P[j, ]); B <- sum(P[i, ] & !P[j, ]); C <- sum(!P[i, ] & P[j, ])
    res <- rbind(res, data.frame(i = rownames(P)[i], j = rownames(P)[j],
                                 sor = (B + C) / (2 * A + B + C), sim = min(B, C) / (A + min(B, C))))
  }
  res$sne <- res$sor - res$sim; res
}
pp <- pares(pa)
say(sprintf("Pares de transectos: βSOR medio=%.3f, βSIM medio=%.3f, βSNE medio=%.3f",
            mean(pp$sor), mean(pp$sim, na.rm = TRUE), mean(pp$sne, na.rm = TRUE)))

## PERMANOVA y dispersión multivariada
bc  <- vegdist(mat, "bray"); sor <- vegdist(pa, "bray")   # Bray sobre presencia = Sørensen
set.seed(5)
perm_bc  <- adonis2(bc  ~ grp, permutations = 9999)
perm_sor <- adonis2(sor ~ grp, permutations = 9999)
disp_bc  <- permutest(betadisper(bc, grp), permutations = 9999)
disp_sor <- permutest(betadisper(sor, grp), permutations = 9999)
t_perm <- data.frame(Distancia = c("Bray-Curtis", "Sørensen"),
                     F_PERMANOVA = c(perm_bc$F[1], perm_sor$F[1]),
                     R2 = c(perm_bc$R2[1], perm_sor$R2[1]),
                     p_PERMANOVA = c(perm_bc$`Pr(>F)`[1], perm_sor$`Pr(>F)`[1]),
                     F_betadisper = c(disp_bc$tab$F[1], disp_sor$tab$F[1]),
                     p_betadisper = c(disp_bc$tab$`Pr(>F)`[1], disp_sor$tab$`Pr(>F)`[1]))
tab(t_perm, "T10_PERMANOVA"); print(t_perm)
for (i in 1:2) say(sprintf("PERMANOVA %s: F=%.2f, R2=%.3f, p=%.4f | betadisper F=%.2f, p=%.4f",
                          t_perm$Distancia[i], t_perm$F_PERMANOVA[i], t_perm$R2[i], t_perm$p_PERMANOVA[i],
                          t_perm$F_betadisper[i], t_perm$p_betadisper[i]))
sim_mean <- function(D) { m <- as.matrix(D); g <- outer(grp, grp, "==")
  c(dentro = mean(1 - m[upper.tri(m) & g]), entre = mean(1 - m[upper.tri(m) & !g])) }
say("Similitud Bray-Curtis media dentro/entre fragmentos: ",
    paste(round(sim_mean(bc), 3), collapse = " / "))

## NMDS (Bray-Curtis)
set.seed(6)
nm <- metaMDS(mat, "bray", k = 3, trymax = 200, trace = 0)   # k = 2 daba stress 0.26
sc <- as.data.frame(scores(nm, "sites"))[, 1:2]; sc$fragmento <- grp; sc$transecto <- rownames(mat)
say(sprintf("NMDS Bray-Curtis (3 dimensiones): stress = %.3f", nm$stress))
## especies con mayor correlación con los ejes (envfit), solo especies en ≥5 transectos
frecuentes <- colnames(mat)[colSums(pa) >= 5]
ef <- envfit(nm, mat[, frecuentes], permutations = 999, choices = 1:2)
ef_df <- data.frame(especie = frecuentes, ef$vectors$arrows * sqrt(ef$vectors$r),
                    r2 = ef$vectors$r, p = ef$vectors$pvals) %>% arrange(p, desc(r2))
tab(ef_df, "T11_envfit_especies")
top_ef <- ef_df %>% filter(p <= 0.01) %>% slice(1:8)
p_nmds <- ggplot(sc, aes(NMDS1, NMDS2)) +
  stat_ellipse(aes(colour = fragmento), level = .95, linetype = 2) +
  geom_point(aes(colour = fragmento, shape = fragmento), size = 2.5) +
  geom_text(aes(label = transecto), size = 2.3, vjust = -1, colour = "grey30") +
  geom_segment(data = top_ef, aes(0, 0, xend = NMDS1, yend = NMDS2), inherit.aes = FALSE,
               arrow = arrow(length = unit(.15, "cm")), colour = "grey45") +
  geom_text(data = top_ef, aes(NMDS1 * 1.1, NMDS2 * 1.1, label = especie), inherit.aes = FALSE,
            size = 2.4, fontface = "italic", colour = "grey20") +
  scale_colour_manual(values = col_frag, name = "Fragmento") +
  scale_shape_manual(values = c(B1 = 16, B2 = 17), name = "Fragmento") +
  annotate("text", x = Inf, y = -Inf, hjust = 1.1, vjust = -.6, size = 3,
           label = sprintf("Stress = %.2f (k = 3)", nm$stress)) +
  scale_x_continuous(expand = expansion(mult = .3)) + scale_y_continuous(expand = expansion(mult = .15)) +
  tema + coord_equal()

## Decaimiento de la similitud con la distancia (Mantel)
co <- read_excel(file.path(base, "datos_agosto1.xlsx"), sheet = "coordenadas transectos")
names(co) <- c("salida", "transecto", "coord", "bosque", "lat", "lon")
co$lon[co$transecto == 10] <- -73.74722          # error de digitación (-74.74722)
co <- filter(co, !is.na(lat), transecto %in% as.integer(rownames(mat)))
tab(co, "coordenadas_corregidas")
idx <- match(co$transecto, as.integer(rownames(mat)))
geo <- as.dist(distm(cbind(co$lon, co$lat)))
bc_g  <- as.dist(as.matrix(bc)[idx, idx]); sor_g <- as.dist(as.matrix(sor)[idx, idx])
set.seed(7)
man_bc  <- mantel(bc_g,  geo, method = "spearman", permutations = 9999)
man_sor <- mantel(sor_g, geo, method = "spearman", permutations = 9999)
say(sprintf("Mantel (n=%d transectos con coordenadas): Bray-Curtis r=%.3f p=%.4f; Sørensen r=%.3f p=%.4f; distancia máx = %.0f m",
            length(idx), man_bc$statistic, man_bc$signif, man_sor$statistic, man_sor$signif, max(geo)))
dd <- data.frame(dist = as.vector(geo), sim = 1 - as.vector(sor_g))
dlm <- lm(log(sim + 0.01) ~ dist, dd)
p_dd <- ggplot(dd, aes(dist, sim)) + geom_point(alpha = .35, size = 1) +
  geom_smooth(method = "lm", formula = y ~ x, colour = "black", se = TRUE) +
  labs(x = "Distancia geográfica (m)", y = "Similitud de Sørensen",
       subtitle = sprintf("Mantel r = %.2f; p = %.3f", man_sor$statistic, man_sor$signif)) + tema
fig(p_nmds, "Fig4a_NMDS", 12, 11); fig(p_dd, "Fig4b_decaimiento_distancia", 10, 8)

## Mapa esquemático de transectos
p_map <- ggplot(co, aes(lon, lat, colour = paste0("B", bosque), shape = paste0("B", bosque))) +
  geom_point(size = 2.5) + geom_text(aes(label = transecto), size = 2.3, vjust = -1, colour = "grey30") +
  scale_colour_manual(values = col_frag, name = "Fragmento") +
  scale_shape_manual(values = c(B1 = 16, B2 = 17), name = "Fragmento") +
  labs(x = "Longitud (°)", y = "Latitud (°)") + coord_quickmap() + tema
fig(p_map, "Fig1_transectos", 12, 11)

## ---------------------------------------------------------------
## D. ESTRUCTURA
## ---------------------------------------------------------------
say("\n== D. ESTRUCTURA ==")
## Sin Attalea nucifera (acaule, sin DAP) para la estructura diamétrica
ds <- filter(d, !acaule)
say("Individuos con DAP (sin A. nucifera): ", nrow(ds))
cortes <- c(0, 5, 10, 20, 40, Inf)
etq    <- c("<5", "5-10", "10-20", "20-40", "≥40")
ds$clase_DAP <- cut(ds$DAP_cm, cortes, etq, right = FALSE)
d$estrato <- cut(d$altura_ok, c(0, 5, 12, 25, Inf),
                 c("Sotobosque (<5 m)", "Subdosel (5-12 m)", "Dosel (12-25 m)", "Emergente (>25 m)"),
                 right = FALSE)
est_dap <- ds %>% count(fragmento, clase_DAP) %>% group_by(fragmento) %>%
  mutate(ind_ha = n / (n_distinct(d$transecto[d$fragmento == first(fragmento)]) * AREA_T),
         pct = 100 * n / sum(n)) %>% ungroup()
est_tot <- ds %>% count(clase_DAP) %>% mutate(fragmento = "Total", ind_ha = n / area_ha, pct = 100 * n / sum(n))
est_dap <- bind_rows(est_tot, est_dap); tab(est_dap, "T12_clases_diametricas")
ab_cl <- ds %>% group_by(clase_DAP) %>% summarise(AB_ha = sum(AB_m2) / area_ha, .groups = "drop") %>%
  mutate(pct_AB = 100 * AB_ha / sum(AB_ha))
tab(ab_cl, "T12b_area_basal_por_clase")
say("Clases diamétricas (Total, % individuos): ",
    paste(sprintf("%s=%.1f%%", est_tot$clase_DAP, est_tot$pct), collapse = "; "))
say("Área basal por clase (%): ", paste(sprintf("%s=%.1f%%", ab_cl$clase_DAP, ab_cl$pct_AB), collapse = "; "))
say(sprintf("Árboles ≥40 cm DAP: %d individuos (%.1f%% de N) con %.1f%% del área basal",
            sum(ds$DAP_cm >= 40), 100 * mean(ds$DAP_cm >= 40),
            100 * sum(ds$AB_m2[ds$DAP_cm >= 40]) / sum(ds$AB_m2)))
## Ajuste exponencial negativo (J invertida) y cociente de De Liocourt
mid <- c(3.75, 7.5, 15, 30, 60)    # 2.5-5 tomado como inferior
ej  <- data.frame(mid = mid, n = est_tot$ind_ha)
fit <- lm(log(n) ~ mid, ej)
say(sprintf("Exponencial negativa: ln(N/ha) = %.2f %.3f·DAP; R2 = %.3f; cociente q medio = %.2f",
            coef(fit)[1], coef(fit)[2], summary(fit)$r.squared, mean(head(ej$n, -1) / tail(ej$n, -1))))
ks <- suppressWarnings(ks.test(ds$DAP_cm[ds$fragmento == "B1"], ds$DAP_cm[ds$fragmento == "B2"]))
say(sprintf("K-S distribución de DAP B1 vs B2: D=%.3f, p=%.4f", ks$statistic, ks$p.value))
say(sprintf("DAP mediano: B1=%.1f cm, B2=%.1f cm; Altura mediana: B1=%.1f m, B2=%.1f m",
            median(ds$DAP_cm[ds$fragmento == "B1"]), median(ds$DAP_cm[ds$fragmento == "B2"]),
            median(d$altura_ok[d$fragmento == "B1"], na.rm = TRUE),
            median(d$altura_ok[d$fragmento == "B2"], na.rm = TRUE)))
est_v <- d %>% filter(!is.na(estrato)) %>% count(fragmento, estrato) %>% group_by(fragmento) %>%
  mutate(pct = 100 * n / sum(n)) %>% ungroup()
est_v <- bind_rows(d %>% filter(!is.na(estrato)) %>% count(estrato) %>%
                     mutate(fragmento = "Total", pct = 100 * n / sum(n)), est_v)
tab(est_v, "T13_estratos_verticales")
say("Estratos (Total %): ", paste(sprintf("%s=%.1f%%", est_v$estrato[est_v$fragmento == "Total"],
                                          est_v$pct[est_v$fragmento == "Total"]), collapse = "; "))
## especies dominantes por estrato
dom_est <- d %>% filter(!is.na(estrato)) %>% count(estrato, especie) %>%
  group_by(estrato) %>% slice_max(n, n = 3, with_ties = FALSE) %>% ungroup()
tab(dom_est, "T14_especies_por_estrato")
## Relación altura-DAP (no palmas): modelo log-log
hd <- d %>% filter(!palma, !is.na(altura_ok))
m_hd <- lm(log(altura_ok) ~ log(DAP_cm), hd)
say(sprintf("Alometría H-DAP (sin palmas, n=%d): ln H = %.2f + %.2f ln DAP; R2 = %.2f",
            nrow(hd), coef(m_hd)[1], coef(m_hd)[2], summary(m_hd)$r.squared))
say(sprintf("Alturas máximas: %s", paste(sprintf("%s %.0f m", head(arrange(d, desc(altura_ok))$especie, 4),
                                                head(sort(d$altura_ok, TRUE), 4)), collapse = "; ")))
p_dap <- ggplot(filter(est_dap, fragmento != "Total"), aes(clase_DAP, ind_ha, fill = fragmento)) +
  geom_col(position = position_dodge(.8), width = .75) +
  scale_fill_manual(values = col_frag, name = "Fragmento") +
  labs(x = "Clase diamétrica (cm)", y = "Individuos/ha", tag = "A") + tema
p_alt <- ggplot(filter(est_v, fragmento != "Total"), aes(estrato, pct, fill = fragmento)) +
  geom_col(position = position_dodge(.8), width = .75) +
  scale_fill_manual(values = col_frag, name = "Fragmento") +
  labs(x = NULL, y = "Individuos (%)", tag = "B") + tema +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
png(file.path(dir_a, "figuras", "Fig5_estructura.png"), 17, 8, units = "cm", res = 300)
gridExtra::grid.arrange(p_dap, p_alt, ncol = 2)
invisible(dev.off())

## ---------------------------------------------------------------
## E. AFINIDADES BIOGEOGRÁFICAS
## ---------------------------------------------------------------
say("\n== E. BIOGEOGRAFÍA ==")
bg <- read_excel(file.path(base, "datos_agosto1.xlsx"), sheet = "distr_simpl")
bg$Especie <- str_squish(bg$Especie)
fix_bg <- c("Belllucia" = "Bellucia", "Byrsomima" = "Byrsonima", "Clathotropis" = "Clathrotropis",
            "Duguettia" = "Duguetia", "kegelli" = "kegelii", "Manilkara bidenta$" = "Manilkara bidentata",
            "Licaniahypoleuca" = "Licania hypoleuca",
            "^Euterpe precatoria var. longevaginata.*" = "Euterpe longevaginata")
for (p in names(fix_bg)) bg$Especie <- str_replace(bg$Especie, p, fix_bg[[p]])
reg <- names(bg)[4:12]                          # Amazonia ... Magdalena
bg_in <- filter(bg, Especie %in% d$especie)
t_bg <- data.frame(Region = reg, Especies = sapply(reg, function(r) sum(bg_in[[r]] == 1, na.rm = TRUE)))
t_bg$Pct <- 100 * t_bg$Especies / nrow(bg_in)
tab(t_bg, "T15_afinidades_biogeograficas")
say("Especies con información de distribución: ", nrow(bg_in), " de ", nrow(bg),
    " en la hoja (", sum(!bg$Especie %in% d$especie), " no coinciden con el inventario)")
say("Afinidades (% de especies): ", paste(sprintf("%s=%.0f%%", t_bg$Region, t_bg$Pct), collapse = "; "))
solo_mag <- bg_in$Especie[rowSums(bg_in[, setdiff(reg, "Magdalena")] == 1, na.rm = TRUE) == 0]
say("Especies registradas solo en Magdalena (posibles endémicas regionales): ",
    paste(solo_mag, collapse = ", "))

## ---------------------------------------------------------------
## Anexo: lista florística
## ---------------------------------------------------------------
lista <- d %>% group_by(familia, especie) %>%
  summarise(B1 = sum(fragmento == "B1"), B2 = sum(fragmento == "B2"), Total = n(),
            Transectos = n_distinct(transecto), DAP_max = if (all(is.na(DAP_cm))) NA else max(DAP_cm, na.rm = TRUE), .groups = "drop") %>%
  arrange(familia, especie)
tab(lista, "Anexo1_lista_floristica")

write_xlsx(list(Resumen = t_comp, IVI = ivi, IVF = ivf, Hill_asintoticos = asin,
                Hill_n = comp_n, Hill_cobertura = comp_C, Incidencia = pool,
                Div_transecto = div_tr, Wilcoxon = wt, Beta_Baselga = t_beta,
                PERMANOVA = t_perm, Envfit = ef_df, Clases_DAP = est_dap,
                AB_clase = ab_cl, Estratos = est_v, Biogeografia = t_bg, Lista = lista),
           file.path(dir_a, "resultados_articulo.xlsx"))
say("\nListo.")
