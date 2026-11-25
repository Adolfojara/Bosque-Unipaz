## ================================================================
## 01. DEPURACIÓN DE DATOS — Bosque UNIPAZ (Barrancabermeja)
## ----------------------------------------------------------------
## Fuente: datos_agosto1.xlsx (hoja "estructura", 745 registros)
##         + vegetacion_individual_prop.csv (determinaciones revisadas)
## Salida: analisis_articulo/datos/datos_limpios.csv
##
## Decisiones de depuración (ver README del análisis):
##  1. Las determinaciones revisadas del CSV reemplazan a las del Excel
##     (las primeras 727 filas coinciden fila a fila en transecto, DAP y
##     altura). Los 18 registros del transecto 30 que faltaban en el CSV
##     se recuperan del Excel.
##  2. Se corrigen errores tipográficos en nombres científicos.
##  3. DAP de individuos con varios tallos: DAP equivalente
##     DAPeq = sqrt(sum(DAP_i^2)) y área basal = suma de áreas por tallo.
##     El Excel calculaba DAP = NoTallos * CAP1 / pi, que sobreestima.
##     Si hay más tallos que columnas de CAP, los tallos sin medir
##     toman el promedio de los medidos.
##  4. El transecto 21 pertenece al bosque 1 (confirmado en campo; la hoja
##     "estructura" lo asignaba al bosque 2).
##  5. Attalea nucifera es una palma acaule y sus "tallos" son pecíolos:
##     se excluye de DAP y área basal (ver abajo).
##  6. Alturas inverosímiles (relación altura/DAP > 400) se marcan como NA
##     solo para el análisis de estructura vertical.
## ================================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(stringr)
})

args <- commandArgs(trailingOnly = FALSE)
f    <- sub("--file=", "", args[grep("--file=", args)])
base <- if (length(f)) normalizePath(file.path(dirname(f), "..")) else getwd()
dir.create(file.path(base, "analisis_articulo", "datos"), showWarnings = FALSE)

xl  <- read_excel(file.path(base, "datos_agosto1.xlsx"), sheet = "estructura")
csv <- read.csv(file.path(base, "vegetacion_individual_prop.csv"),
                stringsAsFactors = FALSE)

stopifnot(all(xl$TRANSECTO[1:727] == csv$parcela))

## 1. Nombre: CSV revisado para las filas 1-727, Excel para el resto
xl$especie <- str_squish(xl$Especie)
xl$especie[1:727] <- str_squish(csv$especie)

## 2. Correcciones tipográficas
fix <- c("Belllucia"   = "Bellucia",   "Byrsomima"  = "Byrsonima",
         "Clathotropis" = "Clathrotropis", "Duguettia" = "Duguetia",
         "Olaceaceae"  = "Olacaceae",  "Compsouneura" = "Compsoneura",
         "Leonia trindra" = "Leonia triandra",
         "^Euterpe kalbreyeri$" = "Euterpe longevaginata",   # nombre indicado por A. Jara
         "Manilkara bidenta$" = "Manilkara bidentata",
         "Machaerium kegelli" = "Machaerium kegelii",
         "^clusia" = "Clusia", "^sorocea" = "Sorocea",
         "^Otoba$" = "Otoba sp.1", "^Strychnos$" = "Strychnos sp.1",
         "^Indet\\.(\\d)" = "Indet. \\1")
for (p in names(fix)) xl$especie <- str_replace(xl$especie, p, fix[[p]])
xl$especie[is.na(xl$especie) | xl$especie %in% c("", "#N/A")] <- "Moraceae sp. (t5-4)"

## Familia: la del Excel, corregida para los nombres revisados
xl$familia <- str_to_title(str_squish(xl$Familia))
fam_gen <- c(Byrsonima = "Malpighiaceae", Helianthostylis = "Moraceae",
             Trichospermum = "Malvaceae", Heisteria = "Olacaceae",
             Posoqueria = "Rubiaceae", Rudgea = "Rubiaceae",
             Strychnos = "Loganiaceae", Maytenus = "Celastraceae",
             Amaioua = "Rubiaceae")
gen <- word(xl$especie, 1)
xl$familia[gen %in% names(fam_gen)] <- fam_gen[gen[gen %in% names(fam_gen)]]
xl$familia[str_detect(gen, "aceae$")] <- gen[str_detect(gen, "aceae$")]
xl$familia[xl$especie == "Violaceae gen-ign. sp.2"] <- "Violaceae"
xl$familia[str_detect(xl$especie, "^Indet")] <- "Indeterminada"
xl$familia[xl$familia == "Indet"] <- "Indeterminada"
xl$familia[xl$especie == "Moraceae sp. (t5-4)"] <- "Moraceae"

## 3. DAP equivalente y área basal por individuo
caps <- c("CAP (cm)", "CAP2(cm)", "CAP3(cm)", "CAP4(cm)", "CAP5(cm)", "CAP6(cm)")
M  <- as.matrix(xl[, caps])
nt <- xl$NoTallos
dap_eq <- ab <- numeric(nrow(xl))
for (i in seq_len(nrow(xl))) {
  d <- M[i, !is.na(M[i, ])] / pi                 # DAP de cada tallo medido
  if (nt[i] > length(d)) d <- c(d, rep(mean(d), nt[i] - length(d)))
  d <- d[seq_len(max(nt[i], 1))]
  dap_eq[i] <- sqrt(sum(d^2))
  ab[i]     <- sum(pi / 4 * (d / 100)^2)
}

datos <- tibble(
  transecto = xl$TRANSECTO,
  fragmento = paste0("B", ifelse(xl$TRANSECTO == 21, 1, xl$Bosque)),  # T21 = bosque 1 (confirmado por A. Jara)
  salida    = xl$SALIDA,
  codigo    = xl$`No Campo`,
  familia   = xl$familia,
  especie   = xl$especie,
  n_tallos  = nt,
  altura_m  = xl$`Altura (m)`,
  DAP_excel = round(xl$`DAP (cm)`, 2),
  DAP_cm    = round(dap_eq, 2),
  AB_m2     = ab,
  palma     = xl$familia == "Arecaceae",
  acaule    = xl$especie == "Attalea nucifera"
) %>%
  ## Attalea nucifera es acaule: los "tallos" medidos son pecíolos (confirmado
  ## por A. Jara). Se conserva como individuo en composición y diversidad, pero
  ## sin DAP ni área basal.
  mutate(DAP_cm = ifelse(acaule, NA, DAP_cm),
         AB_m2  = ifelse(acaule, 0, AB_m2),
         altura_ok = ifelse(!is.na(DAP_cm) & altura_m / (DAP_cm / 100) > 400, NA, altura_m))

write.csv(datos, file.path(base, "analisis_articulo", "datos", "datos_limpios.csv"),
          row.names = FALSE)

cat("Registros:", nrow(datos), "| Especies:", n_distinct(datos$especie),
    "| Familias:", n_distinct(datos$familia[datos$familia != "Indeterminada"]),
    "| Transectos:", n_distinct(datos$transecto), "\n")
cat("Alturas marcadas como NA:", sum(is.na(datos$altura_ok)), "\n")
cat("Área basal total (m2) Excel vs corregida:",
    round(sum(pi/4*(datos$DAP_excel/100)^2), 3), "vs", round(sum(datos$AB_m2), 3), "\n")
