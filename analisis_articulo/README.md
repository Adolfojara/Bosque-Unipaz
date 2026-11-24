# Análisis para el artículo (Caldasia)

Orden de ejecución, desde la raíz del repositorio:

```
Rscript analisis_articulo/01_depuracion.R   # datos/datos_limpios.csv
Rscript analisis_articulo/02_analisis.R     # tablas/, figuras/, resultados_articulo.xlsx, resultados_clave.txt
python3 manuscrito/construir_docx.py        # manuscrito/manuscrito_caldasia.docx
```

Paquetes de R: vegan, dplyr, tidyr, ggplot2, readxl, writexl, geosphere, stringr, gridExtra.
Las curvas de Hill (rarefacción/extrapolación, cobertura, Chao1, bootstrap) están en
`funciones_hill.R` siguiendo a Chao et al. (2014); reproducen los valores del ejemplo de iNEXT.

## Decisiones de depuración

- Fuente: hoja `estructura` de `datos_agosto1.xlsx` (745 registros). Para las primeras 727 filas se usan
  las determinaciones revisadas de `vegetacion_individual_prop.csv` (coinciden fila a fila en transecto,
  DAP y altura). Los 18 registros del transecto 30 faltaban en el CSV y se recuperan del Excel.
- Errores tipográficos corregidos: Belllucia, Byrsomima, Clathotropis, Duguettia, Olaceaceae, Compsouneura,
  Leonia trindra, Manilkara bidenta, Machaerium kegelli, clusia/sorocea en minúscula, "Otoba", "Strychnos".
- El registro sin nombre (t5-4, "moraceae" en campo) se trata como morfoespecie "Moraceae sp. (t5-4)".
- **DAP de individuos con varios tallos:** el Excel calculaba DAP = NoTallos × CAP1 / π (suma de
  circunferencias, sobreestima). Ahora DAP equivalente = √(Σ DAPᵢ²) y área basal = Σ áreas por tallo.
  El área basal total baja de 12,40 a 10,76 m².
- Cuatro alturas inverosímiles (altura/DAP > 400, p. ej. *Tapirira* de 44 m con 4,5 cm de DAP) se excluyen
  solo del análisis de estratos.
- Longitud del transecto 10 corregida (−74,747 → −73,747). Transectos 19 y 21 sin coordenadas.

## Datos confirmados en campo (A. Jara)

- El transecto 21 pertenece al bosque 1 (la hoja `estructura` lo asignaba al 2). Corregido en `01_depuracion.R`.
- El CAP de 500 cm de *Lecythis mesophylla* es correcto.
- *Attalea nucifera* es acaule: los "tallos" medidos son pecíolos. Se conserva en composición y diversidad,
  pero sin DAP ni área basal (queda fuera de clases diamétricas y su dominancia relativa es 0).
