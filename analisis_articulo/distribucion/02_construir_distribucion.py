"""Construye la tabla de distribución verificada de las especies del inventario.

Uso (desde la raíz del repositorio, después de 01_consultar_fuentes.py):
    python3 analisis_articulo/distribucion/02_construir_distribucion.py

Entradas
  consultas/catalogo.json     Fichas del Catálogo de Plantas y Líquenes de Colombia
                              (Bernal et al. 2016): regiones, departamentos, elevación.
  consultas/gbif.json         GBIF: registros por departamento, en el Alto Magdalena
                              y por país.
  distr_simpl_original.csv    Hoja distr_simpl de datos_agosto1.xlsx (tabla original).
  ../datos/datos_limpios.csv  Inventario depurado.

Salidas
  distribucion_verificada.csv Una fila por especie: 0/1 por región y columnas de
                              soporte (fuente, elevación, alertas).
  distribucion_cambios.csv    Una fila por especie y región: valor original, valor
                              nuevo, estado (confirmado / agregado / quitado / ...)
                              y fuente.

Esquema: regiones biogeográficas del Catálogo, con el Valle del Magdalena dividido
en Alto Magdalena (aguas arriba de Honda, Tolima, ~5,2° N) y Magdalena medio y bajo.

Reglas
  * Región presente (1) si el Catálogo la lista para la especie.
  * Alto Magdalena = 1 si el Catálogo incluye Valle del Magdalena y lista Huila o
    Tolima, o si GBIF tiene >= 2 registros en el fondo del valle al sur de Honda
    (lat 1,5-5,2° N, lon -75,7 a -74,4°, elevación 0-1000 m).
  * Magdalena medio y bajo = 1 para todas: todas fueron recolectadas en
    Barrancabermeja (este inventario, con ejemplares de respaldo).
  * GBIF por departamento solo genera alertas (gbif_sugiere) cuando hay >= 3
    registros en departamentos propios de una región que el Catálogo no lista.
"""
import json, pathlib
import pandas as pd

d = pathlib.Path(__file__).resolve().parent
REG_CAT = ["Amazonia", "Andes", "Guayana y Serranía de La Macarena", "Islas Caribeñas",
           "Llanura del Caribe", "Orinoquia", "Pacífico", "Sierra Nevada de Santa Marta",
           "Valle del Cauca"]
REG = REG_CAT + ["Alto Magdalena", "Magdalena medio y bajo"]
VIEJA = {"Amazonia": "Amazonia", "Andes": "Andes", "Imerí/Guyana": "Guayana y Serranía de La Macarena",
         "Caribe": "Llanura del Caribe", "Orinoquia/sabana": "Orinoquia", "Chocó": "Pacífico",
         "V del Cauca": "Valle del Cauca", "Magdalena": "Magdalena medio y bajo"}
DEP_REG = {"Amazonia": ["Amazonas", "Caquetá", "Putumayo", "Guaviare", "Vaupés", "Guainía"],
           "Orinoquia": ["Casanare", "Arauca", "Vichada"],
           "Pacífico": ["Chocó"],
           "Llanura del Caribe": ["Atlántico", "Sucre", "Córdoba"]}
DUDOSAS = {  # notas de revisión a partir del Catálogo y GBIF
    "Elvasia elvasioides": "El Catálogo indica que el registro para Colombia se basa en una identificación errónea; revisar el ejemplar.",
    "Eugenia floribunda": "No está en el Catálogo y GBIF no tiene registros en Colombia; revisar la determinación.",
    "Aniba robusta": "El Catálogo la da solo para los Andes (1500-2200 m); dudosa para un bosque de 100 m.",
    "Guatteria cf. goudotiana": "El Catálogo la da solo para los Andes (1200-2100 m); revisar la determinación.",
    "Pseudomalmea cf. boyacana": "El Catálogo la da solo para los Andes; revisar la determinación.",
    "Pseudoxandra cf. pacifica": "El Catálogo la da solo para el Pacífico; revisar la determinación.",
}

cat = json.loads((d / "consultas" / "catalogo.json").read_text())
gb = json.loads((d / "consultas" / "gbif.json").read_text())
inv = pd.read_csv(d.parent / "datos" / "datos_limpios.csv")
fam = inv.drop_duplicates("especie").set_index("especie").familia

old = pd.read_csv(d / "distr_simpl_original.csv")
old["Especie"] = (old.Especie.astype(str).str.split().str.join(" ")
                  .str.replace("Belllucia", "Bellucia").str.replace("Byrsomima", "Byrsonima")
                  .str.replace("Clathotropis", "Clathrotropis").str.replace("Duguettia", "Duguetia")
                  .str.replace("kegelli", "kegelii").str.replace("Licaniahypoleuca", "Licania hypoleuca")
                  .str.replace("trindra", "triandra").str.replace("Compsouneura", "Compsoneura")
                  .str.replace(r"^Euterpe precatoria var\. longevaginata.*", "Euterpe longevaginata", regex=True))
old = old.drop_duplicates("Especie").set_index("Especie")


def lista(s):
    return [t.strip() for t in (s or "").split(",") if t.strip()]


filas, cambios = [], []
for esp in sorted(cat):
    c, g = cat[esp], gb.get(esp, {})
    regs, deps = lista(c.get("Región Biogeográfica")), lista(c.get("Departamentos"))
    gdep = g.get("departamentos", {}) or {}
    alto_g = g.get("alto_magdalena")
    encontrado = c.get("encontrado") and not c.get("mal_identificado")
    f_cat = "Catálogo" if encontrado else ("Catálogo: identificación errónea" if c.get("mal_identificado") else "sin dato en el Catálogo")
    val, fuente = {}, {}
    for r in REG_CAT:
        val[r] = int(r in regs); fuente[r] = f_cat
    mag = "Valle del Magdalena" in regs
    alto_cat = mag and any(p in deps for p in ("Huila", "Tolima"))
    alto_gb = alto_g is not None and alto_g >= 2
    val["Alto Magdalena"] = int(alto_cat or alto_gb)
    partes = (["Catálogo (Huila/Tolima)"] if alto_cat else []) + ([f"GBIF ({alto_g} registros)"] if alto_gb else [])
    fuente["Alto Magdalena"] = "; ".join(partes) or "sin registros en Catálogo ni GBIF"
    val["Magdalena medio y bajo"] = 1
    fuente["Magdalena medio y bajo"] = "Catálogo" if mag else "Este inventario (Barrancabermeja); no listada en el Catálogo"
    sug = [r for r, ds in DEP_REG.items() if not val[r] and sum(gdep.get(k, 0) for k in ds) >= 3]
    m = g.get("match", {})
    sinon_gbif = m.get("scientificName") if m.get("status") == "SYNONYM" else None
    notas = [x for x in [c.get("nota"), DUDOSAS.get(esp),
                         f"GBIF la trata como sinónimo ({sinon_gbif})" if sinon_gbif else None] if x]
    filas.append({"familia": fam.get(esp), "especie": esp,
                  "nombre_catalogo": c.get("nombre"), **val,
                  "catalogo_encontrado": bool(c.get("encontrado")),
                  "catalogo_identificacion_erronea": bool(c.get("mal_identificado")),
                  "catalogo_regiones": c.get("Región Biogeográfica"),
                  "catalogo_departamentos": c.get("Departamentos"),
                  "catalogo_elevacion": c.get("Elevación"), "catalogo_origen": c.get("Origen"),
                  "catalogo_conservacion": c.get("Estado de conservación"),
                  "catalogo_url": c.get("url"),
                  "gbif_nombre_aceptado": m.get("scientificName"),
                  "gbif_registros_colombia": g.get("co_total"),
                  "gbif_registros_alto_magdalena": alto_g,
                  "gbif_sugiere": "; ".join(sug) or None,
                  "nota_revision": " ".join(notas) or None,
                  "en_hoja_original": esp in old.index})
    for r in REG:
        ov = None
        if esp in old.index:
            col = [k for k, v in VIEJA.items() if v == r]
            if col:
                o = old.loc[esp, col[0]]
                ov = 0 if pd.isna(o) else int(o)
        nv = val[r]
        if esp not in old.index: est = "especie nueva en la tabla"
        elif ov is None: est = "región nueva en el esquema"
        elif ov == nv: est = "confirmado"
        elif nv == 1: est = "agregado"
        else: est = "corregido (quitado)"
        cambios.append({"familia": fam.get(esp), "especie": esp, "region": r,
                        "valor_original": ov, "valor_nuevo": nv, "estado": est,
                        "fuente": fuente[r], "alerta_gbif": r in sug})

tab = pd.DataFrame(filas).sort_values(["familia", "especie"])
tab.to_csv(d / "distribucion_verificada.csv", index=False, encoding="utf-8")
cam = pd.DataFrame(cambios).sort_values(["familia", "especie"], kind="stable")
cam.to_csv(d / "distribucion_cambios.csv", index=False, encoding="utf-8")
print(len(tab), "especies;", cam.estado.value_counts().to_dict())
print("Especies de la hoja original sin consultar:", sorted(set(old.index) - set(tab.especie)))
print("Columna Centroamérica de la hoja original no se usa (fuera del esquema del Catálogo).")
