"""Consulta el Catálogo de Plantas y Líquenes de Colombia y GBIF para cada especie
del inventario identificada a especie. Guarda las respuestas en
consultas/catalogo.json y consultas/gbif.json.

Uso (desde la raíz del repositorio):
    python3 analisis_articulo/distribucion/01_consultar_fuentes.py

Alto Magdalena en GBIF: registros en Colombia dentro de la caja
lat 1,5-5,2° N (aguas arriba de Honda), lon -75,7 a -74,4° y elevación 0-1000 m,
que corresponde al fondo del valle entre las cordilleras Central y Oriental.
"""
import html, json, pathlib, re, time, urllib.error, urllib.parse, urllib.request
import pandas as pd

D = pathlib.Path(__file__).resolve().parent
OUT = D / "consultas"; OUT.mkdir(exist_ok=True)
UA = {"User-Agent": "Mozilla/5.0 (inventario-floristico)"}
CF = {"Guatteria cf. goudotiana": "Guatteria goudotiana",
      "Pseudomalmea cf. boyacana": "Pseudomalmea boyacana",
      "Pseudoxandra cf. pacifica": "Pseudoxandra pacifica"}


def get(url, tries=4):
    for i in range(tries):
        try:
            req = urllib.request.Request(url, headers=UA)
            with urllib.request.urlopen(req, timeout=40) as r:
                return r.read().decode("utf-8", "replace")
        except urllib.error.HTTPError as e:
            if e.code == 404:
                return ""
            err = e; time.sleep(2 + 3 * i)
        except Exception as e:
            err = e; time.sleep(2 + 3 * i)
    raise err


def texto(s):
    s = re.sub(r"<script.*?</script>|<style.*?</style>", "", s, flags=re.S)
    s = html.unescape(re.sub(r"<[^>]+>", "\n", s))
    return [l.strip() for l in s.splitlines() if l.strip()]


CAMPOS = ["Hábito", "Origen", "Estado de conservación", "Espécimen representativo", "Notas",
          "Región Biogeográfica", "Elevación", "Departamentos", "Sinónimos", "Distribucion Global",
          "Distribución Global", "Correcciones o adiciones"]


def catalogo(nombre):
    url = ("https://catalogoplantasdecolombia.unal.edu.co/es/resultados/especie/"
           + urllib.parse.quote(nombre.lower()) + "/")
    L = texto(get(url))
    out = {"url": url, "encontrado": False}
    try:
        i = L.index("Volver")
    except ValueError:
        return out
    if "Material Mal identificado" in L:
        out.update(encontrado=True, nombre=L[i + 2].strip("[]"), mal_identificado=True,
                   Notas=L[L.index("Notas") + 1] if "Notas" in L else "Material mal identificado")
        return out
    if "Hábito" not in L and "Región Biogeográfica" not in L:
        return out
    out["encontrado"] = True
    out["familia_genero"] = L[i + 1]
    out["nombre"] = L[i + 2]
    out["autor"] = L[i + 3] if L[i + 3] not in CAMPOS and L[i + 3] != "Referencias" else ""
    idx = {c: L.index(c) for c in CAMPOS if c in L}
    orden = sorted(idx.items(), key=lambda kv: kv[1])
    for k, (c, p) in enumerate(orden):
        fin = orden[k + 1][1] if k + 1 < len(orden) else len(L)
        val = " ".join(L[p + 1:fin]).replace(" ,", ",")
        out[c] = re.sub(r"\s+", " ", val).strip(" ,")
    return out


def gbif(nombre):
    q = urllib.parse.quote(nombre)
    m = json.loads(get(f"https://api.gbif.org/v1/species/match?name={q}&kingdom=Plantae"))
    key = m.get("acceptedUsageKey") or m.get("usageKey")
    out = {"match": {k: m.get(k) for k in ("usageKey", "acceptedUsageKey", "scientificName", "status",
                                           "matchType", "confidence", "family")}}
    if not key:
        return out
    base = f"https://api.gbif.org/v1/occurrence/search?taxonKey={key}&country=CO&limit=0"
    r = json.loads(get(base + "&facet=stateProvince&facetLimit=60"))
    out["co_total"] = r["count"]
    out["departamentos"] = {c["name"]: c["count"] for f in r.get("facets", []) for c in f["counts"]}
    r = json.loads(get(base + "&decimalLatitude=1.5,5.2&decimalLongitude=-75.7,-74.4&elevation=0,1000"))
    out["alto_magdalena"] = r["count"]
    r = json.loads(get(base.replace("&country=CO", "") + "&facet=country&facetLimit=60"))
    out["paises"] = {c["name"]: c["count"] for f in r.get("facets", []) for c in f["counts"]}
    return out


inv = pd.read_csv(D.parent / "datos" / "datos_limpios.csv")
sp = sorted(s for s in inv.especie.unique()
            if not re.search(r" sp\.|Indet|aceae|aff\.|\(", s))
cat_f, gb_f = OUT / "catalogo.json", OUT / "gbif.json"
cat = json.loads(cat_f.read_text()) if cat_f.exists() else {}
gb = json.loads(gb_f.read_text()) if gb_f.exists() else {}
for s in sp:
    q = CF.get(s, s)
    if s not in cat:
        cat[s] = catalogo(q); time.sleep(0.5)
        if not cat[s]["encontrado"] and q == "Euterpe longevaginata":
            cat[s] = catalogo("Euterpe precatoria var. longevaginata")
            cat[s]["nota"] = "Consultado como Euterpe precatoria var. longevaginata"
    if s not in gb:
        gb[s] = gbif(q); time.sleep(0.3)
    print(s, cat[s].get("Región Biogeográfica"), gb[s].get("alto_magdalena"), flush=True)
    cat_f.write_text(json.dumps(cat, ensure_ascii=False, indent=1))
    gb_f.write_text(json.dumps(gb, ensure_ascii=False, indent=1))
print("Listo:", len(cat), "especies")
