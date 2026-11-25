"""Convierte manuscrito_caldasia.md a Word con el formato de Caldasia:
Times New Roman 12, doble espacio, carta, márgenes de 25 mm, numeración
de líneas continua y de páginas. Uso: python3 manuscrito/construir_docx.py"""
import subprocess, pathlib
from docx import Document
from docx.oxml import OxmlElement
from docx.oxml.ns import qn

d = pathlib.Path(__file__).resolve().parent
out = d / "manuscrito_caldasia.docx"
subprocess.run(["pandoc", "manuscrito_caldasia.md", "-o", out.name,
                "--reference-doc=referencia_caldasia.docx", "--resource-path=.", "--columns=20"],
               cwd=d, check=True)
doc = Document(out)
for sec in doc.sections:
    ln = OxmlElement("w:lnNumType")
    for k, v in [("w:countBy", "1"), ("w:restart", "continuous"), ("w:distance", "284")]:
        ln.set(qn(k), v)
    sec._sectPr.append(ln)
    p = sec.footer.paragraphs[0]; p.alignment = 1
    r = p.add_run()
    for kind, txt in [("begin", None), (None, "PAGE"), ("end", None)]:
        if kind:
            e = OxmlElement("w:fldChar"); e.set(qn("w:fldCharType"), kind)
        else:
            e = OxmlElement("w:instrText"); e.text = txt
        r._r.append(e)
doc.save(out)
print("Generado:", out)
