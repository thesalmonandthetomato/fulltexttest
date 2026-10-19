"""Recover three previously uploaded private Zenodo PDFs without new searches."""
import os,io,zipfile,pathlib,requests,json
ids=(23468287,23468446,23468630)
s=requests.Session();s.headers["Authorization"]="Bearer "+os.environ["ZENODO_TOKEN"]
out=pathlib.Path("outputs/restore_archived_pdfs");out.mkdir(parents=True,exist_ok=True)
summary=[]
for ident in ids:
 r=s.get(f"https://zenodo.org/api/deposit/depositions/{ident}",timeout=30);r.raise_for_status()
 files=r.json()["files"];found=[]
 for entry in files:
  name=entry["filename"]
  if not name.endswith((".zip",".pdf")):continue
  u=entry.get("links",{}).get("download")
  if not u:continue
  f=s.get(u,timeout=120);f.raise_for_status()
  if name.endswith(".pdf"):
   if f.content.startswith(b"%PDF"):
    (out/name).write_bytes(f.content);found.append(name)
  else:
   with zipfile.ZipFile(io.BytesIO(f.content)) as z:
    for member in z.namelist():
     if member.lower().endswith(".pdf"):
      content=z.read(member)
      if content.startswith(b"%PDF"):
       name=pathlib.Path(member).name
       (out/name).write_bytes(content);found.append(name)
 print("RECOVERED",ident,found,flush=True)
 summary.append(dict(deposit=ident,files=found))
(out/"restore_summary.json").write_text(json.dumps(summary,indent=2))
assert len(list(out.glob("*.pdf")))==3,"Expected precisely three PDF files"
